package com.acrylplatform.dex.queue

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.kafka._
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, RestartSource, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import com.acrylplatform.dex.queue.KafkaMatcherQueue.{KafkaProducer, Settings, eventDeserializer}
import com.acrylplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.acrylplatform.dex.queue.QueueEventWithMeta.Offset
import com.acrylplatform.utils.ScorexLogging
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContextExecutor, Future, Promise}

class KafkaMatcherQueue(settings: Settings)(implicit mat: ActorMaterializer) extends MatcherQueue with ScorexLogging {
  private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

  private val duringShutdown = new AtomicBoolean(false)

  private val producer: Producer = {
    val r = if (settings.producer.enable) new KafkaProducer(settings, duringShutdown.get()) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  private val consumerControl = new AtomicReference[Consumer.Control](Consumer.NoopControl)
  private val consumerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.consumer")
    ConsumerSettings(config, new ByteArrayDeserializer, eventDeserializer).withClientId("consumer")
  }

  private val metadataConsumer = mat.system.actorOf(
    KafkaConsumerActor.props(consumerSettings.withClientId("meta-consumer")),
    "meta-consumer"
  )

  @volatile private var lastProcessedOffsetInternal = -1L

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: Seq[QueueEventWithMeta] => Future[Unit]): Unit = {
    val topicPartition = new TopicPartition(settings.topic, 0)

    RestartSource
      .onFailuresWithBackoff(
        minBackoff = settings.consumer.minBackoff,
        maxBackoff = settings.consumer.maxBackoff,
        randomFactor = 0.2,
        maxRestarts = -1
      ) { () =>
        val startOffset = math.max(lastProcessedOffsetInternal, fromOffset)
        log.info(s"Start consuming from $startOffset")
        Consumer
          .plainSource(consumerSettings, Subscriptions.assignmentWithOffset(topicPartition -> startOffset))
          .mapMaterializedValue(consumerControl.set)
          .groupedWithin(settings.consumer.bufferSize, 10.millis)
          .mapAsyncUnordered(1) { messages =>
            // We can do it in parallel, because we're just applying verified events
            // Messages are received one-by-one, e.g. offsets: 1, 2, 3, ...
            var lastOffset = lastProcessedOffsetInternal
            val xs = messages.map { msg =>
              val req = QueueEventWithMeta(msg.offset(), msg.timestamp(), msg.value())
              lastOffset = req.offset
              req
            }
            process(xs).andThen { case _ => lastProcessedOffsetInternal = lastOffset }
          }
      }
      .runWith(Sink.ignore)
  }

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastProcessedOffset: Offset = lastProcessedOffsetInternal

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] = {
    implicit val timeout: Timeout = Timeout(consumerSettings.metadataRequestTimeout)

    // We need to list topic first, see the EndOffsets documentation
    (metadataConsumer ? Metadata.ListTopics).mapTo[Metadata.Topics].flatMap { topics =>
      topics.response.getOrElse(Map.empty).get(settings.topic) match {
        case None => Future.successful(-1L)
        case Some(partitions) =>
          if (partitions.size != 1) throw new IllegalStateException(s"DEX can work only with one partition, given: $partitions")
          val topicPartition = new TopicPartition(settings.topic, partitions.head.partition())
          (metadataConsumer ? Metadata.GetEndOffsets(Set(topicPartition)))
            .mapTo[Metadata.EndOffsets]
            .map { r =>
              // -1 because by contract lastEventOffset must return -1 if there is no topic or it is empty
              // also see https://github.com/apache/kafka/blob/trunk/clients/src/main/java/org/apache/kafka/clients/consumer/KafkaConsumer.java#L2025
              r.response
                .getOrElse(Map.empty)
                .getOrElse(topicPartition, throw new IllegalStateException(s"Unexpected behaviour, no info for $topicPartition: $r")) - 1
            }
      }
    }
  }

  override def close(timeout: FiniteDuration): Unit = {
    duringShutdown.set(true)
    mat.system.stop(metadataConsumer)
    val stoppingConsumer = consumerControl.get().shutdown()
    Await.result(stoppingConsumer, timeout)
  }

}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumer: ConsumerSettings, producer: ProducerSettings)
  case class ConsumerSettings(bufferSize: Int, minBackoff: FiniteDuration, maxBackoff: FiniteDuration)
  case class ProducerSettings(enable: Boolean, bufferSize: Int)

  val eventDeserializer: Deserializer[QueueEvent] = new Deserializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def deserialize(topic: String, data: Array[Byte]): QueueEvent          = QueueEvent.fromBytes(data)
    override def close(): Unit                                                      = {}
  }

  val eventSerializer: Serializer[QueueEvent] = new Serializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def serialize(topic: String, data: QueueEvent): Array[Byte]            = QueueEvent.toBytes(data)
    override def close(): Unit                                                      = {}
  }

  private class KafkaProducer(settings: Settings, duringShutdown: => Boolean)(implicit mat: ActorMaterializer) extends Producer {
    private type InternalProducer = SourceQueueWithComplete[(QueueEvent, Promise[QueueEventWithMeta])]

    private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

    private val producerSettings = {
      val config = mat.system.settings.config.getConfig("akka.kafka.producer")
      akka.kafka.ProducerSettings(config, new ByteArraySerializer, eventSerializer)
    }

    private var internal = newInternal
    watch()

    private def newInternal: InternalProducer =
      Source
        .queue[(QueueEvent, Promise[QueueEventWithMeta])](settings.producer.bufferSize, OverflowStrategy.backpressure)
        .map {
          case (payload, p) =>
            ProducerMessage.single(new ProducerRecord[Array[Byte], QueueEvent](settings.topic, payload.assetPair.bytes, payload), passThrough = p)
        }
        .via(Producer.flexiFlow(producerSettings))
        .map {
          case ProducerMessage.Result(meta, ProducerMessage.Message(msg, passThrough)) =>
            passThrough.success(QueueEventWithMeta(meta.offset(), meta.timestamp(), msg.value()))
          case ProducerMessage.MultiResult(parts, passThrough) => throw new RuntimeException(s"MultiResult(parts=$parts, passThrough=$passThrough)")
          case ProducerMessage.PassThroughResult(passThrough)  => throw new RuntimeException(s"PassThroughResult(passThrough=$passThrough)")
        }
        .toMat(Sink.ignore)(Keep.left)
        .run()

    private def watch(): Unit = internal.watchCompletion().onComplete { _ =>
      if (!duringShutdown) {
        internal = newInternal
        watch()
      }
    }

    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = {
      val p = Promise[QueueEventWithMeta]()
      internal.offer((event, p))
      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit = {
      internal.complete()
      Await.result(internal.watchCompletion(), timeout)
    }
  }
}
