package com.acrylplatform.dex

import java.io.File
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{AskTimeoutException, ask, gracefulStop}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import cats.data.NonEmptyList
import com.acrylplatform.account.{Address, PublicKey}
import com.acrylplatform.api.http.{ApiRoute, CompositeHttpService => _}
import com.acrylplatform.common.utils.{Base58, EitherExt2}
import com.acrylplatform.db._
import com.acrylplatform.dex.Matcher.Status
import com.acrylplatform.dex.api.http.CompositeHttpService
import com.acrylplatform.dex.api.{MatcherApiRoute, MatcherApiRouteV1, OrderBookSnapshotHttpCache}
import com.acrylplatform.dex.db.{AssetPairsDB, OrderBookSnapshotDB, OrderDB}
import com.acrylplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.acrylplatform.dex.history.HistoryRouter
import com.acrylplatform.dex.market.OrderBookActor.MarketStatus
import com.acrylplatform.dex.market._
import com.acrylplatform.dex.model.MatcherModel.{Denormalization, Normalization}
import com.acrylplatform.dex.model._
import com.acrylplatform.dex.queue._
import com.acrylplatform.dex.settings.{MatcherSettings, MatchingRules, RawMatchingRules}
import com.acrylplatform.extensions.{Context, Extension}
import com.acrylplatform.state.VolumeAndFee
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order}
import com.acrylplatform.utils.{ErrorStartingMatcher, ScorexLogging, forceStopApplication}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(context: Context) extends Extension with ScorexLogging {

  private val settings = context.settings.config.as[MatcherSettings]("acryl.dex")

  private val matcherKeyPair = (for {
    address <- Address.fromString(settings.account)
    pk      <- context.wallet.privateKeyAccount(address)
  } yield pk).explicitGet()

  private def matcherPublicKey: PublicKey = matcherKeyPair

  private implicit val as: ActorSystem                 = context.actorSystem
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  import as.dispatcher

  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)

  private val blacklistedAssets: Set[IssuedAsset] = settings.blacklistedAssets
    .map { assetId =>
      AssetPair.extractAssetId(assetId) match {
        case Success(Acryl)          => throw new IllegalArgumentException("Can't blacklist the main coin")
        case Success(a: IssuedAsset) => a
        case Failure(e)              => throw new IllegalArgumentException("Can't parse asset id", e)
      }
    }

  private val pairBuilder        = new AssetPairBuilder(settings, context.blockchain, blacklistedAssets)
  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)
  private val transactionCreator = new ExchangeTransactionCreator(context.blockchain, matcherKeyPair, settings)

  private val orderBooks         = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val rawMatchingRules   = new ConcurrentHashMap[AssetPair, RawMatchingRules]
  private val assetDecimalsCache = new AssetDecimalsCache(context.blockchain)

  private val orderBooksSnapshotCache =
    new OrderBookSnapshotHttpCache(
      settings.orderBookSnapshotHttpCache,
      context.time,
      assetDecimalsCache.get,
      p => Option(orderBookCache.get(p))
    )

  private val marketStatuses = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def normalize(assetPair: AssetPair)(rawMatchingRules: RawMatchingRules): MatchingRules =
    MatchingRules(
      rawMatchingRules.startOffset,
      normalizedTickSize = Normalization.normalizePrice(
        rawMatchingRules.tickSize,
        assetPair,
        MatcherModel.getPairDecimals(assetPair, context.blockchain).getOrElse((8, 8))
      )
    )

  private def denormalizeTickSize(assetPair: AssetPair, normalizedTickSize: Long): Either[MatcherError, Double] =
    Denormalization.denormalizePrice(normalizedTickSize, assetPair, context.blockchain)

  private def convert(assetPair: AssetPair, matchingRules: MatchingRules): RawMatchingRules =
    RawMatchingRules(
      startOffset = matchingRules.startOffset,
      tickSize = denormalizeTickSize(assetPair, matchingRules.normalizedTickSize).left.map { e =>
        log.error(s"Can't convert matching rules for $assetPair: ${e.mkMessage(errorContext).text}. Usually this happens when the blockchain was rolled back.")
        0.00000001
      }.merge
    )

  private def matchingRules(assetPair: AssetPair): NonEmptyList[RawMatchingRules] = {
    lazy val default = convert(assetPair, MatchingRules.Default)
    val xs           = settings.matchingRules.getOrElse(assetPair, NonEmptyList.one[RawMatchingRules](default))
    if (xs.head.startOffset == 0) xs else default :: xs
  }

  private def orderBookProps(assetPair: AssetPair, matcherActor: ActorRef): Props =
    OrderBookActor.props(
      matcherActor,
      addressActors,
      orderBookSnapshotStore,
      assetPair,
      updateOrderBookCache(assetPair),
      marketStatuses.put(assetPair, _),
      newMatchingRules => rawMatchingRules.put(assetPair, newMatchingRules),
      normalize(assetPair),
      settings,
      transactionCreator.createTransaction,
      context.time,
      matchingRules(assetPair)
    )

  private val matcherQueue: MatcherQueue = settings.eventsQueue.tpe match {
    case "local" =>
      log.info("Events will be stored locally")
      new LocalMatcherQueue(settings.eventsQueue.local, new LocalQueueStore(db), context.time)

    case "kafka" =>
      log.info("Events will be stored in Kafka")
      new KafkaMatcherQueue(settings.eventsQueue.kafka)(materializer)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))
  private val rateCache                                          = RateCache(db)

  private def validateOrder(o: Order) =
    for {
      _ <- OrderValidator.matcherSettingsAware(matcherPublicKey, blacklistedAddresses, blacklistedAssets, settings, rateCache)(o)
      _ <- OrderValidator.timeAware(context.time)(o)
      _ <- OrderValidator.marketAware(settings.orderFee, settings.deviation, getMarketStatus(o.assetPair), rateCache)(o)
      _ <- OrderValidator.blockchainAware(
        context.blockchain,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        context.time,
        settings.orderFee,
        settings.orderRestrictions,
        rateCache
      )(o)
      _ <- pairBuilder.validateAssetPair(o.assetPair)
    } yield o

  private implicit val errorContext = new ErrorFormatterContext {
    override def assetDecimals(asset: Asset): Int = assetDecimalsCache.get(asset)
  }

  lazy val matcherApiRoutes: Seq[ApiRoute] = {
    val keyHash = Base58.tryDecode(context.settings.config.getString("acryl.rest-api.api-key-hash")).toOption

    Seq(
      MatcherApiRoute(
        pairBuilder,
        matcherPublicKey,
        matcher,
        addressActors,
        matcherQueue.storeEvent,
        p => Option(orderBooks.get()).flatMap(_.get(p)),
        p => Option(marketStatuses.get(p)),
        assetPair => {
          lazy val default = RawMatchingRules(
            startOffset = 0,
            tickSize = denormalizeTickSize(assetPair, 1).left.map(_ => 0.00000001).merge
          )

          rawMatchingRules.computeIfAbsent(assetPair, _ => default).tickSize
        },
        validateOrder,
        orderBooksSnapshotCache,
        settings,
        () => status.get(),
        db,
        context.time,
        () => matcherQueue.lastProcessedOffset,
        () => matcherQueue.lastEventOffset,
        ExchangeTransactionCreator.minAccountFee(context.blockchain, matcherPublicKey.toAddress),
        keyHash,
        rateCache,
        settings.allowedOrderVersions.filter(OrderValidator.checkOrderVersion(_, context.blockchain).isRight)
      ),
      MatcherApiRouteV1(
        pairBuilder,
        orderBooksSnapshotCache,
        () => status.get(),
        keyHash
      )
    )
  }

  lazy val matcherApiTypes: Set[Class[_]] =
    Set(
      classOf[MatcherApiRoute],
      classOf[MatcherApiRouteV1],
    )

  private val snapshotsRestore = Promise[Unit]()

  private lazy val assetPairsDb = AssetPairsDB(db)

  private lazy val orderBookSnapshotDB = OrderBookSnapshotDB(db)

  lazy val orderBookSnapshotStore: ActorRef = context.actorSystem.actorOf(
    OrderBookSnapshotStoreActor.props(orderBookSnapshotDB),
    "order-book-snapshot-store"
  )

  private val pongTimeout = new Timeout(settings.processConsumedTimeout * 2)

  lazy val matcher: ActorRef = context.actorSystem.actorOf(
    MatcherActor.props(
      settings,
      assetPairsDb, {
        case Left(msg) =>
          log.error(s"Can't start matcher: $msg")
          forceStopApplication(ErrorStartingMatcher)

        case Right((self, processedOffset)) =>
          snapshotsRestore.trySuccess(())
          matcherQueue.startConsume(
            processedOffset + 1,
            xs => {
              if (xs.isEmpty) Future.successful(())
              else {
                val assetPairs: Set[AssetPair] = xs.map { eventWithMeta =>
                  log.debug(s"Consumed $eventWithMeta")

                  self ! eventWithMeta
                  eventWithMeta.event.assetPair
                }(collection.breakOut)

                self
                  .ask(MatcherActor.PingAll(assetPairs))(pongTimeout)
                  .recover {
                    case NonFatal(e) => log.error("PingAll is timed out!", e)
                  }
                  .map(_ => ())
              }
            }
          )
      },
      orderBooks,
      orderBookProps,
      context.blockchain.assetDescription
    ),
    MatcherActor.name
  )

  private lazy val orderDb = OrderDB(settings, db)

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    context.actorSystem.actorOf(HistoryRouter.props(context.blockchain, settings.postgresConnection, orderHistorySettings), "history-router")
  }

  private lazy val addressActors =
    context.actorSystem.actorOf(
      Props(
        new AddressDirectory(
          context.spendableBalanceChanged,
          settings,
          (address, startSchedules) =>
            Props(new AddressActor(
              address,
              context.utx.spendableBalance(address, _),
              5.seconds,
              context.time,
              orderDb,
              id => context.blockchain.filledVolumeAndFee(id) != VolumeAndFee.empty,
              matcherQueue.storeEvent,
              startSchedules
            )),
          historyRouter
        )),
      "addresses"
    )

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private lazy val db = openDB(settings.dataDir)

  @volatile var matcherServerBinding: ServerBinding = _

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down matcher")
    setStatus(Status.Stopping)

    Await.result(matcherServerBinding.unbind(), 10.seconds)

    val stopMatcherTimeout = 5.minutes
    matcherQueue.close(stopMatcherTimeout)

    orderBooksSnapshotCache.close()
    Await.result(gracefulStop(matcher, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
    materializer.shutdown()
    log.debug("Matcher's actor system has been shut down")
    db.close()
    log.debug("Matcher's database closed")
    log.info("Matcher shutdown successful")
  }

  private def checkDirectory(directory: File): Unit = if (!directory.exists()) {
    log.error(s"Failed to create directory '${directory.getPath}'")
    sys.exit(1)
  }

  override def start(): Unit = {
    val journalDir  = new File(settings.journalDataDir)
    val snapshotDir = new File(settings.snapshotsDataDir)
    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${settings.bindAddress}:${settings.port} ...")

    val combinedRoute = new CompositeHttpService(matcherApiTypes, matcherApiRoutes, context.settings.restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, settings.bindAddress, settings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")
    context.actorSystem.actorOf(
      ExchangeTransactionBroadcastActor
        .props(
          settings.exchangeTransactionBroadcast,
          context.time,
          tx => context.utx.putIfNew(tx).resultE.isRight,
          context.blockchain.containsTransaction(_),
          txs => txs.foreach(context.broadcastTx)
        ),
      "exchange-transaction-broadcast"
    )

    context.actorSystem.actorOf(MatcherTransactionWriter.props(db, settings), MatcherTransactionWriter.name)

    val startGuard = for {
      _ <- waitSnapshotsRestored(settings.snapshotsLoadingTimeout)
      deadline = settings.startEventsProcessingTimeout.fromNow
      lastOffsetQueue <- getLastOffset(deadline)
      _ = log.info(s"Last queue offset is $lastOffsetQueue")
      _ <- waitOffsetReached(lastOffsetQueue, deadline)
      _ = log.info("Last offset has been reached, notify addresses")
    } yield addressActors ! AddressDirectory.StartSchedules

    startGuard.onComplete {
      case Success(_) => setStatus(Status.Working)
      case Failure(e) =>
        log.error(s"Can't start matcher: ${e.getMessage}", e)
        forceStopApplication(ErrorStartingMatcher)
    }
  }

  private def setStatus(newStatus: Status): Unit = {
    status.set(newStatus)
    log.info(s"Status now is $newStatus")
  }

  private def waitSnapshotsRestored(timeout: FiniteDuration): Future[Unit] = {
    val failure = Promise[Unit]()
    context.actorSystem.scheduler.scheduleOnce(timeout) {
      failure.failure(new TimeoutException(s"Can't restore snapshots in ${timeout.toSeconds} seconds"))
    }

    Future.firstCompletedOf[Unit](List(snapshotsRestore.future, failure.future))
  }

  private def getLastOffset(deadline: Deadline): Future[QueueEventWithMeta.Offset] = matcherQueue.lastEventOffset.recoverWith {
    case _: AskTimeoutException =>
      if (deadline.isOverdue()) Future.failed(new TimeoutException("Can't get last offset from queue"))
      else getLastOffset(deadline)
  }

  private def waitOffsetReached(lastQueueOffset: QueueEventWithMeta.Offset, deadline: Deadline): Future[Unit] = {
    def loop(p: Promise[Unit]): Unit = {
      val currentOffset = matcherQueue.lastProcessedOffset
      log.trace(s"offsets: $currentOffset >= $lastQueueOffset, deadline: ${deadline.isOverdue()}")
      if (currentOffset >= lastQueueOffset) p.success(())
      else if (deadline.isOverdue())
        p.failure(new TimeoutException(s"Can't process all events in ${settings.startEventsProcessingTimeout.toMinutes} minutes"))
      else context.actorSystem.scheduler.scheduleOnce(5.second)(loop(p))
    }

    val p = Promise[Unit]()
    loop(p)
    p.future
  }
}

object Matcher extends ScorexLogging {
  type StoreEvent = QueueEvent => Future[Option[QueueEventWithMeta]]

  sealed trait Status
  object Status {
    case object Starting extends Status
    case object Working  extends Status
    case object Stopping extends Status
  }
}
