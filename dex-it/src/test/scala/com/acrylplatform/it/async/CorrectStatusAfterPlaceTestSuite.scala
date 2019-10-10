package com.acrylplatform.it.async

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it._
import com.acrylplatform.it.api.AsyncMatcherHttpApi._
import com.acrylplatform.it.api.UnexpectedStatusCodeException
import com.acrylplatform.it.async.CorrectStatusAfterPlaceTestSuite._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.IssueTransactionV1
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.acrylplatform.transaction.transfer.MassTransferTransaction

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class CorrectStatusAfterPlaceTestSuite extends MatcherSuiteBase {

  private val matcherConfig = ConfigFactory.parseString(
    s"""acryl {
       |  dex {
       |    price-assets = ["${Asset1.id()}", "${Asset2.id()}"]
       |    rest-order-limit = 100
       |    events-queue {
       |      local {
       |        polling-interval = 1s
       |        max-elements-per-poll = 100
       |      }
       |
       |      kafka.consumer {
       |        buffer-size = 100
       |      }
       |    }
       |  }
       |}
       |
       |akka.kafka.consumer.poll-interval = 1s""".stripMargin
  )

  private val pairs = Seq(
    AssetPair(Acryl, IssuedAsset(Asset1.id())),
    AssetPair(Acryl, IssuedAsset(Asset2.id())),
    AssetPair(IssuedAsset(Asset2.id()), IssuedAsset(Asset1.id())),
  )

  override protected val nodeConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback)

  private val traders: Seq[KeyPair] = (1 to 10).map(_ => KeyPair(Random.nextString(20).getBytes))

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val startTs    = System.currentTimeMillis()
    val sendAmount = Long.MaxValue / (traders.size + 1)
    val issueAndDistribute = for {
      // distribute acryl
      transferAcrylTx <- {
        val transferTx = MassTransferTransaction
          .selfSigned(
            sender = bob,
            assetId = Acryl,
            transfers = traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, 100.acryl)).toList,
            timestamp = startTs,
            feeAmount = 0.006.acryl,
            attachment = Array.emptyByteArray
          )
          .explicitGet()

        node.broadcastRequest(transferTx.json())
      }

      // issue
      issueTxs <- Future.traverse(Assets)(asset => node.broadcastRequest(asset.json()))
      _        <- Future.traverse(issueTxs)(tx => node.waitForTransaction(tx.id))

      // distribute assets
      transferAssetsTxs <- Future.sequence {
        Assets.map { issueTx =>
          val transferTx = MassTransferTransaction
            .selfSigned(
              sender = Issuer,
              assetId = IssuedAsset(issueTx.id()),
              transfers = traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, sendAmount)).toList,
              timestamp = startTs,
              feeAmount = 0.006.acryl,
              attachment = Array.emptyByteArray
            )
            .explicitGet()

          node.broadcastRequest(transferTx.json())
        }
      }

      _ <- node.waitForTransaction(transferAcrylTx.id)
      _ <- Future.traverse(transferAssetsTxs)(tx => node.waitForTransaction(tx.id))
    } yield ()

    Await.result(issueAndDistribute, 5.minute)
  }

  "place orders and check their statuses" in {
    val ts = System.currentTimeMillis()

    val orders = for {
      account <- traders
      pair    <- pairs
      i       <- 1 to 60
    } yield node.prepareOrder(account, pair, OrderType.SELL, 100000L, 10000L, 0.003.acryl, 1, timestamp = ts + i)

    val r = Await.result(Future.traverse(orders.grouped(orders.size / 5))(requests), 5.minutes).flatten
    r.foreach {
      case (id, status) => withClue(id)(status should not be "NotFound")
    }
  }

  private def request(order: Order): Future[(String, String)] =
    for {
      _ <- node.placeOrder(order).recover {
        case e: UnexpectedStatusCodeException if e.statusCode == 503 || e.responseBody.contains("has already been placed") => // Acceptable
      }
      status <- node.orderStatus(order.idStr(), order.assetPair, waitForStatus = false)
    } yield (order.idStr(), status.status)

  private def requests(orders: Seq[Order]): Future[Seq[(String, String)]] = Future.traverse(orders)(request)
}

object CorrectStatusAfterPlaceTestSuite {
  private val Issuer = alice

  private val Asset1 = IssueTransactionV1
    .selfSigned(
      sender = Issuer,
      name = "asset1".getBytes,
      description = Array.emptyByteArray,
      quantity = Long.MaxValue,
      decimals = 0,
      reissuable = false,
      fee = 1.acryl,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val Asset2 = IssueTransactionV1
    .selfSigned(
      sender = Issuer,
      name = "asset2".getBytes,
      description = Array.emptyByteArray,
      quantity = Long.MaxValue,
      decimals = 0,
      reissuable = false,
      fee = 1.acryl,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val Assets = List(Asset1, Asset2)
}
