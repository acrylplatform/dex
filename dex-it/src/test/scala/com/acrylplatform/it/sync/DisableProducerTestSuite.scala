package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.it.MatcherSuiteBase
import com.acrylplatform.it.api.SyncHttpApi.{sync => _, _}
import com.acrylplatform.it.api.SyncMatcherHttpApi._
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class DisableProducerTestSuite extends MatcherSuiteBase {
  private def matcherConfig = ConfigFactory.parseString(s"""acryl.dex.events-queue {
       |  local.enable-storing  = no
       |  kafka.producer.enable = no
       |}""".stripMargin)

  override protected def nodeConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback)
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte

  "check no events are written to queue" - {
    // Alice issues new asset
    val aliceAsset = node.signedBroadcast(IssueEthTx.json()).id
    node.waitForTransaction(aliceAsset)
    node.waitForHeight(node.height + 1)

    val aliceAcrylPair = AssetPair(IssuedAsset(IssueEthTx.id()), Acryl)
    // check assets's balances
    node.assertAssetBalance(alice.address, aliceAsset, IssueEthTx.quantity)
    node.assertAssetBalance(matcher.address, aliceAsset, 0)

    "place an order and wait some time" in {
      // Alice places sell order
      val order1 =
        node.prepareOrder(alice, aliceAcrylPair, OrderType.SELL, 500, 2.acryl * Order.PriceConstant, matcherFee, orderVersion, 20.days)

      node
        .expectIncorrectOrderPlacement(
          order1,
          expectedStatusCode = 501,
          expectedStatus = "NotImplemented",
          expectedMessage = Some("This feature is disabled, contact with the administrator")
        )

      // Alice places buy order
      val order2 =
        node.prepareOrder(alice, aliceAcrylPair, OrderType.BUY, 500, 2.acryl * Order.PriceConstant, matcherFee, orderVersion, 21.days)

      node
        .expectIncorrectOrderPlacement(
          order2,
          expectedStatusCode = 501,
          expectedStatus = "NotImplemented",
          expectedMessage = Some("This feature is disabled, contact with the administrator")
        )

      Thread.sleep(5000)
      node.getCurrentOffset should be(-1)
      node.getLastOffset should be(-1)

      docker.killAndStartContainer(dockerNodes().head)

      node.getCurrentOffset should be(-1)
      node.getLastOffset should be(-1)
    }
  }
}
