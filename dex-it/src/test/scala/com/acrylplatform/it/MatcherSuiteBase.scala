package com.acrylplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.it.MatcherSuiteBase.baseConfig
import com.acrylplatform.it.sync.config.MatcherPriceAssetConfig
import com.acrylplatform.it.transactions.NodesFromDocker
import com.acrylplatform.it.util._
import org.scalatest._

import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val smartFee         = 0.004.acryl
  val minFee           = 0.001.acryl + smartFee
  val issueFee         = 1.acryl
  val smartIssueFee    = 1.acryl + smartFee
  val leasingFee       = 0.002.acryl + smartFee
  val tradeFee         = 0.003.acryl
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected override def createDocker: Docker = new Docker(
    imageName = "com.acrylplatform/dex-it:latest",
    tag = getClass.getSimpleName,
    suiteConfig = baseConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
  )

  protected def node = dockerNodes().head

  protected def nodeConfigs: Seq[Config] = MatcherPriceAssetConfig.Configs

}

object MatcherSuiteBase {
  private def baseConfig(seed: Int): Config = Option(System.getenv("KAFKA_SERVER")).fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""
         |acryl.dex.events-queue {
         |  type = kafka
         |  kafka {
         |    servers = "$kafkaServer"
         |    topic = "dex-$seed"
         |  }
         |}""".stripMargin)
  }
}
