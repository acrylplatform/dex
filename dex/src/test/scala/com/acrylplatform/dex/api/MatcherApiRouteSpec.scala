package com.acrylplatform.dex.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.testkit.{TestActor, TestProbe}
import com.google.common.primitives.Longs
import com.typesafe.config.ConfigFactory
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.Base58
import com.acrylplatform.dex._
import com.acrylplatform.dex.error.ErrorFormatterContext
import com.acrylplatform.dex.settings.MatcherSettings
import com.acrylplatform.http.ApiMarshallers._
import com.acrylplatform.http.RouteSpec
import com.acrylplatform.state.Blockchain
import com.acrylplatform.transaction.Asset
import com.acrylplatform.{RequestGen, WithDB, crypto}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsString, JsValue}

import scala.concurrent.Future

class MatcherApiRouteSpec extends RouteSpec("/matcher") with RequestGen with PathMockFactory with Eventually with WithDB {

  private val settings                       = MatcherSettings.valueReader.read(ConfigFactory.load(), "acryl.dex")
  private val matcherKeyPair                 = KeyPair("matcher".getBytes("utf-8"))
  private def getAssetDecimals(asset: Asset) = 8

  routePath("/balance/reserved/{publicKey}") - {
    val publicKey = matcherKeyPair.publicKey
    val ts        = System.currentTimeMillis()
    val signature = crypto.sign(matcherKeyPair, publicKey ++ Longs.toByteArray(ts))

    def mkGet(route: Route)(base58PublicKey: String, ts: Long, base58Signature: String): RouteTestResult =
      Get(routePath(s"/balance/reserved/$base58PublicKey")).withHeaders(
        RawHeader("Timestamp", s"$ts"),
        RawHeader("Signature", base58Signature)
      ) ~> route

    "returns a reserved balance for specified publicKey" in test { route =>
      mkGet(route)(Base58.encode(publicKey), ts, Base58.encode(signature)) ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    "returns HTTP 400 when provided a wrong base58-encoded" - {
      "signature" in test { route =>
        mkGet(route)(Base58.encode(publicKey), ts, ";;") ~> check {
          status shouldBe StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The request has an invalid signature"
        }
      }

      "public key" in test { route =>
        mkGet(route)(";;", ts, Base58.encode(signature)) ~> check {
          handled shouldBe false
        }
      }
    }
  }

  private def test[U](f: Route => U): U = {
    val blockchain   = stub[Blockchain]
    val addressActor = TestProbe("address")
    addressActor.setAutoPilot { (sender: ActorRef, msg: Any) =>
      msg match {
        case AddressDirectory.Envelope(_, AddressActor.GetReservedBalance) => sender ! Map.empty[Asset, Long]
        case _                                                             =>
      }

      TestActor.NoAutoPilot
    }

    implicit val context: ErrorFormatterContext = (_: Asset) => 8

    val route = MatcherApiRoute(
      assetPairBuilder = new AssetPairBuilder(settings, blockchain, Set.empty),
      matcherPublicKey = matcherKeyPair.publicKey,
      matcher = ActorRef.noSender,
      addressActor = addressActor.ref,
      storeEvent = _ => Future.failed(new NotImplementedError("Storing is not implemented")),
      orderBook = _ => None,
      getMarketStatus = _ => None,
      tickSize = _ => 0.1,
      orderValidator = _ => Left(error.FeatureNotImplemented),
      orderBookSnapshot = new OrderBookSnapshotHttpCache(settings.orderBookSnapshotHttpCache, ntpTime, getAssetDecimals, _ => None),
      matcherSettings = settings,
      matcherStatus = () => Matcher.Status.Working,
      db = db,
      time = ntpTime,
      currentOffset = () => 0L,
      lastOffset = () => Future.successful(0L),
      matcherAccountFee = 300000L,
      apiKeyHash = None,
      rateCache = RateCache.inMem,
      validatedAllowedOrderVersions = Set(1, 2, 3)
    ).route

    f(route)
  }

}
