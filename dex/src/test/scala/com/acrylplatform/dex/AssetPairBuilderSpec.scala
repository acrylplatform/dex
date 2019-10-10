package com.acrylplatform.dex

import com.google.common.base.Charsets
import com.typesafe.config.ConfigFactory
import com.acrylplatform.account.PublicKey
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.settings.MatcherSettings
import com.acrylplatform.settings.loadConfig
import com.acrylplatform.state.diffs.produce
import com.acrylplatform.state.{AssetDescription, Blockchain}
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class AssetPairBuilderSpec extends FreeSpec with Matchers with MockFactory {
  import AssetPairBuilderSpec._

  private def b(v: String) = ByteStr.decodeBase58(v).get

  private val ACRYL  = "ACRYL"
  private val WUSD   = IssuedAsset(ByteStr.decodeBase58("HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj").get)
  private val WBTC   = IssuedAsset(ByteStr.decodeBase58("Fmg13HEHJHuZYbtJq8Da8wifJENq8uBxDuWoP9pVe2Qe").get)
  private val WEUR   = IssuedAsset(ByteStr.decodeBase58("2xnE3EdpqXtFgCP156qt1AbyjpqdZ5jGjWo3CwTawcux").get)
  private val WCNY   = IssuedAsset(ByteStr.decodeBase58("6pmDivReTLikwYqQtJTv6dTcE59knriaodB3AK8T9cF8").get)
  private val Asset1 = mkAssetId(1)
  private val Asset2 = mkAssetId(2)
  private val Asset3 = mkAssetId(3)

  private val predefinedPriceAssets =
    Seq(
      WBTC,
      WUSD,
      WEUR,
      WCNY,
      IssuedAsset(b("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS")),
    )

  private val blacklistedAssets = Set(Asset3)
  private val priceAssets       = ConfigFactory.parseString(s"""acryl.dex {
       |  blacklisted-assets  = [${blacklistedAssets.map(_.id.toString).mkString(",")}]
       |  blacklisted-names   = ["name$$"]
       |  price-assets        = [${predefinedPriceAssets.map(_.id.toString).mkString(",")}]
       |  white-list-only     = no
       |  allowed-asset-pairs = [ACRYL-${Asset3.id.toString}]
       |}""".stripMargin)

  private val settings   = loadConfig(priceAssets).as[MatcherSettings]("acryl.dex")
  private val blockchain = stub[Blockchain]

  private val builder = new AssetPairBuilder(settings, blockchain, blacklistedAssets)

  private val pairs = Table(
    ("amount", "price", "result"),
    (ACRYL, WUSD.id.toString, Right(())),
    (WUSD.id.toString, ACRYL, Left("AssetPairReversed")),
    (WBTC.id.toString, WEUR.id.toString, Left("AssetPairReversed")),
    (WEUR.id.toString, WBTC.id.toString, Right(())),
    (Asset1.id.toString, ACRYL, Right(())),
    (ACRYL, Asset1.id.toString, Left("AssetPairReversed")),
    (Asset2.id.toString, Asset1.id.toString, Right(())),
    (Asset1.id.toString, Asset2.id.toString, Left("AssetPairReversed")),
    (Asset1.id.toString, WBTC.id.toString, Right(())),
    (WEUR.id.toString, Asset1.id.toString, Left("AssetPairReversed")),
    (ACRYL, Asset3.id.toString, Right(())),
  )

  "AssetPairBuilder" - {
    "correctly ordered and assets IDs are valid" in {
      for (id <- predefinedPriceAssets) {
        (blockchain.assetDescription _).when(id).returns(mkAssetDescription())
      }

      (blockchain.assetDescription _).when(Asset1).returns(mkAssetDescription())
      (blockchain.assetDescription _).when(Asset2).returns(mkAssetDescription())
      (blockchain.assetDescription _).when(Asset3).returns(mkAssetDescription())

      forAll(pairs) {
        case (amountAsset, priceAsset, isValid) =>
          val pair = builder.createAssetPair(amountAsset, priceAsset)
          isValid match {
            case Right(_) => pair shouldBe 'right
            case Left(e)  => pair should produce(e)
          }
      }
    }
    "rejects a pair when" - {
      "blacklist" - {
        "contains asset id" in {
          (blockchain.assetDescription _).when(Asset3).returns(mkAssetDescription())
          builder.validateAssetPair(AssetPair(Asset3, Acryl)) should produce("AmountAssetBlacklisted")
        }
        "matchers asset name" in {
          (blockchain.assetDescription _).when(Asset1).returns(mkAssetDescription())
          (blockchain.assetDescription _).when(Asset2).returns(mkAssetDescription("forbidden Asset name"))
          (blockchain.assetDescription _).when(Asset3).returns(mkAssetDescription("name of an asset"))

          builder.validateAssetPair(AssetPair(Asset3, Asset1)) should produce("AmountAssetBlacklisted")
          builder.validateAssetPair(AssetPair(Asset2, Asset1)) should produce("AmountAssetBlacklisted")
        }
      }
      "asset was not issued" in {
        (blockchain.assetDescription _).when(Asset1).returns(None)
        (blockchain.assetDescription _).when(Asset2).returns(mkAssetDescription())

        builder.validateAssetPair(AssetPair(Asset2, Asset1)) should produce("AssetNotFound")
      }
      "amount and price assets are the same" in {
        builder.validateAssetPair(AssetPair(WUSD, WUSD)) should produce("AssetPairSameAsset")
      }
      "pair is not in allowedAssetPairs and whiteListOnly is enabled" in {
        val builder   = new AssetPairBuilder(settings.copy(whiteListOnly = true), blockchain, blacklistedAssets)
        val assetPair = AssetPair(Acryl, WUSD)
        builder.validateAssetPair(assetPair) should produce("AssetPairIsDenied")
      }
    }
  }
}

object AssetPairBuilderSpec {
  private def mkAssetId(index: Byte): IssuedAsset = IssuedAsset(ByteStr(Array.fill[Byte](32)(index)))
  private def mkAssetDescription(assetName: String = ""): Option[AssetDescription] =
    Some(
      AssetDescription(PublicKey(Array.emptyByteArray),
                       assetName.getBytes(Charsets.UTF_8),
                       Array.emptyByteArray,
                       8,
                       reissuable = false,
                       BigInt(1),
                       None,
                       0))
}
