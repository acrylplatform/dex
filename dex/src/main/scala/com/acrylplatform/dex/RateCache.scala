package com.acrylplatform.dex

import java.util.concurrent.ConcurrentHashMap

import com.acrylplatform.dex.db.RateDB
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB
import play.api.libs.json.{JsObject, Json}

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap

trait RateCache {

  /** Adds or updates asset rate, returns previous rate value if there was one */
  def upsertRate(asset: Asset, value: Double): Option[Double]

  def getRate(asset: Asset): Option[Double]

  def getAllRates: Map[Asset, Double]

  /** Deletes asset rate, returns previous rate value if there was one */
  def deleteRate(asset: Asset): Option[Double]

  def getJson: JsObject = RateCache.getJson(getAllRates)
}

object RateCache {

  def getJson(ratesMap: Map[Asset, Double]): JsObject = Json.obj(
    ratesMap.map { case (asset, rate) => AssetPair.assetIdStr(asset) -> Json.toJsFieldJsValueWrapper(rate) }.toSeq: _*
  )

  def apply(db: DB): RateCache = new RateCache {

    private val rateDB    = RateDB(db)
    private val rateMap   = new ConcurrentHashMap[IssuedAsset, Double](rateDB.getAllRates.asJava)
    private val AcrylRate = Option(1d)

    def upsertRate(asset: Asset, value: Double): Option[Double] =
      asset.fold { AcrylRate } { issuedAsset =>
        rateDB.upsertRate(issuedAsset, value)
        Option(rateMap.put(issuedAsset, value))
      }

    def getRate(asset: Asset): Option[Double] = asset.fold { AcrylRate } { asset =>
      if (rateMap containsKey asset) Some(rateMap get asset) else None
    }

    def getAllRates: Map[Asset, Double] = {
      rateMap.asScala.toMap.map { case (issuedAsset, value) => Asset.fromCompatId(issuedAsset.compatId) -> value } + (Acryl -> 1d)
    }

    def deleteRate(asset: Asset): Option[Double] = asset.fold { AcrylRate } { issuedAsset =>
      rateDB.deleteRate(issuedAsset)
      Option(rateMap.remove(issuedAsset))
    }
  }

  def inMem: RateCache = new RateCache {

    private val rates: TrieMap[Asset, Double] = TrieMap(Acryl -> 1d)

    def upsertRate(asset: Asset, value: Double): Option[Double] = {
      asset.fold { Option(1d) } { issuedAsset =>
        val previousValue = rates.get(issuedAsset)
        rates += (asset -> value)
        previousValue
      }
    }

    def getRate(asset: Asset): Option[Double] = rates.get(asset)
    def getAllRates: Map[Asset, Double]       = rates.toMap

    def deleteRate(asset: Asset): Option[Double] =
      asset.fold { Option(1d) } { issuedAsset =>
        val previousValue = rates.get(issuedAsset)
        rates -= issuedAsset
        previousValue
      }
  }
}
