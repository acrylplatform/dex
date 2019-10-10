package com.acrylplatform.dex.model

import java.util.concurrent.ConcurrentHashMap

import com.acrylplatform.state.Blockchain
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.assets.exchange.AssetPair
import com.acrylplatform.utils.ScorexLogging

class AssetDecimalsCache(blockchain: Blockchain) extends ScorexLogging {

  private val AcrylDecimals      = 8
  private val assetDecimalsCache = new ConcurrentHashMap[Asset, Int](1000, 0.9f, 10)

  def get(asset: Asset): Int = {
    asset.fold { AcrylDecimals } { issuedAsset =>
      Option(assetDecimalsCache.get(asset)) getOrElse {
        val assetDecimals =
          blockchain
            .assetDescription(issuedAsset)
            .map(_.decimals)
            .getOrElse {
              log.error(s"Can not get asset decimals since asset '${AssetPair.assetIdStr(asset)}' not found!")
              8
            }

        assetDecimalsCache.put(issuedAsset, assetDecimals)
        assetDecimals
      }
    }
  }
}
