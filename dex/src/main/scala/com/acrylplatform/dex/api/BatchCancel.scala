package com.acrylplatform.dex.api
import com.acrylplatform.account.Address
import com.acrylplatform.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
