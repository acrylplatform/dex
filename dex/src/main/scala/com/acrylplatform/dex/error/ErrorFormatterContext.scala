package com.acrylplatform.dex.error

import com.acrylplatform.transaction.Asset

trait ErrorFormatterContext {
  def assetDecimals(asset: Asset): Int
}
