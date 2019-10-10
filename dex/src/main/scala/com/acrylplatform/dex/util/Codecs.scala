package com.acrylplatform.dex.util

import java.nio.ByteBuffer

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.model.OrderStatus
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}

object Codecs {
  def len(assetId: Asset): Int = assetId.fold(1)(1 + _.id.arr.length)

  implicit class ByteBufferExt(val b: ByteBuffer) extends AnyVal {
    def putAssetId(assetId: Asset): ByteBuffer = assetId match {
      case Acryl => b.put(0.toByte)
      case IssuedAsset(aid) =>
        require(aid.arr.length < Byte.MaxValue, "Asset ID is too long")
        b.put(aid.arr.length.toByte).put(aid.arr)
    }

    def getAssetId: Asset = b.get() match {
      case 0 => Acryl
      case len =>
        val arr = new Array[Byte](len)
        b.get(arr)
        IssuedAsset(ByteStr(arr))
    }

    def putFinalOrderStatus(orderInfoVersion: Byte, st: OrderStatus): ByteBuffer = {
      val tpe: Byte = st match {
        case _: OrderStatus.Filled    => 0
        case _: OrderStatus.Cancelled => 1
        case x                        => throw new IllegalArgumentException(s"Can't encode order status $x")
      }
      val r = b.put(tpe).putLong(st.filledAmount)
      if (orderInfoVersion <= 1) r else r.putLong(st.filledFee)
    }

    def getFinalOrderStatus(orderInfoVersion: Byte, totalAmount: Long, totalFee: Long): OrderStatus.Final = {
      def fee(filledAmount: Long) = if (orderInfoVersion <= 1) (BigInt(filledAmount) * totalFee / totalAmount).toLong else b.getLong

      b.get match {
        case 0 =>
          val filledAmount = b.getLong
          OrderStatus.Filled(filledAmount, fee(filledAmount))
        case 1 =>
          val filledAmount = b.getLong
          OrderStatus.Cancelled(filledAmount, fee(filledAmount))
        case x => throw new IllegalArgumentException(s"Can't parse order status: $x")
      }
    }
  }
}
