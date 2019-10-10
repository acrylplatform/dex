package com.acrylplatform.dex.db

import com.acrylplatform.account.Address
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.model.{OrderInfo, OrderStatus}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, Order}

object EmptyOrderDB extends OrderDB {
  override def containsInfo(id: ByteStr): Boolean                                                  = false
  override def status(id: ByteStr): OrderStatus.Final                                              = OrderStatus.NotFound
  override def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit = {}
  override def saveOrder(o: Order): Unit                                                           = {}
  override def loadRemainingOrders(owner: Address,
                                   maybePair: Option[AssetPair],
                                   activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])] =
    Seq.empty
}
