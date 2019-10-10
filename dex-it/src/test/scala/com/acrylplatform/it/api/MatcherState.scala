package com.acrylplatform.it.api

import com.acrylplatform.account.KeyPair
import com.acrylplatform.dex.queue.QueueEventWithMeta
import com.acrylplatform.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, (OrderBookResponse, MarketStatusResponse)],
                        orderStatuses: Map[String, MatcherStatusResponse],
                        reservedBalances: Map[KeyPair, Map[String, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[OrderbookHistory]]])
