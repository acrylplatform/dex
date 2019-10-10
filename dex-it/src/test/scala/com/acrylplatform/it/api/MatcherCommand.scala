package com.acrylplatform.it.api

import com.acrylplatform.account.KeyPair
import com.acrylplatform.it.Node
import com.acrylplatform.transaction.assets.exchange.Order

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(node: Node, order: Order)                         extends MatcherCommand
  case class Cancel(node: Node, owner: KeyPair, order: Order) extends MatcherCommand
}
