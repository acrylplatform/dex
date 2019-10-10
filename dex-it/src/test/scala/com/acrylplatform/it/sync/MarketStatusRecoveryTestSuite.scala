package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.it.api.MatcherState

class MarketStatusRecoveryTestSuite extends MatcherRecoveryTestSuite {
  // To create a snapshot for each event at least for one order book
  protected override def configOverrides: Config =
    ConfigFactory.parseString("acryl.dex.snapshots-interval = 2").withFallback(super.configOverrides)

  override protected def cleanState(state: MatcherState): MatcherState = state.copy(snapshots = Map.empty)
}
