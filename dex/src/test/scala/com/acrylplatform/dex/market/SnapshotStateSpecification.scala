package com.acrylplatform.dex.market

import com.acrylplatform.NoShrink
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction.assets.exchange.AssetPair
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpecLike}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => DrivenPropertyChecks}

class SnapshotStateSpecification extends PropSpecLike with DrivenPropertyChecks with Matchers with NoShrink {
  property("nextSnapshotOffset generates greater offsets than old and last processed") {
    val assetPair = AssetPair(
      IssuedAsset(ByteStr("asset1".getBytes())),
      IssuedAsset(ByteStr("asset2".getBytes()))
    )

    val g = for {
      interval            <- Gen.choose(1, 1000L).label("interval")
      currSnapshotOffset  <- Gen.choose(-1, 1000L).label("currSnapshotOffset")
      lastProcessedOffset <- Gen.choose(-1, 1000L).label("lastProcessedOffset")
    } yield (currSnapshotOffset, lastProcessedOffset, interval)

    forAll(g) {
      case (currSnapshotOffset, lastProcessedOffset, interval) =>
        val nextOffset = SnapshotsState.nextSnapshotOffset(assetPair, currSnapshotOffset, lastProcessedOffset, interval)
        nextOffset should be > currSnapshotOffset
        nextOffset should be > lastProcessedOffset
    }
  }
}
