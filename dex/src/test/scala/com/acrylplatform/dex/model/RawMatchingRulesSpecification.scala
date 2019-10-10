package com.acrylplatform.dex.model

import cats.data.NonEmptyList
import com.acrylplatform.NoShrink
import com.acrylplatform.dex.MatcherTestData
import com.acrylplatform.dex.settings.RawMatchingRules
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class RawMatchingRulesSpecification extends PropSpec with PropertyChecks with Matchers with MatcherTestData with NoShrink {
  property("skipOutdated: rules.head.startOffset <= currentOffset < rules(1).startOffset") {
    val g = for {
      currOffset <- currOffsetGen
      rules      <- rulesChainGen(5)
    } yield (currOffset, rules)

    forAll(g) {
      case (currOffset, rules) =>
        val updatedRules = RawMatchingRules.skipOutdated(currOffset, rules)
        updatedRules.toList match {
          case first :: Nil =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
          case first :: second :: _ =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
            withClue(s"currOffset=$currOffset, second.startOffset=${second.startOffset}") {
              currOffset shouldBe <(second.startOffset)
            }
          case xs => throw new IllegalStateException(s"$xs")
        }
    }
  }

  private val currOffsetGen = Gen.choose(0L, Long.MaxValue)

  private def nextRulesGen(prevRules: RawMatchingRules): Gen[Option[RawMatchingRules]] =
    if (prevRules.startOffset == Long.MaxValue) Gen.const(None)
    else
      for {
        startOffset <- Gen.choose(prevRules.startOffset + 1, Long.MaxValue)
        tickSize    <- Gen.choose(1, Double.MaxValue)
      } yield Some(RawMatchingRules(startOffset, tickSize))

  private val firstRuleGen: Gen[RawMatchingRules] = Gen.choose(1, Double.MaxValue).map(RawMatchingRules(0L, _))

  private def rulesChainGen(maxNumber: Int): Gen[NonEmptyList[RawMatchingRules]] = {
    def loop(rest: Int, acc: Gen[NonEmptyList[RawMatchingRules]]): Gen[NonEmptyList[RawMatchingRules]] =
      if (rest == 0) acc
      else
        for {
          xs <- acc
          x  <- nextRulesGen(xs.head)
          r  <- x.fold(Gen.const(xs))(x => loop(rest - 1, Gen.const(x :: xs)))
        } yield r

    Gen.lzy(loop(maxNumber, firstRuleGen.map(NonEmptyList.one)).map(_.reverse))
  }
}
