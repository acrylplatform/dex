package com.acrylplatform.dex.smart

import com.acrylplatform.account.{KeyPair, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.dex.error.ProduceError.produce
import com.acrylplatform.lang.script.Script
import com.acrylplatform.lang.v1.compiler.Terms
import com.acrylplatform.lang.v1.evaluator.Log
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.exchange.{AssetPair, OrderType, OrderV1}
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import com.acrylplatform.{NoShrink, TransactionGen}
import org.scalatest.{FreeSpecLike, Matchers}

import scala.util.Try

class MatcherScriptRunnerTest extends FreeSpecLike with Matchers with TransactionGen with NoShrink {
  private val sampleOrder = OrderV1(
    sender = KeyPair("test".getBytes()),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Acryl, IssuedAsset(ByteStr("asset".getBytes("utf-8")))),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = 30000L
  )

  private def run(script: Script): (Log, Either[String, Terms.EVALUATED]) = MatcherScriptRunner(script, sampleOrder)

  "dApp sunny day" in {
    run(dAppScriptSunny)._2.explicitGet() shouldBe Terms.FALSE
  }

  "Blockchain functions are disabled in dApp" in {
    Try(run(dAppScriptBlockchain)).toEither should produce("""An access to the blockchain\.accountData is denied on DEX""".r)
  }

  private def dAppScriptSunny: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |let addr = addressFromPublicKey(base58'H1kGVNdJwV7N5dF73YWs1R8uet6g5bCvTHmTnYy1hSnr')
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => o.sender == addr
            |      case _ => false
            |    }
            |}
            |""".stripMargin
      )
      .explicitGet()
      ._1

  private def dAppScriptBlockchain: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => getBooleanValue(o.sender, "foo")
            |      case _ => false
            |    }
            |}
            |""".stripMargin
      )
      .explicitGet()
      ._1
}
