package com.acrylplatform.it

import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.smart.SetScriptTransaction
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import com.acrylplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc, script, 0.014.acryl, System.currentTimeMillis())
      .explicitGet()

    nodes.head
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }
}
