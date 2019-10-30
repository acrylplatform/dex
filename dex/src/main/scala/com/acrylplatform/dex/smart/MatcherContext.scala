package com.acrylplatform.dex.smart

import cats.Eval
import com.acrylplatform.account.{Address, Alias}
import com.acrylplatform.block.Block.BlockId
import com.acrylplatform.block.{Block, BlockHeader}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.lang.directives.values._
import com.acrylplatform.lang.script.Script
import com.acrylplatform.lang.v1.evaluator.ctx._
import com.acrylplatform.lang.{ExecutionError, ValidationError}
import com.acrylplatform.settings.BlockchainSettings
import com.acrylplatform.state.reader.LeaseDetails
import com.acrylplatform.state.{AccountDataInfo, AssetDescription, BalanceSnapshot, Blockchain, DataEntry, InvokeScriptResult, LeaseBalance, Portfolio, TransactionId, VolumeAndFee}
import com.acrylplatform.transaction.assets.exchange.Order
import com.acrylplatform.transaction.lease.LeaseTransaction
import com.acrylplatform.transaction.smart.BlockchainContext
import com.acrylplatform.transaction.transfer.TransferTransaction
import com.acrylplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.control.NoStackTrace

// Used only for order validation
object MatcherContext {

  def build(version: StdLibVersion, nByte: Byte, inE: Eval[Order], isDApp: Boolean): Either[ExecutionError, EvaluationContext] = {
    val in: Coeval[Order] = Coeval.delay(inE.value)
    BlockchainContext
      .build(
        version,
        nByte,
        in.map(o => Coproduct[BlockchainContext.In](o)),
        Coeval.raiseError(new Denied("height")),
        deniedBlockchain,
        isTokenContext = false,
        isContract = isDApp,
        in.map(_.senderPublicKey.toAddress.bytes)
      )
  }

  private class Denied(methodName: String) extends SecurityException(s"An access to the blockchain.$methodName is denied on DEX") with NoStackTrace
  private def kill(methodName: String) = throw new Denied(methodName)

  private val deniedBlockchain = new Blockchain {
    override def settings: BlockchainSettings                                     = kill("settings")
    override def height: Int                                                      = kill("height")
    override def score: BigInt                                                    = kill("score")
    override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]      = kill("blockHeaderAndSize")
    override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = kill("blockHeaderAndSize")
    override def lastBlock: Option[Block]                                         = kill("lastBlock")
    override def carryFee: Long                                                   = kill("carryFee")
    override def blockBytes(height: Int): Option[Array[Byte]]                     = kill("blockBytes")
    override def blockBytes(blockId: ByteStr): Option[Array[Byte]]                = kill("blockBytes")
    override def heightOf(blockId: ByteStr): Option[Int]                          = kill("heightOf")

    /** Returns the most recent block IDs, starting from the most recent  one */
    override def lastBlockIds(howMany: Int): Seq[ByteStr] = kill("lastBlockIds")

    /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
    override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = kill("blockIdsAfter")
    override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader]            = kill("parentHeader")
    override def totalFee(height: Int): Option[Long]                                         = kill("totalFee")

    /** Features related */
    override def approvedFeatures: Map[Short, Int]                                 = kill("approvedFeatures")
    override def activatedFeatures: Map[Short, Int]                                = kill("activatedFeatures")
    override def featureVotes(height: Int): Map[Short, Int]                        = kill("featureVotes")
    override def transactionInfo(id: ByteStr): Option[(Int, Transaction)]          = kill("transactionInfo")
    override def transactionHeight(id: ByteStr): Option[Int]                       = kill("transactionHeight")
    override def containsTransaction(tx: Transaction): Boolean                     = kill("containsTransaction")
    override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = kill("assetDescription")
    override def resolveAlias(a: Alias): Either[ValidationError, Address]          = kill("resolveAlias")
    override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]              = kill("leaseDetails")
    override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee                = kill("filledVolumeAndFee")

    /** Retrieves Acryl balance snapshot in the [from, to] range (inclusive) */
    override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = kill("balanceSnapshots")
    override def accountScript(address: Address): Option[Script]                                  = kill("accountScript")
    override def hasScript(address: Address): Boolean                                             = kill("hasScript")
    override def assetScript(id: Asset.IssuedAsset): Option[Script]                               = kill("assetScript")
    override def hasAssetScript(id: Asset.IssuedAsset): Boolean                                   = kill("hasAssetScript")
    override def accountDataKeys(address: Address): Seq[String]                                   = kill("accountDataKeys")
    override def accountData(acc: Address, key: String): Option[DataEntry[_]]                     = kill("accountData")
    override def accountData(acc: Address): AccountDataInfo                                       = kill("accountData")
    override def leaseBalance(address: Address): LeaseBalance                                     = kill("leaseBalance")
    override def balance(address: Address, mayBeAssetId: Asset): Long                             = kill("balance")

    /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
      *
      * @note Portfolios passed to `pf` only contain Acryl and Leasing balances to improve performance */
    override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = kill("collectLposPortfolios")
    override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult]    = kill("invokeScriptResult")

    override def transferById(id: BlockId): Option[(Int, TransferTransaction)]                           = kill("transferById")

    override def collectActiveLeases[T](pf: PartialFunction[LeaseTransaction, T]): Seq[T] = kill("collectActiveLeases")
  }

}
