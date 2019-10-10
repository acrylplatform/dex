package com.acrylplatform.dexgen.utils

import java.util.concurrent.ThreadLocalRandom

import com.acrylplatform.account.{Address, KeyPair, PublicKey}
import com.acrylplatform.dexgen.utils.Implicits._
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.Transaction
import com.acrylplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.acrylplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}
import scorex.crypto.signatures.Curve25519.KeyLength

object Gen {
  private def random = ThreadLocalRandom.current

  def txs(minFee: Long, maxFee: Long, senderAccounts: Seq[KeyPair], recipientGen: Iterator[Address]): Iterator[Transaction] = {
    val senderGen = Iterator.randomContinually(senderAccounts)
    val feeGen    = Iterator.continually(minFee + random.nextLong(maxFee - minFee))
    transfers(senderGen, recipientGen, feeGen)
      .zip(massTransfers(senderGen, recipientGen, feeGen))
      .flatMap { case (tt, mtt) => Iterator(mtt, tt) }
  }

  def transfers(senderGen: Iterator[KeyPair], recipientGen: Iterator[Address], feeGen: Iterator[Long]): Iterator[Transaction] = {
    val now = System.currentTimeMillis()
    senderGen
      .zip(recipientGen)
      .zip(feeGen)
      .zipWithIndex
      .map {
        case (((src, dst), fee), i) =>
          TransferTransactionV1.selfSigned(Acryl, src, dst, fee, now + i, Acryl, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }

  def massTransfers(senderGen: Iterator[KeyPair], recipientGen: Iterator[Address], amountGen: Iterator[Long]): Iterator[Transaction] = {
    val transferCountGen = Iterator.continually(random.nextInt(MassTransferTransaction.MaxTransferCount + 1))
    senderGen
      .zip(transferCountGen)
      .map {
        case (sender, count) =>
          val transfers = List.tabulate(count)(_ => ParsedTransfer(recipientGen.next(), amountGen.next()))
          val fee       = 100000 + count * 50000
          MassTransferTransaction.selfSigned(Acryl, sender, transfers, System.currentTimeMillis, fee, Array.emptyByteArray)
      }
      .collect { case Right(tx) => tx }
  }

  val address: Iterator[Address] = Iterator.continually {
    val pk = Array.fill[Byte](KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(PublicKey(pk))
  }

  def address(uniqNumber: Int): Iterator[Address] = Iterator.randomContinually(address.take(uniqNumber).toSeq)

  def address(limitUniqNumber: Option[Int]): Iterator[Address] = limitUniqNumber.map(address(_)).getOrElse(address)

}
