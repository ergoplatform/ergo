package org.ergoplatform.utils

import org.ergoplatform.modifiers.history.{BlockTransactions, BlockTransactionsSerializer}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.bytesToId
import sigma.VersionContext
import sigma.ast.{EvaluatedValue, IntConstant, SLong, SType, Tuple, Upcast}

/**
  * A register or context extension value serialized with a leading Upcast of a constant from ErgoTree version 3
  * and without it below, and transactions as a node reads them from a block.
  */
object VersionDependentValues {

  val upcastValue: EvaluatedValue[SType] = Tuple(IntConstant(1), Upcast(IntConstant(1), SLong))

  // `upcastValue` serialized from ErgoTree version 3 and below it
  val upcastHexV3: String = "860204027e040205"
  val upcastHexBelowV3: String = "860204020402"

  private val v6 = Header.scriptAndTreeFromBlockVersions(Header.Interpreter60Version)

  /** Runs `block` at the versions a v6 block's transactions are parsed at. */
  def atV6[T](block: => T): T = VersionContext.withVersions(v6.activatedVersion, v6.ergoTreeVersion)(block)

  /** `tx` as a node reads it from a block of `blockVersion` (a v6 block by default). */
  def readFromBlock(tx: ErgoTransaction, blockVersion: Byte = Header.Interpreter60Version): ErgoTransaction = {
    val section = BlockTransactions(bytesToId(Array.fill(32)(1.toByte)), blockVersion, Seq(tx))
    BlockTransactionsSerializer.parseBytes(BlockTransactionsSerializer.toBytes(section)).txs.head
  }

}
