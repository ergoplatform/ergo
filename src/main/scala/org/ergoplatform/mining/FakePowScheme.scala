package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.math.BigInt
import scala.util.Random

class FakePowScheme(levelOpt: Option[Int]) extends PoWScheme {

  val start = Long.MinValue
  val finish = Long.MaxValue

  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     votes: Array[Byte],
                     startingNonce: Long,
                     finishingNonce: Long): Option[Header] = {

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)
    val level: Int = levelOpt.map(lvl => BigInt(2).pow(lvl).toInt).getOrElse(Random.nextInt(1000) + 1)
    Some(new Header(version, parentId, interlinks,
      adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits, height, votes,
      nonce = 0L, Ints.toByteArray(level)))
  }

  override def verify(header: Header): Boolean = true

  override def realDifficulty(header: Header): Difficulty =
    Ints.fromByteArray(header.equihashSolutions) * header.requiredDifficulty

  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            votes: Array[Byte]): Header = {
    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, votes, start, finish).get
  }

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[AnyoneCanSpendTransaction],
                 timestamp: Timestamp,
                 votes: Array[Byte]): ErgoFullBlock = {
    proveBlock(parentOpt, nBits, stateRoot, adProofBytes, transactions, timestamp, votes, start, finish).get
  }

}

object DefaultFakePowScheme extends FakePowScheme(None)
