package org.ergoplatform.mining

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.ModifierId
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.math.BigInt

trait PowScheme {

  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            extensionHash: Digest32
           ): Option[Header]

  def proveBlock(parentOpt: Option[Header],
                 nBits: Long,
                 stateRoot: ADDigest,
                 adProofBytes: SerializedAdProof,
                 transactions: Seq[AnyoneCanSpendTransaction],
                 timestamp: Timestamp,
                 extensionHash: Digest32): Option[ErgoFullBlock] = {

    val transactionsRoot = BlockTransactions.rootHash(transactions.map(_.id))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot,
      timestamp, extensionHash).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new ErgoFullBlock(h, BlockTransactions(h.id, transactions), Some(adProofs))
    }
  }

  def proveBlock(candidateBlock: CandidateBlock): Option[ErgoFullBlock] = {

    val parentOpt: Option[Header] = candidateBlock.parentOpt
    val nBits: Long = candidateBlock.nBits
    val stateRoot: ADDigest = candidateBlock.stateRoot
    val adProofBytes: SerializedAdProof = candidateBlock.adProofBytes
    val transactions: Seq[AnyoneCanSpendTransaction] = candidateBlock.transactions
    val timestamp: Timestamp = candidateBlock.timestamp
    val extensionHash: Digest32 = candidateBlock.extensionHash

    val transactionsRoot = BlockTransactions.rootHash(transactions.map(_.id))
    val adProofsRoot = ADProofs.proofDigest(adProofBytes)

    prove(parentOpt, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, extensionHash).map { h =>
      val adProofs = ADProofs(h.id, adProofBytes)
      new ErgoFullBlock(h, BlockTransactions(h.id, transactions), Some(adProofs))
    }
  }

  def verify(header: Header): Boolean

  def realDifficulty(header: Header): BigInt

  protected def derivedHeaderFields(parentOpt: Option[Header]): (ModifierId, Byte, Seq[ModifierId], Int) = {
    val interlinks: Seq[ModifierId] =
      parentOpt.map(parent => new PoPoWProofUtils(this).constructInterlinkVector(parent)).getOrElse(Seq.empty)

    val height = parentOpt.map(parent => parent.height + 1).getOrElse(0)

    val version = 0: Byte

    val parentId: ModifierId = ModifierId @@ parentOpt.map(_.id).getOrElse(Header.GenesisParentId)

    (parentId, version, interlinks, height)
  }

  def correctWorkDone(realDifficulty: Difficulty, difficulty: BigInt): Boolean = {
    realDifficulty >= difficulty
  }
}


