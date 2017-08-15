package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.crypto.authds.avltree.AVLValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  *
  * @param rootHash
  */
class UtxoState(override val rootHash: Digest, dir: File) extends ErgoState[UtxoState] {

  implicit val hf = new Blake2b256Unsafe

  private val store = new LSMStore(dir)
  private val np = NodeParameters(keySize = 32, valueSize = ErgoState.BoxSize, labelSize = 32)
  protected val storage = new VersionedIODBAVLStorage(store, np)

  protected val persistentProver =
    new PersistentBatchAVLProver(new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize)), storage)

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(height, height))
  }

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): ADProof.ProofRepresentation =
    txs.flatMap(_.id).toArray

  //todo: the same question as with DigestState.version
  override def version: VersionTag = rootHash

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    p.rollback(version).map { _ =>
      new UtxoState(version, dir) {
        override protected val persistentProver = p
      }
    }
  }

  override def validate(mod: ErgoPersistentModifier): Try[Unit] =
    Failure(new Exception("validate() is not implemented for UtxoState as it requires for costly provers' rollback"))

  //todo: utxo snapshot could go here
  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] =
    mod match {
      case fb: ErgoFullBlock =>
        Try {
          assert(fb.parentId.sameElements(version))

          val transactions = fb.blockTransactions.txs

          transactions.foreach(tx => assert(tx.semanticValidity.isSuccess))

          val mods = boxChanges(transactions).operations.map(ADProof.changeToMod)
          mods.foldLeft[Try[Option[AVLValue]]](Success(None)) { case (t, m) =>
            t.flatMap(_ => {
              persistentProver.performOneOperation(m)
            })
          }
          assert(fb.header.stateRoot.sameElements(persistentProver.digest), "digest after txs application is wrong")

          //todo: persistentProver.prover().generateProof(), or implement persistentProver.generateProof()
          val proofBytes: Array[Byte] = ???
          val proofHash = hf(proofBytes)
          assert(fb.header.ADProofsRoot.sameElements(proofHash))
          new UtxoState(persistentProver.digest, dir)
        }

      case a: Any =>
        log.info(s"Unhandled modifier: $a")
        Failure(new Exception("unknown modifier"))
    }
}

object UtxoState {
  def fromBoxHolder(bh: BoxHolder, dir: File): UtxoState = {
    val p = new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(ErgoState.BoxSize))
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, b.bytes)))

    new UtxoState(p.digest, dir) {
      override val persistentProver = new PersistentBatchAVLProver(p, storage)
    }
  }
}
