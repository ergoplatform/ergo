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
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Failure, Success, Try}

/**
  * Utxo set implementation.
  * @param rootHash
  */
class UtxoState(override val rootHash: Digest, dir: File = new File("/tmp/utxo")) extends ErgoState[UtxoState] {

  implicit val hf = new Blake2b256Unsafe

  private val store = new LSMStore(dir)
  private val storage = new VersionedIODBAVLStorage(store, NodeParameters(keySize = 32, valueSize = 48, labelSize = 32))

  private val prover = new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(48))

  protected val persistentProver = new PersistentBatchAVLProver(prover, storage)

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(new AnyoneCanSpendProposition, height, height))
  }

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): ADProof.ProofRepresentation =
    txs.flatMap(_.id).toArray

  //todo: the same question as with DigestState.version
  override def version: VersionTag = rootHash

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val p = persistentProver
    p.rollback(version).map { _ =>
      new UtxoState(version) {
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

          val proofBytes = prover.generateProof()
          val proofHash = hf(proofBytes)
          assert(fb.header.ADProofsRoot.sameElements(proofHash))
          new UtxoState(persistentProver.digest)
        }

      case a: Any =>
        log.info(s"Unhandled modifier: $a")
        Failure(new Exception("unknown modifier"))
    }
}
