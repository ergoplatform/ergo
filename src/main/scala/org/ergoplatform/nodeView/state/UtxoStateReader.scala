package org.ergoplatform.nodeView.state

import io.iohk.iodb.Store
import org.ergoplatform.{ErgoBox, ErgoTransaction}
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.{Failure, Success, Try}

trait UtxoStateReader extends ErgoStateReader with ScorexLogging with TransactionValidation[AnyoneCanSpendProposition.type, ErgoTransaction] {

  protected implicit val hf = Algos.hash

  val store: Store
  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
    val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    PersistentBatchAVLProver.create(bp, storage).get
  }

  override def validate(tx: ErgoTransaction): Try[Unit] = if (tx.inputs.map(_.boxId).forall { k =>
    persistentProver.unauthenticatedLookup(k).isDefined
  }) {
    Success()
  } else {
    Failure(new Exception(s"Not all boxes of the transaction $tx are in the state"))
  }

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[ErgoBox] = {
    //TODO fix
    randomBox().toIndexedSeq
  }

  def boxById(id: ADKey): Option[ErgoBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(ErgoBox.serializer.parseBytes)
      .flatMap(_.toOption)

  def randomBox(): Option[ErgoBox] =
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)


  /**
    * Generate proofs for specified transactions if applied to current state
    *
    * @param txs - transactions to generate proofs
    * @return proof for specified transactions and new state digest
    */
  def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
    log.debug(s"Going to create proof for ${txs.length} transactions")
    val rootHash = persistentProver.digest
    if (txs.isEmpty) {
      Failure(new Error("Trying to generate proof for empty transaction sequence"))
    } else if (!storage.version.exists(_.sameElements(rootHash))) {
      Failure(new Error(s"Incorrect storage: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}"))
    } else {
      persistentProver.avlProver.generateProofForOperations(boxChanges(txs).operations.map(ADProofs.changeToMod))
    }
  }
}
