package org.ergoplatform.nodeView.state

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

trait UtxoStateReader extends ErgoStateReader with TransactionValidation[ErgoTransaction] {

  protected implicit val hf = Algos.hash

  val constants: StateConstants
  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, HF]

  override def validate(tx: ErgoTransaction): Try[Unit] = tx.statelessValidity
    .flatMap(_ => tx.statefulValidity(tx.inputs.flatMap(i => boxById(i.boxId)), stateContext).map(_ => Unit))

  /**
    *
    * @param fb - ergo full block
    * @return emission box from this block transactions
    */
  protected[state] def extractEmissionBox(fb: ErgoFullBlock): Option[ErgoBox] = emissionBoxIdOpt match {
    case Some(id) =>
      fb.blockTransactions.txs.view.reverse.find(_.inputs.exists(t => java.util.Arrays.equals(t.boxId, id))) match {
        case Some(tx) if tx.outputs.head.proposition == constants.genesisEmissionBox.proposition =>
          Some(tx.outputs.head)
        case Some(_) =>
          log.info(s"Last possible emission box consumed")
          None
        case None =>
          log.warn(s"Emission box not found in block ${fb.encodedId}")
          boxById(id)
      }
    case None =>
      log.debug("No emission box: emission should be already finished before this block")
      None
  }

  protected def emissionBoxIdOpt: Option[ADKey] = store.get(ByteArrayWrapper(UtxoState.EmissionBoxIdKey))
    .map(s => ADKey @@ s.data)

  def emissionBoxOpt: Option[ErgoBox] = emissionBoxIdOpt.flatMap(boxById)

  def boxById(id: ADKey): Option[ErgoBox] =
    persistentProver
      .unauthenticatedLookup(id)
      .map(ErgoBoxSerializer.parseBytes)
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
    val rootHash = persistentProver.digest
    log.debug(s"Going to create proof for ${txs.length} transactions at root ${Algos.encode(rootHash)}")
    if (txs.isEmpty) {
      Failure(new Error("Trying to generate proof for empty transaction sequence"))
    } else if (!storage.version.exists(t => java.util.Arrays.equals(t, rootHash))) {
      Failure(new Error(s"Incorrect storage: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}. " +
        s"Possible reason - state update is in process."))
    } else {
      persistentProver.avlProver.generateProofForOperations(ErgoState.stateChanges(txs).operations.map(ADProofs.changeToMod))
    }
  }
}
