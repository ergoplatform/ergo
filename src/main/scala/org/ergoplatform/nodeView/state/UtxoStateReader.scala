package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.transaction.state.TransactionValidation
import scorex.crypto.authds.avltree.batch.{NodeParameters, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

trait UtxoStateReader extends ErgoStateReader with TransactionValidation[ErgoTransaction] {

  protected implicit val hf: HF = Algos.hash

  val constants: StateConstants

  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedLDBAVLStorage(store, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, HF]

  /**
    * Validate transaction against provided state context, if specified,
    * or state context from the previous block if not
    */
  def validateWithCost(tx: ErgoTransaction,
                       stateContextOpt: Option[ErgoStateContext],
                       complexityLimit: Int): Try[Long] = {
    val verifier = ErgoInterpreter(stateContext.currentParameters)
    val context = stateContextOpt.getOrElse(stateContext)
    tx.statelessValidity().flatMap { _ =>
      val boxesToSpend = tx.inputs.flatMap(i => boxById(i.boxId))
      val txComplexity = boxesToSpend.map(_.ergoTree.complexity).sum
      if (txComplexity > complexityLimit) {
        throw new Exception(s"Transaction $tx has too high complexity $txComplexity")
      }
      tx.statefulValidity(
        boxesToSpend,
        tx.dataInputs.flatMap(i => boxById(i.boxId)),
        context)(verifier)
    }
  }

  /**
    * Validate transaction as if it was included at the end of the last block.
    * This validation does not guarantee that transaction will be valid in future
    * as soon as state (both UTXO set and state context) will change.
    *
    */
  override def validate(tx: ErgoTransaction): Try[Unit] = validateWithCost(tx, None, Int.MaxValue).map(_ => Unit)

  /**
    *
    * @param fb - ergo full block
    * @return emission box from this block transactions
    */
  protected[state] def extractEmissionBox(fb: ErgoFullBlock): Option[ErgoBox] = emissionBoxIdOpt match {
    case Some(id) =>
      fb.blockTransactions.txs.view.reverse.find(_.inputs.exists(t => java.util.Arrays.equals(t.boxId, id))) match {
        case Some(tx) if tx.outputs.head.ergoTree == constants.settings.chainSettings.monetary.emissionBoxProposition =>
          tx.outputs.headOption
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

  protected def emissionBoxIdOpt: Option[ADKey] = store.get(UtxoState.EmissionBoxIdKey).map(s => ADKey @@ s)

  def emissionBoxOpt: Option[ErgoBox] = emissionBoxIdOpt.flatMap(boxById)

  def boxById(id: ADKey): Option[ErgoBox] = persistentProver.synchronized {
    persistentProver.unauthenticatedLookup(id) flatMap { adValue =>
      ErgoBoxSerializer.parseBytesTry(adValue) match {
        case Failure(e) =>
          log.error(s"Failed to parse box from state, $e")
          None
        case Success(box) =>
          Some(box)
      }
    }
  }

  // used in tests only
  def randomBox(): Option[ErgoBox] = persistentProver.synchronized {
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)
  }

  /**
    * Generate proofs for specified transactions if applied to current state
    *
    * @param txs - transactions to generate proofs
    * @return proof for specified transactions and new state digest
    */
  def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = persistentProver.synchronized {
    val rootHash = persistentProver.digest
    log.trace(s"Going to create proof for ${txs.length} transactions at root ${Algos.encode(rootHash)}")
    if (txs.isEmpty) {
      Failure(new Error("Trying to generate proof for empty transaction sequence"))
    } else if (!storage.version.exists(t => java.util.Arrays.equals(t, rootHash))) {
      Failure(new Error(s"Incorrect storage: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}. " +
        "Possible reason - state update is in process."))
    } else {
      persistentProver.avlProver.generateProofForOperations(ErgoState.stateChanges(txs).operations.map(ADProofs.changeToMod))
    }
  }

  /**
    * Producing a copy of the state which takes into account outputs of given transactions.
    * Useful when checking mempool transactions.
    */
  def withTransactions(txns: Seq[ErgoTransaction]): UtxoState = {
    new UtxoState(persistentProver, version, store, constants) {
      lazy val createdBoxes: Seq[ErgoBox] = txns.flatMap(_.outputs)

      override def boxById(id: ADKey): Option[ErgoBox] = {
        super.boxById(id).orElse(createdBoxes.find(box => box.id.sameElements(id)))
      }
    }
  }

  /**
    * Producing a copy of the state which takes into account pool of unconfirmed transactions.
    * Useful when checking mempool transactions.
    */
  def withMempool(mp: ErgoMemPoolReader): UtxoState = withTransactions(mp.getAll)

}
