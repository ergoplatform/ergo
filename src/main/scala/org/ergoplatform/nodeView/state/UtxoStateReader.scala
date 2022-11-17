package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import scorex.core.transaction.state.TransactionValidation
import scorex.core.transaction.state.TransactionValidation.TooHighCostError
import scorex.core.validation.MalformedModifierError
import scorex.crypto.authds.avltree.batch.{Lookup, NodeParameters, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

trait UtxoStateReader extends ErgoStateReader with TransactionValidation {

  protected implicit val hf: HF = Algos.hash

  val constants: StateConstants

  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedLDBAVLStorage(store, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, HF]

  def generateBatchProofForBoxes(boxes: Seq[ErgoBox.BoxId]): SerializedAdProof = persistentProver.synchronized {
    boxes.map { box => persistentProver.performOneOperation(Lookup(ADKey @@@ box)) }
    persistentProver.prover().generateProof()
  }

  /**
    * Validate transaction against provided state context, if specified,
    * or state context from the previous block if not
    */
  def validateWithCost(tx: ErgoTransaction,
                       stateContextOpt: Option[ErgoStateContext],
                       costLimit: Int,
                       interpreterOpt: Option[ErgoInterpreter]): Try[Int] = {
    val context = stateContextOpt.getOrElse(stateContext)
    val parameters = context.currentParameters.withBlockCost(costLimit)
    val verifier = interpreterOpt.getOrElse(ErgoInterpreter(parameters))

    tx.statelessValidity().flatMap { _ =>
      val boxesToSpend = tx.inputs.flatMap(i => boxById(i.boxId))
      tx.statefulValidity(
        boxesToSpend,
        tx.dataInputs.flatMap(i => boxById(i.boxId)),
        context,
        accumulatedCost = 0L)(verifier) match {
        case Success(txCost) if txCost > costLimit =>
          Failure(TooHighCostError(s"Transaction $tx has too high cost $txCost"))
        case Success(txCost) =>
          Success(txCost)
        case Failure(mme: MalformedModifierError) if mme.message.contains("CostLimitException") =>
          Failure(TooHighCostError(s"Transaction $tx has too high cost"))
        case f: Failure[_] => f
      }
    }
  }

  /**
    * Validate transaction as if it was included at the end of the last block.
    * This validation does not guarantee that transaction will be valid in future
    * as soon as state (both UTXO set and state context) will change.
    *
    * Used in mempool.
    */
  override def validateWithCost(tx: ErgoTransaction, maxTxCost: Int): Try[Int] = {
    validateWithCost(tx, None, maxTxCost, None)
  }

  /**
    *
    * @param fb - ergo full block
    * @return emission box from this block transactions
    */
  protected[state] def extractEmissionBox(fb: ErgoFullBlock): Option[ErgoBox] = {
    def hasEmissionBox(tx: ErgoTransaction): Boolean =
      if(fb.height > constants.settings.chainSettings.reemission.activationHeight) {
        // after EIP-27 we search for emission box NFT for efficiency's sake
        tx.outputs.size == 2 &&
          !tx.outputs.head.additionalTokens.isEmpty &&
          java.util.Arrays.equals(tx.outputs.head.additionalTokens(0)._1, constants.settings.chainSettings.reemission.emissionNftIdBytes)
      } else {
        tx.outputs.head.ergoTree == constants.settings.chainSettings.monetary.emissionBoxProposition
      }

    def fullSearch(fb: ErgoFullBlock): Option[ErgoBox] = {
      fb.transactions
        .find(hasEmissionBox)
        .map(_.outputs.head)
        .filter(_.value > 100000 * EmissionRules.CoinsInOneErgo) // to filter out possible spam
    }

    emissionBoxIdOpt match {
      case Some(id) =>
        fb.blockTransactions.txs.view.reverse.find(_.inputs.exists(t => java.util.Arrays.equals(t.boxId, id))) match {
          case Some(tx) if hasEmissionBox(tx) =>
            tx.outputs.headOption
          case Some(_) =>
            log.info(s"Last possible emission box consumed")
            None
          case None =>
            log.warn(s"Emission box possibly not spent in block ${fb.encodedId}")
            boxById(id) match {
              case s: Some[ErgoBox] => s
              case None => fullSearch(fb)
            }

        }
      case None =>
        log.debug("No emission box: emission should be already finished before this block")
        fullSearch(fb)
    }
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
      ErgoState.stateChanges(txs).flatMap { stateChanges =>
        persistentProver.avlProver.generateProofForOperations(stateChanges.operations)
      }
    }
  }

  /**
    * Producing a copy of the state which takes into account outputs of given transactions.
    * Useful when checking mempool transactions.
    */
  def withUnconfirmedTransactions(unconfirmedTxs: Seq[UnconfirmedTransaction]): UtxoState = {
    new UtxoState(persistentProver, version, store, constants) {
      lazy val createdBoxes: Seq[ErgoBox] = unconfirmedTxs.map(_.transaction).flatMap(_.outputs)

      override def boxById(id: ADKey): Option[ErgoBox] = {
        super.boxById(id).orElse(createdBoxes.find(box => box.id.sameElements(id)))
      }
    }
  }

  /**
   * Producing a copy of the state which takes into account outputs of given transactions.
   * Useful when checking mempool transactions.
   */
  def withTransactions(transactions: Seq[ErgoTransaction]): UtxoState = {
    new UtxoState(persistentProver, version, store, constants) {
      lazy val createdBoxes: Seq[ErgoBox] = transactions.flatMap(_.outputs)

      override def boxById(id: ADKey): Option[ErgoBox] = {
        super.boxById(id).orElse(createdBoxes.find(box => box.id.sameElements(id)))
      }
    }
  }

  /**
    * Producing a copy of the state which takes into account pool of unconfirmed transactions.
    * Useful when checking mempool transactions.
    */
  def withMempool(mp: ErgoMemPoolReader): UtxoState = withUnconfirmedTransactions(mp.getAll)

}
