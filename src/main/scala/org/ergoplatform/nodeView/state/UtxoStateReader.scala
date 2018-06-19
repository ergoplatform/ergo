package org.ergoplatform.nodeView.state

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoStateContext, ErgoTransaction}
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Algos.HF
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

trait UtxoStateReader extends ErgoStateReader with ScorexLogging with TransactionValidation[ErgoTransaction] {

  protected implicit val hf = Algos.hash

  val constants: StateConstants
  val store: Store
  private lazy val np = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)


  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
    val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    PersistentBatchAVLProver.create(bp, storage).get
  }

  override def validate(tx: ErgoTransaction): Try[Unit] = tx.statelessValidity
    .flatMap(_ => tx.statefulValidity(tx.inputs.flatMap(i => boxById(i.boxId)), stateContext()).map(_ => Unit))

  /**
    * Extract emission box from transactions and save it to emissionBoxOpt
    *
    * @param fb - ergo full block
    */
  def extractEmissionBox(fb: ErgoFullBlock): Option[ErgoBox] = {
    val coinsAtHeight = constants.emission.remainingCoinsAfterHeight(fb.header.height)
    fb.blockTransactions.txs.reverse.flatMap(_.outputs)
      .find(o => o.value == coinsAtHeight && o.proposition == constants.genesisEmissionBox.proposition) match {
      case Some(newEmissionBox) => Some(newEmissionBox)
      case _ =>
        log.warn(s"Emission box not found in block ${fb.encodedId}")
        None
    }
  }

  // TODO implement
  def stateContext(): ErgoStateContext = ErgoStateContext(0, rootHash)

  def emissionBox(): Option[ErgoBox] = store.get(ByteArrayWrapper(UtxoState.EmissionBoxKey))
    .flatMap(b => ErgoBoxSerializer.parseBytes(b.data).toOption)

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
    } else if (!storage.version.exists(_.sameElements(rootHash))) {
      Failure(new Error(s"Incorrect storage: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}. " +
        s"Possible reason - state update is in process."))
    } else {
      persistentProver.avlProver.generateProofForOperations(ErgoState.stateChanges(txs).operations.map(ADProofs.changeToMod))
    }
  }
}
