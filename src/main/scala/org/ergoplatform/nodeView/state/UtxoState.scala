package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendNoncedBoxSerializer, AnyoneCanSpendProposition}
import scorex.core.transaction.state.BoxStateChanges
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Success, Try}


class UtxoState(override val rootHash: Array[Byte]) extends ErgoState[UtxoState] {

  implicit val hf = new Blake2b256Unsafe

  private val dir = new File("/tmp/utxo")
  dir.mkdirs()

  private val store = new LSMStore(dir)
  private val storage = new VersionedIODBAVLStorage(store, NodeParameters(keySize = 32, valueSize = 48, labelSize = 32))

  private val prover = new BatchAVLProver(keyLength = 32, valueLengthOpt = Some(48))

  private val persistentProver = new PersistentBatchAVLProver(prover, storage)

  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(new AnyoneCanSpendProposition, height, height))
  }

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): ADProof.ProofRepresentation =
    txs.flatMap(_.id).toArray

  override def version: VersionTag = ???

  override def rollbackTo(version: VersionTag): Try[UtxoState] = ???

  override def validate(mod: ErgoPersistentModifier): Try[Unit] = ???

  override def applyModifier(mod: ErgoPersistentModifier): Try[UtxoState] = ???
}
