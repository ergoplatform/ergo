package org.ergoplatform.nodeView.state

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import scorex.core.transaction.state.BoxStateChanges
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.crypto.authds.avltree.batch.VersionedIODBAVLStorage
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.Try


class UtxoState extends ErgoState[UtxoState] with BoxMinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  UtxoState] {

  implicit val hf = new Blake2b256Unsafe

  val dir = new File("/tmp/utxo")
  dir.mkdirs()

  val store = new LSMStore(dir)
  val tree = new VersionedIODBAVLStorage(store, keySize = 32, valueSize = 48)


  /**
    * @return boxes, that miner (or any user) can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    IndexedSeq(AnyoneCanSpendNoncedBox(new AnyoneCanSpendProposition, height, height))
  }

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): ADProof.ProofRepresentation =
    txs.flatMap(_.id).toArray

  override def closedBox(boxId: Array[Byte]): Option[AnyoneCanSpendNoncedBox] =
    tree.

  override def boxesOf(proposition: AnyoneCanSpendProposition): Seq[AnyoneCanSpendNoncedBox] = ???

  override def changes(mod: ErgoPersistentModifier): Try[BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = ???

  override def applyChanges(changes: BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox], newVersion: VersionTag): Try[UtxoState] = ???

  override def semanticValidity(tx: AnyoneCanSpendTransaction): Try[Unit] = ???

  override def rootHash(): Array[Byte] = ???

  override def version: VersionTag = ???

  override def rollbackTo(version: VersionTag): Try[UtxoState] = ???
}
