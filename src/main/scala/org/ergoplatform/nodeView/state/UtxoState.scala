package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import scorex.core.transaction.state.BoxStateChanges
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState

import scala.util.Try


class UtxoState extends ErgoState[UtxoState] with BoxMinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  UtxoState] {

  /**
    * @return boxes, that miner can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    //TODO: kushti:  if state is not about UTXO, it is not possible to extract this info even
    IndexedSeq(AnyoneCanSpendNoncedBox(new AnyoneCanSpendProposition, height, height))
  }

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): ADProof.ProofRepresentation =
    txs.flatMap(_.id).toArray

  override def closedBox(boxId: Array[Byte]): Option[AnyoneCanSpendNoncedBox] = ???

  override def boxesOf(proposition: AnyoneCanSpendProposition): Seq[AnyoneCanSpendNoncedBox] = ???

  override def changes(mod: ErgoPersistentModifier): Try[BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = ???

  override def applyChanges(changes: BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox], newVersion: VersionTag): Try[UtxoState] = ???

  override def semanticValidity(tx: AnyoneCanSpendTransaction): Try[Unit] = ???
}
