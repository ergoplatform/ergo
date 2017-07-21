package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.utils.ScorexLogging

import scala.util.Try

class ErgoState extends BoxMinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  ErgoState] with ScorexLogging {

  /**
    * @return boxes, that miner can take to himself when he creates a new block
    */
  def anyoneCanSpendBoxesAtHeight(height: Int): IndexedSeq[AnyoneCanSpendNoncedBox] = {
    //TODO implement correctly
    IndexedSeq(AnyoneCanSpendNoncedBox(new AnyoneCanSpendProposition, height, height))
  }

  //TODO implement correctly
  def rootHash(): Array[Byte] = Algos.emptyMerkleTreeRoot

  //TODO implement correctly
  def proofsForTransactions(txs: Seq[AnyoneCanSpendTransaction]): Array[Byte] = txs.flatMap(_.id).toArray

  //TODO implement correctly
  def stateHeight: Int = 0

  override def semanticValidity(tx: AnyoneCanSpendTransaction): Try[Unit] = ???

  override def version: VersionTag = ???

  override def closedBox(boxId: Array[Byte]): Option[AnyoneCanSpendNoncedBox] = ???

  override def boxesOf(proposition: AnyoneCanSpendProposition): Seq[AnyoneCanSpendNoncedBox] = ???

  override def changes(mod: ErgoPersistentModifier): Try[StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = ???

  override def applyChanges(changes: StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox], newVersion: VersionTag): Try[ErgoState] = ???

  override def rollbackTo(version: VersionTag): Try[ErgoState] = ???

  override type NVCT = this.type
}

object ErgoState {

  def readOrGenerate(settings: ErgoSettings): ErgoState = new ErgoState

  //Initial state even before genesis block application
  val initialState: ErgoState = new ErgoState
}
