package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.utils.ScorexLogging

import scala.util.Try


/**
  * State in Ergo could be UTXO or just a single digest.
  */
class ErgoState[IState <: MinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState]] extends MinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  IState] with ScorexLogging {

  self: IState =>

  //TODO: kushti: AVL+ root, not Merkle
  def rootHash(): Array[Byte] = Algos.emptyMerkleTreeRoot

  //TODO implement correctly
  def stateHeight: Int = 0

  override def version: VersionTag = ???

  override def validate(mod: ErgoPersistentModifier): Try[Unit] = ???

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState] = ???

  override def rollbackTo(version: VersionTag): Try[IState] = ???

  override type NVCT = this.type
}

object ErgoState {

  def readOrGenerate(settings: ErgoSettings) =
    if(settings.ADState) new DigestState else new UtxoState

  //Initial state even before genesis block application
  val initialState: UtxoState = new UtxoState
}
