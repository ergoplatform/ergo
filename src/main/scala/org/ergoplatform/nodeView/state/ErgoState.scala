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
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: MinimalState[AnyoneCanSpendProposition,
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
  def rootHash(): Array[Byte]

  //TODO implement correctly
  def stateHeight: Int = 0

  override def version: VersionTag

  override def validate(mod: ErgoPersistentModifier): Try[Unit]

  override def applyModifier(mod: ErgoPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  override type NVCT = this.type
}

object ErgoState {

  type Digest = Array[Byte]

  def readOrGenerate(settings: ErgoSettings) =
    if(settings.ADState) new DigestState else new UtxoState

  //Initial state even before genesis block application
  val initialState: UtxoState = new UtxoState
}
