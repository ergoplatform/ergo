package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.utils.ScorexLogging

import scala.util.Try

class ErgoState extends BoxMinimalState[AnyoneCanSpendProposition,
  AnyoneCanSpendNoncedBox,
  AnyoneCanSpendTransaction,
  ErgoBlock,
  ErgoState] with ScorexLogging {

  override def semanticValidity(tx: AnyoneCanSpendTransaction): Try[Unit] = ???

  override def version: VersionTag = ???

  override def closedBox(boxId: Array[Byte]): Option[AnyoneCanSpendNoncedBox] = ???

  override def boxesOf(proposition: AnyoneCanSpendProposition): Seq[AnyoneCanSpendNoncedBox] = ???

  override def changes(mod: ErgoBlock): Try[StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = ???

  override def applyChanges(changes: StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox], newVersion: VersionTag): Try[ErgoState] = ???

  override def rollbackTo(version: VersionTag): Try[ErgoState] = ???

  override type NVCT = this.type
}
