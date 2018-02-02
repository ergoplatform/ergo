package org.ergoplatform.local

import akka.actor.ActorRef
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.ErgoSettings
import scorex.core.{LocalInterface, ModifierId, VersionTag}


class ErgoLocalInterface (override val viewHolderRef: ActorRef)
  extends LocalInterface[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override protected def onStartingPersistentModifierApplication(pmod: ErgoPersistentModifier): Unit = {}

  override protected def onFailedTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onSuccessfulTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}
}
