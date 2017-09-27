package org.ergoplatform

import akka.actor.ActorRef
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import scorex.core.LocalInterface


class ErgoLocalInterface (override val viewHolderRef: ActorRef)
  extends LocalInterface[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override protected def onStartingPersistentModifierApplication(pmod: ErgoPersistentModifier): Unit = ???

  override protected def onFailedTransaction(tx: AnyoneCanSpendTransaction): Unit = ???

  override protected def onFailedModification(mod: ErgoPersistentModifier): Unit = ???

  override protected def onSuccessfulTransaction(tx: AnyoneCanSpendTransaction): Unit = ???

  override protected def onSuccessfulModification(mod: ErgoPersistentModifier): Unit = ???

  override protected def onNoBetterNeighbour(): Unit = ???

  override protected def onBetterNeighbourAppeared(): Unit = ???
}
