package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import scorex.core.NodeViewHolder._
import scorex.core.network.NodeViewSynchronizer
import scorex.core.settings.NetworkSettings

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               localInterfaceRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings) extends NodeViewSynchronizer[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction,
  ErgoSyncInfo, ErgoSyncInfoMessageSpec.type](networkControllerRef, viewHolderRef, localInterfaceRef,
  syncInfoSpec, networkSettings) {
  override protected val deliveryTracker = new ErgoDeliveryTracker

  protected val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) =>
      mod.toSeq.foreach(m => broadcastModifierInv(m))
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
  }

  override protected def viewHolderEvents: Receive = onSemanticallySuccessfulModifier orElse super.viewHolderEvents

}
