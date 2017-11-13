package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import scorex.core.network.NodeViewSynchronizer
import scorex.core.settings.NetworkSettings

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               localInterfaceRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings) extends NodeViewSynchronizer[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction,
  ErgoSyncInfo, ErgoSyncInfoMessageSpec.type](networkControllerRef, viewHolderRef, localInterfaceRef,
  syncInfoSpec, networkSettings){

}
