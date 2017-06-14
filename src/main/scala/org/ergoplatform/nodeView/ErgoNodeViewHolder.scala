/*
package org.ergoplatform.nodeView

import org.ergoplatform.modifiers._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.{NodeViewHolder, NodeViewModifier}
import scorex.crypto.encode.Base58


class ErgoNodeViewHolder(settings: Settings) extends NodeViewHolder[PublicKey25519Proposition,
  AnyoneCanSpendTransaction,
  ErgoBlock] {
  override val networkChunkSize: Int = settings.networkChunkSize

  override type SI = ErgoSyncInfo

  override type HIS = ErgoHistory
  override type MS = ErgoState
  override type VL = ErgoWallet
  override type MP = ErgoMemPool

  override val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(ErgoHeader.ModifierTypeId -> ErgoHeaderSerializer,
      Transaction.ModifierTypeId -> AnyoneCanSpendTransactionSerializer)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  override protected def genesisState: (HIS, MS, VL, MP) = ???

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[(HIS, MS, VL, MP)] = ???
}*/
