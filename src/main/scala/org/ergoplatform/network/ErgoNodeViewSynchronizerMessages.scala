package org.ergoplatform.network

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.nodeView.history._
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import scorex.core.network.ConnectedPeer
import scorex.util.ModifierId
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.subblocks.InputBlockInfo

/**
  * Repository of messages processed ErgoNodeViewSynchronizer actor
  */
object ErgoNodeViewSynchronizerMessages {
  /**
    * Signal which is instructing ErgoNodeViewSynchronizer to send sync message to peers (when it is needed)
    */
  case object SendLocalSyncInfo

    /**
     * Check delivery of modifier with type `modifierTypeId` and id `modifierId`.
     * `source` may be defined if we expect modifier from concrete peer or None if
     * we just need some modifier, but don't know who have it
     *
     */
    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: NetworkObjectTypeId.Value,
                             modifierId: ModifierId)

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(peer: ConnectedPeer) extends PeerManagerEvent

    trait NodeViewHolderEvent

    sealed trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory(reader: ErgoHistoryReader) extends NodeViewChange

    case class ChangedMempool(mempool: ErgoMemPoolReader) extends NodeViewChange

    case class ChangedVault(reader: ErgoWalletReader) extends NodeViewChange

    case class ChangedState(reader: ErgoStateReader) extends NodeViewChange

    case class NewBestInputBlock(id: Option[ModifierId]) extends NodeViewChange

    /**
     * Event which is published when rollback happened (on finding a better chain)
     *
     * @param branchPoint - block id which is last in the chain after rollback (before applying blocks from a fork)
     */
    case class Rollback(branchPoint: ModifierId) extends NodeViewHolderEvent

    case object RollbackFailed extends NodeViewHolderEvent

    // hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    trait InitialTransactionCheckOutcome extends ModificationOutcome {
      val transaction: UnconfirmedTransaction
    }

    case class FailedTransaction(transaction: UnconfirmedTransaction, error: Throwable) extends InitialTransactionCheckOutcome

    case class SuccessfulTransaction(transaction: UnconfirmedTransaction) extends InitialTransactionCheckOutcome

    /**
     * Transaction declined by the mempool (not permanently invalidated, so pool can accept it in future)
     */
    case class DeclinedTransaction(transaction: UnconfirmedTransaction) extends InitialTransactionCheckOutcome

    /**
     * Transaction which was failed not immediately but after sitting for some time in the mempool or during block
     * candidate generation
     */
    case class FailedOnRecheckTransaction(id: ModifierId, error: Throwable) extends ModificationOutcome

    /**
     * A signal that block section with id `modifierId` was invalidated due to `error`, but it may be valid in future
     */
    case class RecoverableFailedModification(typeId: NetworkObjectTypeId.Value, modifierId: ModifierId, error: Throwable) extends ModificationOutcome

    /**
     * A signal that block section with id `modifierId` was permanently invalidated during stateless checks
     */
    case class SyntacticallyFailedModification(typeId: NetworkObjectTypeId.Value, modifierId: ModifierId, error: Throwable) extends ModificationOutcome

    /**
     * Signal associated with stateful validation of a block section
     */
    case class SemanticallyFailedModification(typeId: NetworkObjectTypeId.Value, modifierId: ModifierId, error: Throwable) extends ModificationOutcome

    /**
     * Signal associated with stateless validation of a block section
     */
    case class SyntacticallySuccessfulModifier(typeId: NetworkObjectTypeId.Value, modifierId: ModifierId) extends ModificationOutcome

    /**
     * Signal sent by node view holder when a full block is applied to state
     *
     * @param header - full block's header
     */
    case class FullBlockApplied(header: Header) extends ModificationOutcome

    /**
     * Signal sent after block sections processing (validation and application to state) done
     *
     * @param headersCacheSize       - headers cache size after processing
     * @param blockSectionsCacheSize - block sections cache size after processing
     * @param cleared                - blocks removed from cache being overfull
     */
    case class BlockSectionsProcessingCacheUpdate(headersCacheSize: Int,
                                                  blockSectionsCacheSize: Int,
                                                  cleared: (NetworkObjectTypeId.Value, Seq[ModifierId]))

    /**
     * Command to re-check mempool to clean transactions become invalid while sitting in the mempool up
     *
     * @param state   - up-to-date state to check transaction against
     * @param mempool - mempool to check
     */
    case class RecheckMempool(state: UtxoStateReader, mempool: ErgoMemPoolReader)

    /**
     * Signal for a central node view holder component to initialize UTXO state from UTXO set snapshot
     * stored in the local database
     *
     * @param blockHeight - height of a block corresponding to the UTXO set snapshot
     * @param blockId     - id of a block corresponding to the UTXO set snapshot
     */
    case class InitStateFromSnapshot(blockHeight: Height, blockId: ModifierId)

  /**
    * Command for a central node view holder component to process NiPoPoW proof,
    * and possibly initialize headers chain from a best NiPoPoW proof known, when enough proofs collected
    *
    * @param nipopowProof - proof to initialize history from
    */
  case class ProcessNipopow(nipopowProof: NipopowProof)

  case class ProcessInputBlock(subblock: InputBlockInfo, remote: ConnectedPeer)

  case class ProcessInputBlockTransactions(std: InputBlockTransactionsData)
}
