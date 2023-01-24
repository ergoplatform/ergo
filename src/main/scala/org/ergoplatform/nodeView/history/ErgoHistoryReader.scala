package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.{Header, PreGenesisHeader}
import org.ergoplatform.modifiers.history.popow.{NipopowAlgos, NipopowProof, PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NetworkObjectTypeId, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewComponent
import scorex.core.consensus.{ContainsModifiers, Equal, Fork, ModifierSemanticValidity, Older, PeerChainStatus, Unknown, Younger}
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.MalformedModifierError
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Read-only copy of ErgoHistory
  */
trait ErgoHistoryReader
  extends NodeViewComponent
    with ContainsModifiers[BlockSection]
    with HeadersProcessor
    with BlockSectionProcessor
    with ScorexLogging
    with ScorexEncoding {

  type ModifierIds = Seq[(NetworkObjectTypeId.Value, ModifierId)]

  protected[history] val historyStorage: HistoryStorage

  protected val settings: ErgoSettings

  private val Valid = 1.toByte
  private val Invalid = 0.toByte

  /**
    * True if there's no history, even genesis block
    */
  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet (from a chain or a PoPoW proof).
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it), call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for fullnode regime after initial bootstrap.
    */
  def bestFullBlockOpt: Option[ErgoFullBlock] =
    bestFullBlockIdOpt.flatMap(id => typedModifierById[Header](id)).flatMap(getFullBlock)

  /**
    * @param id - modifier id
    * @return raw bytes of semantically valid ErgoPersistentModifier with the given id it is in history
    */
  def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
    if (isSemanticallyValid(id) != ModifierSemanticValidity.Invalid) {
      historyStorage.modifierBytesById(id)
    } else {
      None
    }

  /**
    * @param id - modifier id
    * @return type and raw bytes of semantically valid ErgoPersistentModifier with the given id it is in history
    */
   def modifierTypeAndBytesById(id: ModifierId): Option[(NetworkObjectTypeId.Value, Array[Byte])] =
    if (isSemanticallyValid(id) != ModifierSemanticValidity.Invalid) {
      historyStorage.modifierTypeAndBytesById(id)
    } else {
      None
    }

  /**
    * @param id - modifier id
    * @return semantically valid ErgoPersistentModifier with the given id it is in history
    */
  override def modifierById(id: ModifierId): Option[BlockSection] =
    if (isSemanticallyValid(id) != ModifierSemanticValidity.Invalid) {
      historyStorage.modifierById(id)
    } else {
      None
    }

  /** Get modifier of expected type by its identifier
    *
    * @param id - modifier id
    * @tparam T - expected Type
    * @return semantically valid ErgoPersistentModifier of type T with the given id it is in history
    */
  def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T) => Some(m)
    case _ => None
  }

  override def contains(id: ModifierId): Boolean = historyStorage.contains(id)

  /**
    * Check, that it's possible to apply modifier to history
    */
  def applicable(modifier: BlockSection): Boolean = applicableTry(modifier).isSuccess

  /**
    * For given headers (sorted in reverse chronological order), find first one (most recent one) which is known
    * to our history
    */
  private def commonPoint(headers: Seq[Header]): Option[Header] = {
    headers.find { h =>
      contains(h.id)
    }
  }

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead,
    *         Fork if other peer is on another chain, Unknown if we can't deduct neighbour's status
    */
  def compare(info: ErgoSyncInfo): PeerChainStatus = {
    info match {
      case syncV1: ErgoSyncInfoV1 =>
        compareV1(syncV1)
      case syncV2: ErgoSyncInfoV2 =>
        compareV2(syncV2)
    }
  }

  /**
    * Whether another's node syncinfo indicates that another node is ahead or behind ours, or on fork
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history,
    *         Younger if another node is behind,
    *         Older if the neighbour is ahead,
    *         Fork if the neighbour is on a fork
    */
  def compareV2(info: ErgoSyncInfoV2): PeerChainStatus = {
    bestHeaderOpt.map { myLastHeader =>
      if (info.lastHeaders.isEmpty) {
        Younger
      } else {
        val myHeight = myLastHeader.height

        val otherHeaders = info.lastHeaders
        val otherLastHeader = otherHeaders.head // always available
        val otherHeight = otherLastHeader.height

        if (otherHeight == myHeight) {
          if (otherLastHeader.id == myLastHeader.id) {
            // Last headers are the same => chains are equal
            Equal
          } else {
            // todo: check PoW of otherLastHeader
            if (commonPoint(otherHeaders.tail).isDefined) {
              Fork
            } else {
              Unknown
            }
          }
        } else if (otherHeight > myHeight) {
          Older // todo: check difficulty ?
        } else { // otherHeight < myHeight
          Younger //todo: check if the block is on my chain?
        }
      }
    }.getOrElse {
      if (info.lastHeaders.isEmpty) {
        Equal
      } else {
        Older
      }
    } // other peer is older if the node doesn't have any header yet
  }

  /**
    * Whether another's node syncinfo indicates that another node is ahead or behind ours
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history,
    *         Younger if another node is behind,
    *         Older if the neighbour is ahead,
    *         Fork if the neighbour is on a fork
    */
  def compareV1(info: ErgoSyncInfoV1): PeerChainStatus = {
    bestHeaderIdOpt match {
      case Some(id) if info.lastHeaderIds.lastOption.contains(id) =>
        //Our best header is the same as other node best header
        Equal
      case Some(id) if info.lastHeaderIds.contains(id) =>
        //Our best header is in other node best chain, but not at the last position
        Older
      case Some(_) if info.lastHeaderIds.isEmpty =>
        //Other history is empty, our contain some headers
        Younger
      case Some(_) =>
        if (info.lastHeaderIds.view.reverse.exists(m => contains(m) || m == PreGenesisHeader.id)) {
          //We are on different forks now.
          Fork
        } else {
          //We don't have any of id's from other's node sync info in history.
          //We don't know whether we can sync with it and what blocks to send in Inv message.
          //Assume it is older and far ahead from us
          Older
        }
      case None if info.lastHeaderIds.isEmpty =>
        //Both nodes do not keep any blocks
        Equal
      case None =>
        //Our history is empty, other contain some headers
        Older
    }
  }

  /**
    *
    * Calculating continuation from common header which will be sent to another node
    * if comparison status is YOUNGER or FORK, for sync message V1.
    */
  def continuationIdsV1(syncInfo: ErgoSyncInfoV1, size: Int): ModifierIds =
    if (isEmpty) {
      // if no any header applied yet, return identifiers from other node's sync info
      syncInfo.lastHeaderIds.map(b => Header.modifierTypeId -> b)
    } else if (syncInfo.lastHeaderIds.isEmpty) {
      // if other node has no headers yet, send up to `size` headers from genesis
      val heightTo = Math.min(headersHeight, size + ErgoHistory.EmptyHistoryHeight)
      (ErgoHistory.GenesisHeight to heightTo).flatMap { height =>
        bestHeaderIdAtHeight(height).map(id => Header.modifierTypeId -> id)
      }
    } else {
      // else, find common header with the other node and send up to `size` headers from it (including the point)
      val ids = syncInfo.lastHeaderIds
      val branchingPointOpt: Option[ModifierId] = ids.view.reverse
        .find(m => isInBestChain(m))
        .orElse(if (ids.contains(PreGenesisHeader.id)) Some(PreGenesisHeader.id) else None)
      branchingPointOpt.toSeq.flatMap { branchingPoint =>
        val otherNodeHeight = heightOf(branchingPoint).getOrElse(ErgoHistory.GenesisHeight)
        val heightTo = Math.min(headersHeight, otherNodeHeight + size - 1)
        (otherNodeHeight to heightTo).flatMap { height =>
          bestHeaderIdAtHeight(height).map(id => Header.modifierTypeId -> id)
        }
      }
    }

  /**
    *
    * Calculating continuation from common header which will be sent to another node
    * if comparison status is YOUNGER of FORK, for sync message V2.
    */
  def continuationIdsV2(syncV2: ErgoSyncInfoV2, size: Int): ModifierIds = {
    if (syncV2.lastHeaders.isEmpty) {
      // if other node has no headers yet, send up to `size` headers from genesis
      val heightTo = Math.min(headersHeight, size + ErgoHistory.EmptyHistoryHeight)
      (ErgoHistory.GenesisHeight to heightTo)
        .flatMap(height => bestHeaderIdAtHeight(height))
        .map(h => Header.modifierTypeId -> h) //todo: remove modifierTypeId ?
    } else {
      commonPoint(syncV2.lastHeaders) match {
        case Some(commonHeader) =>
          val heightTo = Math.min(headersHeight, commonHeader.height + size - 1)
          ((commonHeader.height + 1) to heightTo)
            .flatMap(height => bestHeaderIdAtHeight(height))
            .map(h => Header.modifierTypeId -> h) //todo: remove modifierTypeId ?
        case None =>
          Seq.empty
      }
    }
  }

  /**
    * Finding other peer's continuation header from a header that is common to our node's history
    * @param syncInfo  other's node sync info
    * @return maybe continuation header
    */
  def continuationHeaderV2(syncInfo: ErgoSyncInfoV2): Option[Header] = {
    if (syncInfo.lastHeaders.isEmpty) {
      Option.empty
    } else {
      val lastHeader = syncInfo.lastHeaders.head
      // let's find continuation header whose parent is our bestHeader
      if (bestHeaderIdOpt.contains(lastHeader.parentId)) {
        Some(lastHeader)
      } else {
        Option.empty
      }
    }
  }

  /**
    * Calculating continuation from common header which will be sent to another node
    * if comparison status is YOUNGER or FORK.
    *
    * @param syncInfo other's node sync info
    * @param size max return size
    * @return Ids of headers, that node with info should download and apply to synchronize
    */
  def continuationIds(syncInfo: ErgoSyncInfo, size: Int): ModifierIds = {
    syncInfo match {
      case syncV1: ErgoSyncInfoV1 => continuationIdsV1(syncV1, size)
      case syncV2: ErgoSyncInfoV2 => continuationIdsV2(syncV2, size)
    }
  }

  /**
    *
    * @param header     - header to start
    * @param withFilter - condition to satisfy
    * @return all possible forks, starting from specified header and satisfying withFilter condition
    */
  protected[history] def continuationHeaderChains(header: Header, withFilter: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec
    def loop(currentHeight: Option[Int], acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextLevelHeaders = currentHeight.toList
        .flatMap { h => headerIdsAtHeight(h + 1) }
        .flatMap { id => typedModifierById[Header](id) }
        .filter(withFilter)
      if (nextLevelHeaders.isEmpty) {
        acc.map(_.reverse)
      } else {
        val updatedChains = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId == chain.head.id)).map(c => h +: c)
        }
        val nonUpdatedChains = acc.filter(chain => !nextLevelHeaders.exists(_.parentId == chain.head.id))
        loop(currentHeight.map(_ + 1), updatedChains ++ nonUpdatedChains)
      }
    }

    loop(heightOf(header.id), Seq(Seq(header)))
  }

  /**
    * Information about our node synchronization status. Other node should be able to compare it's view with ours by
    * this syncInfo message and calculate modifiers missed by our node.
    *
    * V1 version (last header ids to be sent)
    *
    * @return
    */
  def syncInfoV1: ErgoSyncInfoV1 = {
    /**
      * Return last count headers from best headers chain if exist or chain up to genesis otherwise
      */
    def lastHeaderIds(count: Int): IndexedSeq[ModifierId] = {
      val currentHeight = headersHeight
      val from = Math.max(currentHeight - count + 1, 1)
      val res = (from to currentHeight).flatMap{h =>
        bestHeaderIdAtHeight(h)
      }
      if(from == 1) {
        PreGenesisHeader.id +: res
      } else {
        res
      }
    }

    if (isEmpty) {
      ErgoSyncInfoV1(Nil)
    } else {
      ErgoSyncInfoV1(lastHeaderIds(ErgoSyncInfo.MaxBlockIds))
    }
  }


  /**
    * @return sync info for neigbour peers, V2 message
    * @param full - if false, only last header to be sent, otherwise, multiple headers
    *               full info is needed when
    */
  def syncInfoV2(full: Boolean): ErgoSyncInfoV2 = {
    if (isEmpty) {
      ErgoSyncInfoV2(Nil)
    } else {
      val h = headersHeight

      val offsets = if (full) {
        ErgoHistoryReader.FullV2SyncOffsets
      } else {
        ErgoHistoryReader.ReducedV2SyncOffsets
      }

      val headers = offsets.flatMap(offset => bestHeaderAtHeight(h - offset))

      ErgoSyncInfoV2(headers)
    }
  }

  /**
    * Return last count headers from best headers chain if exist or chain up to genesis otherwise
    */
  def lastHeaders(count: Int, offset: Int = 0): HeaderChain = {
    bestHeaderOpt
      .map(bestHeader => headerChainBack(count, bestHeader, _ => false).drop(offset))
      .getOrElse(HeaderChain.empty)
  }

  /**
    * @return ids of headers (max. limit) starting from offset
    */
  def headerIdsAt(offset: Int, limit: Int): Seq[ModifierId] = {
    (offset until (limit + offset)).flatMap(height => bestHeaderIdAtHeight(height))
  }

  /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier  - modifier to apply
    * @return `Success` if modifier can be applied, `Failure(ModifierError)` if can not
    */
  def applicableTry(modifier: BlockSection): Try[Unit] = {
    modifier match {
      case header: Header =>
        validate(header)
      case m: NonHeaderBlockSection =>
        validate(m)
      case m: Any =>
        Failure(new MalformedModifierError(s"Modifier $m has incorrect type", modifier.id, modifier.modifierTypeId))
    }
  }

  def getFullBlock(header: Header): Option[ErgoFullBlock] = {
    (typedModifierById[BlockTransactions](header.transactionsId),
      typedModifierById[Extension](header.extensionId),
      typedModifierById[ADProofs](header.ADProofsId)) match {
      case (Some(txs), Some(ext), Some(proofs)) => Some(ErgoFullBlock(header, txs, ext, Some(proofs)))
      case (Some(txs), Some(ext), None) if !nodeSettings.stateType.requireProofs =>
        Some(ErgoFullBlock(header, txs, ext, None))
      case _ => None
    }
  }

  /**
    * Returns full block from a best headers-chain at given height
    * @param height - height to get the full block from
    * @return - full block or None if there's no such a block at given height
    */
  def bestFullBlockAt(height: Height): Option[ErgoFullBlock] = {
    bestHeaderIdAtHeight(height)
      .flatMap(headerId => typedModifierById[Header](headerId))
      .flatMap(header => getFullBlock(header))
  }


  /**
    * Returns block transactions from a best headers-chain at given height
    * @param height - height to get the block transactions from
    * @return - block transactions or None if there's no such a block at given height
    */
  def bestBlockTransactionsAt(height: Height): Option[BlockTransactions] = {
    bestHeaderIdAtHeight(height)
      .flatMap(headerId => typedModifierById[Header](headerId))
      .flatMap(header => typedModifierById[BlockTransactions](header.transactionsId))
  }

  /**
    * Return headers, required to apply to reach header2 if you are at header1 position.
    *
    * @param startHeaderOpt - initial position
    * @param finalHeader    - header you should reach
    * @return (Modifier it required to rollback first, header chain to apply)
    */
  def chainToHeader(startHeaderOpt: Option[Header], finalHeader: Header): (Option[ModifierId], HeaderChain) = {
    startHeaderOpt match {
      case Some(h1) =>
        val (prevChain, newChain) = commonBlockThenSuffixes(h1, finalHeader)
        (prevChain.headOption.map(_.id), newChain.tail)
      case None =>
        (None, headerChainBack(finalHeader.height + 1, finalHeader, _ => false))
    }
  }

  /**
    * Find common block and subchains from common block to header1 and header2
    *
    * @param header1 : Header - header in first subchain
    * @param header2 : Header - header in second subchain
    * @return (chain from common block to header1, chain from common block to header2)
    */
  protected[history] def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain) = {
    assert(contains(header1) && contains(header2), "Should never call this function for non-existing headers")
    val heightDiff = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDiff)
      if (r._1.head == r._2.head) {
        r
      } else {
        if (!otherChain.head.isGenesis) {
          val biggerOther = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
          loop(biggerOther.size, biggerOther)
        } else {
          (HeaderChain(PreGenesisHeader +: r._1.headers), HeaderChain(PreGenesisHeader +: r._2.headers))
        }
      }
    }

    loop(2, HeaderChain(Seq(header2)))
  }

  protected[history] def commonBlockThenSuffixes(otherChain: HeaderChain,
                                                 startHeader: Header,
                                                 limit: Int): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id == h.id)

    val ourChain = headerChainBack(limit, startHeader, until)
    val commonBlock = ourChain.head
    val commonBlockThenSuffixes = otherChain.takeAfter(commonBlock)
    (ourChain, commonBlockThenSuffixes)
  }

  /**
    * Return semantic validity status of modifier with id == modifierId
    *
    * @param modifierId - modifier id to check
    * @return
    */
  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = {
    historyStorage.getIndex(validityKey(modifierId)) match {
      case Some(b) if b.headOption.contains(Valid) => ModifierSemanticValidity.Valid
      case Some(b) if b.headOption.contains(Invalid) => ModifierSemanticValidity.Invalid
      case None if contains(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case m =>
        log.error(s"Incorrect validity status: $m")
        ModifierSemanticValidity.Absent
    }
  }

  /**
    * @param header - header to start from (it is excluded from result)
    * @param howMany - maximum number of headers to read after `header`
    * @return up to `howMany` headers after `header` (exclusive)
    */
  def bestHeadersAfter(header: Header, howMany: Int): Seq[Header] = {
    @tailrec
    def accumulateHeaders(height: Int, accumulator: Seq[Header], left: Int): Seq[Header] = {
      if (left == 0) {
        accumulator
      } else {
        bestHeaderAtHeight(height) match {
          case Some(hdr) => accumulateHeaders(height + 1, accumulator :+ hdr, left - 1)
          case None => accumulator
        }
      }
    }

    val height = header.height
    accumulateHeaders(height + 1, Seq.empty, howMany)
  }

  /**
    * Constructs popow header against given header identifier
    *
    * @param headerId - identifier of the header
    * @return PoPowHeader(header + interlinks + interlinkProof) or
    *         None if header of extension of a corresponding block are not available
    */
  def popowHeader(headerId: ModifierId): Option[PoPowHeader] = {
    typedModifierById[Header](headerId).flatMap(h =>
      typedModifierById[Extension](h.extensionId).flatMap { ext =>
        val interlinks = NipopowAlgos.unpackInterlinks(ext.fields).toOption
        val interlinkProof = NipopowAlgos.proofForInterlinkVector(ext)
        (interlinks, interlinkProof) match {
          case (Some(links), Some(proof)) => Some(PoPowHeader(h, links, proof))
          case _ => None
        }
      }
    )
  }

  /**
    * Constructs popow header (header + interlinks) for еру best header at given height
    *
    * @param height - height
    * @return PoPowHeader(header + interlinks) or None if header of extension of a corresponding block are not available
    */
  def popowHeader(height: Int): Option[PoPowHeader] = {
    bestHeaderIdAtHeight(height).flatMap(popowHeader)
  }

  /**
    * Constructs PoPoW proof for given m and k according to KMZ17 (FC20 version).
    * See PoPowAlgos.prove for construction details.
    * @param m - min superchain length
    * @param k - suffix length
    * @param headerIdOpt - optional header to start suffix from (so to construct proof for the header).
    *                    Please note that k-1 headers will be provided after the header.
    * @return PoPow proof if success, Failure instance otherwise
    */
  def popowProof(m: Int, k: Int, headerIdOpt: Option[ModifierId]): Try[NipopowProof] = {
    val proofParams = PoPowParams(m, k)
    nipopowAlgos.prove(histReader = this, headerIdOpt = headerIdOpt)(proofParams)
  }

  /**
    * Get estimated height of headers-chain, if it is synced
    * @return height of last header known, if headers-chain is synced, or None if not synced
    */
  def estimatedTip(): Option[Height] = {
    Try { //error may happen if history not initialized
      if(isHeadersChainSynced) {
        Some(headersHeight)
      } else {
        None
      }
    }.getOrElse(None)
  }

}

object ErgoHistoryReader {
  // When we need to help other peer to find a common block when its status is unknown,
  // we send headers with offsets (from the blockchain tip) from below
  val FullV2SyncOffsets = Array(0, 16, 128, 512)

  // When only last header to be sent in sync v2 message
  val ReducedV2SyncOffsets = Array(0)
}
