package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, ShardedStore, Store}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs.{ADProofsProcessor, ADStateProofsProcessor, EmptyADProofsProcessor, FullStateProofsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.{BlockTransactionsProcessor, EmptyBlockTransactionsProcessor, FullnodeBlockTransactionsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.popow.{EmptyPoPoWProofsProcessor, FullPoPoWProofsProcessor, PoPoWProofsProcessor}
import org.ergoplatform.settings.{Algos, ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core._
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.consensus.{History, ModifierSemanticValidity}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

/**
  * History implementation. It is processing persistent modifiers generated locally or coming from network.
  * Depending on chosen node settings, it will process modifiers in a different way, different processors define how to
  * process different type of modifiers.
  *
  * HeadersProcessor: processor of block headers. It's the same for all node settings
  * ADProofsProcessor: processor of ADProofs. ADProofs may
  *   1. Be downloaded from other nodes (ADState == true)
  *   2. Be calculated by using local state (ADState == false)
  *   3. Be ignored by history in light mode (verifyTransactions == false)
  * PoPoWProofsProcessor: processor of PoPoWProof. PoPoWProof may
  *   1. Be downloaded once during bootstrap from other peers (poPoWBootstrap == true)
  *   2. Be ignored by history (poPoWBootstrap == false)
  * BlockTransactionsProcessor: Processor of BlockTransactions. BlockTransactions may
  *   1. Be downloaded from other peers (verifyTransactions == true)
  *   2. Be ignored by history (verifyTransactions == false)
  */
trait ErgoHistory
  extends History[ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
    with HeadersProcessor
    with ADProofsProcessor
    with PoPoWProofsProcessor
    with UTXOSnapshotChunkProcessor
    with BlockTransactionsProcessor
    with ScorexLogging {

  protected val chainSettings: ChainSettings
  protected val config: NodeConfigurationSettings
  protected val storage: Store

  protected lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)

  override type NVCT = ErgoHistory

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet (from a chain or a PoPoW proof).
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for fullnode regime after initial bootstrap.
    */
  def bestFullBlockOpt: Option[ErgoFullBlock] =
    bestFullBlockIdOpt.flatMap(id => typedModifierById[Header](id)).flatMap(getFullBlock)

  /**
    * Get ErgoPersistentModifier by it's id if it is in history
    */
  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    val modifier = historyStorage.modifierById(id)
    assert(modifier.forall(_.id sameElements id), s"Modifier $modifier id is incorrect, ${Algos.encode(id)} expected")
    modifier
  }

  /**
    * Get ErgoPersistentModifier of type T by it's id if it is in history
    */
  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T@unchecked) if m.isInstanceOf[T] => Some(m)
    case _ => None
  }

  /**
    * Append ErgoPersistentModifier to History if valid
    */
  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    applicableTry(modifier).map { _ =>
      modifier match {
        case header: Header =>
          (this, process(header))
        case blockTransactions: BlockTransactions =>
          (this, process(blockTransactions))
        case aDProofs: ADProofs =>
          (this, process(aDProofs))
        case poPoWProof: PoPoWProof =>
          (this, process(poPoWProof))
        case chunk: UTXOSnapshotChunk =>
          (this, process(chunk))
      }
    }
  }

  /**
    * Id of best block to mine
    */
  override def openSurfaceIds(): Seq[ModifierId] = bestFullBlockIdOpt.orElse(bestHeaderIdOpt).toSeq

  /**
    * Check, that it's possible to apply modifier to history
    */
  override def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(info: ErgoSyncInfo): HistoryComparisonResult.Value = {
    bestHeaderIdOpt match {
      case Some(id) if info.lastHeaderIds.lastOption.exists(_ sameElements id) =>
        HistoryComparisonResult.Equal
      case Some(id) if info.lastHeaderIds.exists(_ sameElements id) =>
        HistoryComparisonResult.Older
      case Some(_) if info.lastHeaderIds.isEmpty =>
        HistoryComparisonResult.Younger
      case Some(_) =>
        //Compare headers chain
        val ids = info.lastHeaderIds
        ids.view.reverse.find(m => contains(m)) match {
          case Some(lastId) =>
            //TODO We anyway have our fork to send. is it ok?
            HistoryComparisonResult.Younger
          case None => HistoryComparisonResult.Nonsense
        }
      case None =>
        log.warn("Trying to compare with other node while our history is empty")
        HistoryComparisonResult.Older
    }
  }

  /**
    * @param info other's node sync info
    * @param size max return size
    * @return Ids of headerss, that node with info should download and apply to synchronize
    */
  override def continuationIds(info: ErgoSyncInfo, size: Int): Option[ModifierIds] = Try {
    if (isEmpty) {
      info.startingPoints
    } else if (info.lastHeaderIds.isEmpty) {
      val heightFrom = Math.min(headersHeight, size - 1)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[Header](startId).get
      val headers = headerChainBack(size, startHeader, _ => false)
      assert(headers.headers.exists(_.height == 0), "Should always contain genesis header")
      headers.headers.flatMap(h => Seq((Header.modifierTypeId, h.id)))
    } else {
      val ids = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight = heightOf(lastHeaderInOurBestChain).get
      val heightFrom = Math.min(headersHeight, theirHeight + size)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[Header](startId).get
      val headerIds = headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
        .headers.map(h => Header.modifierTypeId -> h.id)
      headerIds
    }
  }.toOption

  /**
    * @return all possible forks, that contains specified header
    */
  private[history] def continuationHeaderChains(header: Header): Seq[HeaderChain] = {
    def loop(acc: Seq[Header]): Seq[HeaderChain] = {
      val bestHeader = acc.last
      val currentHeight = heightOf(bestHeader.id).get
      val nextLevelHeaders = headerIdsAtHeight(currentHeight + 1).map(id => typedModifierById[Header](id).get)
        .filter(_.parentId sameElements bestHeader.id)
      if (nextLevelHeaders.isEmpty) Seq(HeaderChain(acc))
      else nextLevelHeaders.map(h => acc :+ h).flatMap(chain => loop(chain))
    }

    loop(Seq(header))
  }


  /**
    *
    * @param answer - whether it is answer to other node request or not
    * @return Node ErgoSyncInfo
    */
  override def syncInfo(answer: Boolean): ErgoSyncInfo = if (isEmpty) {
    ErgoSyncInfo(answer, Seq())
  } else {
    ErgoSyncInfo(answer, lastHeaders(ErgoSyncInfo.MaxBlockIds).headers.map(_.id))
  }

  /**
    * Return last count headers from best headers chain if exist or chain up to genesis otherwise
    */
  def lastHeaders(count: Int): HeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, b => false)).getOrElse(HeaderChain.empty)

  private def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = {
    modifier match {
      case header: Header =>
        validate(header)
      case m: BlockTransactions =>
        validate(m)
      case m: ADProofs =>
        validate(m)
      case m: PoPoWProof =>
        validate(m)
      case chunk: UTXOSnapshotChunk =>
        validate(chunk)
      case m: Any =>
        Failure(new Error(s"Modifier $m has incorrect type"))
    }
  }

  def getFullBlock(header: Header): Option[ErgoFullBlock] = {
    val aDProofs = typedModifierById[ADProofs](header.ADProofsId)
    typedModifierById[BlockTransactions](header.transactionsId).map { txs =>
      ErgoFullBlock(header, txs, aDProofs)
    }
  }

  def missedModifiersForFullChain(): Seq[(ModifierTypeId, ModifierId)] = {
    if (config.verifyTransactions) {
      bestHeaderOpt.toSeq
        .flatMap(h => headerChainBack(headersHeight + 1, h, p => contains(p.ADProofsId) && contains(p.transactionsId)).headers)
        .flatMap(h => Seq((BlockTransactions.modifierTypeId, h.transactionsId), (ADProofs.modifierTypeId, h.ADProofsId)))
        .filter(id => !contains(id._2))
    } else {
      Seq()
    }
  }

  protected[history] def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain) = {
    assert(contains(header1))
    assert(contains(header2))

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack)
      if (r._1.head == r._2.head) {
        r
      } else {
        val biggerOther = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) {
          loop(biggerOther.size, biggerOther)
        } else {
          throw new Error(s"Common point not found for headers $header1 and $header2")
        }
      }
    }

    loop(2, HeaderChain(Seq(header2)))
  }

  protected[history] def commonBlockThenSuffixes(otherChain: HeaderChain,
                                                 startHeader: Header,
                                                 limit: Int): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)

    val ourChain = headerChainBack(limit, startHeader, until)
    val commonBlock = ourChain.head
    val commonBlockThenSuffixes = otherChain.takeAfter(commonBlock)
    (ourChain, commonBlockThenSuffixes)
  }

  /**
    * Report some modifier as valid or invalid semantically
    */

  //todo: fix
  override def reportSemanticValidity(modifier: ErgoPersistentModifier,
                                      valid: Boolean,
                                      unusedParam: ModifierId): (ErgoHistory, ProgressInfo[ErgoPersistentModifier]) = {
    def validityRowsForHeader(h: Header): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
      Seq(h.id, h.transactionsId, h.ADProofsId).map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte)))
    }

    if (valid) {
      modifier match {
        case fb: ErgoFullBlock =>
          val bestHeader = bestHeaderOpt.get
          val ids = Seq(fb.id, fb.header.id, fb.blockTransactions.id) ++ fb.aDProofs.map(_.id)
            .map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte)))

          historyStorage.db.update(validityKey(modifier.id), Seq(), Seq(validityKey(modifier.id) ->
            ByteArrayWrapper(Array(1.toByte))))

          val bestFull = bestFullBlockOpt.get
          if (fb == bestFull) {
            //applied best header to history
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
          } else {
            //in fork processing
            val modHeight = heightOf(fb.header.id).get
            val chainBack = headerChainBack(headersHeight - modHeight, bestHeader, h => h.parentId sameElements fb.header.id)
            //block in the best chain that link to this header
            val toApply = chainBack.headOption.flatMap(opt => getFullBlock(opt))
            assert(toApply.get.header.parentId sameElements fb.header.id, "Should never be here, State is inconsistent")
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), toApply, Seq())
          }
        case _ =>
          historyStorage.db.update(validityKey(modifier.id), Seq(), Seq(validityKey(modifier.id) ->
            ByteArrayWrapper(Array(1.toByte))))
          this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
      }

    } else {
      val headerOpt: Option[Header] = modifier match {
        case h: Header => Some(h)
        case full: ErgoFullBlock => Some(full.header)
        case proof: ADProofs => typedModifierById[Header](proof.headerId)
        case txs: BlockTransactions => typedModifierById[Header](txs.headerId)
        case _ => None
      }
      headerOpt match {
        case Some(h) =>
          val invalidatedHeaders = continuationHeaderChains(h).flatMap(_.headers).distinct
          log.info(s"Invalidated header ${h.encodedId} and linked ${invalidatedHeaders.map(_.encodedId).mkString(",")}")
          val validityRow = invalidatedHeaders.flatMap(h => validityRowsForHeader(h))

          def isStillValid(id: ModifierId): Boolean = !invalidatedHeaders.exists(_.id sameElements id)

          def loopHeightDown(height: Int): Header = {
            assert(height >= 0, s"Mark genesis invalid is not true")
            headerIdsAtHeight(height).find(id => isStillValid(id)).flatMap(id => typedModifierById[Header](id)) match {
              case Some(header) => header
              case None => loopHeightDown(height - 1)
            }
          }

          val branchValidHeader: Header = loopHeightDown(headersHeight)
          val bestValidFullOpt: Option[Header] = bestFullBlockOpt.flatMap(h => heightOf(h.header.id))
            .map(loopHeightDown)

          if (bestHeaderOpt.contains(branchValidHeader) && bestFullBlockOpt.forall(b => bestValidFullOpt.contains(b))) {
            historyStorage.db.update(validityKey(modifier.id), Seq(), validityRow)
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
          } else {
            val changedLinks = bestValidFullOpt.toSeq.map(h => BestFullBlockKey -> ByteArrayWrapper(h.id)) :+
              (BestHeaderKey, ByteArrayWrapper(branchValidHeader.id))
            val (validChain, invalidatedChain) = (bestValidFullOpt, bestFullBlockOpt) match {
              case (Some(bestValid), Some(bestFull)) =>
                val headersChain = commonBlockThenSuffixes(bestValid, bestFull.header)
                (headersChain._1.headers.flatMap(h => getFullBlock(h)), headersChain._2.headers.flatMap(h => getFullBlock(h)))
              case _ =>
                val headersChain = commonBlockThenSuffixes(branchValidHeader, bestHeaderOpt.get)
                (headersChain._1.headers, headersChain._2.headers)
            }
            assert(invalidatedChain.head == validChain.head, s"${invalidatedChain.head} == ${validChain.head}")
            val branchPoint: Some[ModifierId] = invalidatedChain.head match {
              case fullBlock: ErgoFullBlock => Some(fullBlock.header.id)
              case header: Header => Some(header.id)
            }

            val toInsert = validityRow ++ changedLinks
            historyStorage.db.update(validityKey(modifier.id), Seq(), toInsert)

            //TODO ???
            this -> ProgressInfo[ErgoPersistentModifier](branchPoint, invalidatedChain.tail,
              validChain.tail.headOption, Seq())
          }
        case None =>
          historyStorage.db.update(validityKey(modifier.id), Seq(), Seq(validityKey(modifier.id) ->
            ByteArrayWrapper(Array(0.toByte))))
          this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
      }
    }

  }

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value = {
    historyStorage.db.get(validityKey(modifierId)) match {
      case Some(b) if b.data.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(b) if b.data.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if contains(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case m =>
        log.error(s"Incorrect validity status: $m")
        ModifierSemanticValidity.Absent
    }
  }
}

object ErgoHistory extends ScorexLogging {

  type Height = Int
  type Score = BigInt
  type Difficulty = BigInt
  type NBits = Long

  val GenesisHeight = 0

  //todo: move pow to settings
  def readOrGenerate(settings: ErgoSettings): ErgoHistory = {
    val dataDir = settings.directory
    val iFile = new File(s"$dataDir/history")
    iFile.mkdirs()
    val db = new ShardedStore(iFile)

    val nodeSettings = settings.nodeSettings


    val history: ErgoHistory = (nodeSettings.ADState, nodeSettings.verifyTransactions, nodeSettings.PoPoWBootstrap) match {
      case (true, true, true) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case (true, true, false) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case (false, true, true) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case (false, true, false) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case (true, false, true) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case (true, false, false) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: Store = db
          override val powScheme = chainSettings.poWScheme
        }
      case m =>
        throw new Error(s"Unsupported settings combination ADState==${m._1}, verifyTransactions==${m._2}, " +
          s"poPoWBootstrap==${m._1}")
    }
    history
  }
}