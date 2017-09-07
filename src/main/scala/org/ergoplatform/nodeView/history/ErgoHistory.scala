package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs.{ADProofsProcessor, ADStateProofsProcessor, EmptyADProofsProcessor, FullStateProofsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.{BlockTransactionsProcessor, EmptyBlockTransactionsProcessor, FullnodeBlockTransactionsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.popow.{EmptyPoPoWProofsProcessor, FullPoPoWProofsProcessor, PoPoWProofsProcessor}
import org.ergoplatform.settings.{Algos, ErgoSettings, NodeConfigurationSettings}
import scorex.core._
import scorex.core.consensus.{History, ModifierSemanticValidity}
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
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

  protected val config: NodeConfigurationSettings
  protected val storage: LSMStore

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
  def bestFullBlockOpt: Option[ErgoFullBlock] = Try {
    getFullBlock(typedModifierById[Header](bestFullBlockIdOpt.get).get)
  }.toOption

  /**
    * Get ErgoPersistentModifier by it's id if it is in history
    */
  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    val modifier = historyStorage.modifierById(id)
    assert(modifier.forall(_.id sameElements id), s"Modifier $modifier id is incorrect, ${Base58.encode(id)} expected")
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
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} to history")
    applicableTry(modifier).map { _ =>
      modifier match {
        case m: Header =>
          (this, process(m))
        case m: BlockTransactions =>
          (this, process(m))
        case m: ADProofs =>
          (this, process(m))
        case m: PoPoWProof =>
          (this, process(m))
        case m: UTXOSnapshotChunk =>
          (this, process(m))
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
        //Header chain is equals, compare full blocks
        (info.fullBlockIdOpt, bestFullBlockIdOpt) match {
          case (Some(theirBestFull), Some(ourBestFull)) if !(theirBestFull sameElements ourBestFull) =>
            if (scoreOf(theirBestFull).exists(theirScore => scoreOf(ourBestFull).exists(_ > theirScore))) {
              HistoryComparisonResult.Younger
            } else {
              HistoryComparisonResult.Older
            }
          case _ =>
            HistoryComparisonResult.Equal
        }
      case Some(id) if info.lastHeaderIds.exists(_ sameElements id) =>
        HistoryComparisonResult.Older
      case Some(_) =>
        //Compare headers chain
        val ids = info.lastHeaderIds
        ids.view.reverse.find(m => contains(m)) match {
          case Some(lastId) =>
            val ourDiffOpt = heightOf(lastId).map(h => height - h)
            if (ourDiffOpt.exists(ourDiff => ourDiff > (ids.length - ids.indexWhere(_ sameElements lastId)))) {
              HistoryComparisonResult.Younger
            } else {
              HistoryComparisonResult.Older
            }
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
    * @return Ids of modifiers, that node with info should download and apply to synchronize
    */
  override def continuationIds(info: ErgoSyncInfo, size: Int): Option[ModifierIds] = Try {
    val ids = info.lastHeaderIds
    val lastHeaderInHistory = ids.view.reverse.find(m => contains(m)).get
    val theirHeight = heightOf(lastHeaderInHistory).get
    val heightFrom = Math.min(height, theirHeight + size)
    val startId = headerIdsAtHeight(heightFrom).head
    val startHeader = typedModifierById[Header](startId).get
    val headerIds = headerChainBack(heightFrom - theirHeight, startHeader, (h: Header) => h.isGenesis)
      .headers.map(h => Header.modifierTypeId -> h.id)
    val fullBlockContinuation: ModifierIds = info.fullBlockIdOpt.flatMap(heightOf) match {
      case Some(bestFullBlockHeight) =>
        val heightFrom = Math.min(height, bestFullBlockHeight + size)
        val startId = headerIdsAtHeight(heightFrom).head
        val startHeader = typedModifierById[Header](startId).get
        val headers = headerChainBack(heightFrom - bestFullBlockHeight, startHeader, (h: Header) => h.isGenesis)
        headers.headers.flatMap(h => Seq((ADProofs.modifierTypeId, h.ADProofsId),
          (BlockTransactions.modifierTypeId, h.transactionsId)))
      case _ => Seq()
    }
    headerIds ++ fullBlockContinuation
  }.toOption

  /**
    *
    * @param answer - whether it is answer to other node request or not
    * @return Node ErgoSyncInfo
    */
  override def syncInfo(answer: Boolean): ErgoSyncInfo = if (isEmpty) {
    ErgoSyncInfo(answer, Seq(), None)
  } else {
    ErgoSyncInfo(answer,
      lastHeaders(ErgoSyncInfo.MaxBlockIds).headers.map(_.id),
      bestFullBlockIdOpt)
  }

  /**
    * Return last count headers from best headers chain if exist or chain up to genesis otherwise
    */
  def lastHeaders(count: Int): HeaderChain =
    bestHeaderOpt
      .map(bestHeader => headerChainBack(count, bestHeader, b => b.isGenesis))
      .getOrElse(HeaderChain.empty)


  private def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = {
    modifier match {
      case m: Header =>
        validate(m)
      case m: BlockTransactions =>
        validate(m)
      case m: ADProofs =>
        validate(m)
      case m: PoPoWProof =>
        validate(m)
      case m: UTXOSnapshotChunk =>
        validate(m)
      case m =>
        Failure(new Error(s"Modifier $m has incorrect type"))
    }
  }

  protected def getFullBlock(header: Header): ErgoFullBlock = {
    val aDProofs = typedModifierById[ADProofs](header.ADProofsId)
    val txs = typedModifierById[BlockTransactions](header.transactionsId).get
    ErgoFullBlock(header, txs, aDProofs)
  }

  protected[history] def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain) = {
    assert(contains(header1))
    val otherHeightOpt = heightOf(header2.id)
    assert(otherHeightOpt.isDefined)
    val otherHeight = otherHeightOpt.get

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack)
      if (r._1.head == r._2.head) {
        r
      } else {
        val biggerOther = headerChainBack(numberBack, otherChain.head, (h: Header) => h.isGenesis) ++ otherChain.tail
        if (biggerOther.length < otherHeight) {
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
                                      valid: Boolean): (ErgoHistory, ProgressInfo[ErgoPersistentModifier]) = {
/*
    val headerId = modifier match {
      case h: Header => h.id
      case proof: ADProof => typedModifierById[Header](proof.headerId).map(h => h.id).get
      case txs: BlockTransactions => typedModifierById[Header](txs.headerId).map(h => h.id).get

      case snapshot: UTXOSnapshotChunk => ???
      case m =>
        log.warn(s"reportInvalid for invalid modifier type: $m")
        ???
    }*/


    if(!valid) {
      val (idsToRemove: Seq[ByteArrayWrapper], toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = modifier match {
        case h: Header => toDrop(h)
        case proof: ADProofs => typedModifierById[Header](proof.headerId).map(h => toDrop(h)).getOrElse(Seq())
        case txs: BlockTransactions => typedModifierById[Header](txs.headerId).map(h => toDrop(h)).getOrElse(Seq())
        case snapshot: UTXOSnapshotChunk => toDrop(snapshot)
        case m =>
          log.warn(s"reportInvalid for invalid modifier type: $m")
          Seq(ByteArrayWrapper(m.id)) -> Seq()
      }
      historyStorage.update(ModifierId @@ Algos.hash(modifier.id ++ "reportInvalid".getBytes), idsToRemove, toInsert)
    }

    lazy val progressInto = ProgressInfo[ErgoPersistentModifier](None, Seq(), Seq(), Seq()) //todo: dumb values, fix
    this -> progressInto
  }

  //todo: fix
  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value = ???
}

object ErgoHistory extends ScorexLogging {

  type Height = Int
  type Score = BigInt
  type Difficulty = BigInt
  type NBits = Long

  val GenesisHeight = 0

  //todo: move pow to settings
  def readOrGenerate(settings: ErgoSettings, pow: PoWScheme): ErgoHistory = {
    val dataDir = settings.directory
    val iFile = new File(s"$dataDir/history")
    iFile.mkdirs()
    val db = new LSMStore(iFile, maxJournalEntryCount = 10000)

    val nodeSettings = settings.nodeSettings


    val history: ErgoHistory = (nodeSettings.ADState, nodeSettings.verifyTransactions, nodeSettings.PoPoWBootstrap) match {
      case (true, true, true) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case (true, true, false) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case (false, true, true) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case (false, true, false) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case (true, false, true) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case (true, false, false) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val storage: LSMStore = db
          override val powScheme = pow
        }
      case m =>
        throw new Error(s"Unsupported settings combination ADState==${m._1}, verifyTransactions==${m._2}, " +
          s"poPoWBootstrap==${m._1}")
    }

    history
  }
}