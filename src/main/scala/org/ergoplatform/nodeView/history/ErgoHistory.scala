package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.block.{ErgoBlock, ErgoFullBlock, ErgoHeader}
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewModifier._
import scorex.core.block.BlockValidator
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.{Failure, Try}

class ErgoHistory(fullBlockStorage: HistoryStorage[ErgoFullBlock],
                  headerStorage: HistoryStorage[ErgoHeader],
                  validators: Seq[BlockValidator[ErgoBlock]],
                  settings: ErgoSettings)
  extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoBlock, ErgoSyncInfo, ErgoHistory]
    with ScorexLogging {

  val bestFullBlock: ErgoFullBlock = fullBlockStorage.bestBlock
  val bestHeader: ErgoHeader = headerStorage.bestBlock
  lazy val bestFullBlockId: ModifierId = bestFullBlock.id
  lazy val bestHeaderId: ModifierId = bestHeader.id
  lazy val fullBlocksHeight: Int = fullBlockStorage.heightOf(bestFullBlockId).getOrElse(0)
  lazy val headersHeight: Int = fullBlockStorage.heightOf(bestHeaderId).getOrElse(0)

  override def isEmpty: Boolean = fullBlockStorage.height == 0

  override def modifierById(id: ModifierId): Option[ErgoBlock] = fullBlockById(id).orElse(headerById(id))

  def fullBlockById(id: ModifierId): Option[ErgoFullBlock] = fullBlockStorage.modifierById(id)

  def headerById(id: ModifierId): Option[ErgoHeader] = headerStorage.modifierById(id)

  override def append(block: ErgoBlock): Try[(ErgoHistory, ProgressInfo[ErgoBlock])] = Try {
    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    require(applicable(block))
    validate(block).get
    val progress: ProgressInfo[ErgoBlock] = if (block.parentId sameElements storageOf(block).bestBlockId) {
      //new block at the end of a chain
      storageOf(block).insert(block, isBest = true)
      ProgressInfo(None, Seq(), Seq(block))
    } else if (storageOf(block).heightOf(block.parentId) == storageOf(block).heightOf(storageOf(block).bestBlockId)) {
      log.debug(s"New best fork with block ${block.encodedId}")
      processFork(block).get
    } else {
      log.debug(s"New orphaned PoW block ${block.encodedId}")
      storageOf(block).insert(block, isBest = false)
      ProgressInfo(None, Seq(), Seq()) //todo: fix
    }
    (new ErgoHistory(fullBlockStorage, headerStorage, validators, settings), progress)
  }

  private def storageOf[T <: ErgoBlock](b: T): HistoryStorage[T] = b match {
    case h: ErgoHeader => headerStorage.asInstanceOf[HistoryStorage[T]]
    case h: ErgoFullBlock => fullBlockStorage.asInstanceOf[HistoryStorage[T]]
  }

  private def processFork(block: ErgoBlock): Try[ProgressInfo[ErgoBlock]] = Try {
    //TODO don't put settings.maxRollback blocks in memory
    val currentChain = lastBlocks(settings.maxRollback)
    val parent = modifierById(block.parentId).get
    def until(b: ErgoBlock): Boolean = b.isGenesis || currentChain.exists(_.id sameElements b.id)
    val toApply = chainBack(settings.maxRollback, parent, until)
    storageOf(block).insert(block, isBest = true)
    val bestCommon = toApply.head
    val i = currentChain.indexWhere(_.id sameElements bestCommon.id)
    val toRollback = currentChain.takeRight(currentChain.length - i)
    assert(toRollback.head.id sameElements bestCommon.id)
    ProgressInfo(Some(bestCommon.id), toRollback, toApply)
  }

  override def compare(other: ErgoSyncInfo): HistoryComparisonResult.Value = {
    def until(b: ErgoBlock): Boolean = b.isGenesis || other.lastBlockIds.exists(_ sameElements b.id)
    chainBack(settings.maxRollback + 1, bestFullBlock, until) match {
      case last: Seq[ErgoBlock] if last.length > settings.maxRollback =>
        HistoryComparisonResult.Nonsense
      case last: Seq[ErgoBlock] if last.isEmpty =>
        if (other.lastBlockIds.exists(id => !contains(id))) HistoryComparisonResult.Older
        else HistoryComparisonResult.Equal
      case last: Seq[ErgoBlock] =>
        val bestCommon = last.head
        val blocksAfterCommon = other.lastBlockIds.length - other.lastBlockIds.indexWhere(_ sameElements bestCommon.id)
        if (blocksAfterCommon == last.length) {
          HistoryComparisonResult.Equal
        } else if (blocksAfterCommon > last.length) {
          HistoryComparisonResult.Older
        } else {
          HistoryComparisonResult.Younger
        }
    }
  }


  private def validate(block: ErgoBlock): Try[Unit] = Try {
    val validationResuls = validators.map(_.validate(block))
    validationResuls.foreach {
      case Failure(e) => log.warn(s"Block validation failed", e)
      case _ =>
    }
    validationResuls.foreach(_.get)
  }

  override def drop(modifierId: ModifierId): ErgoHistory = {
    fullBlockStorage.drop(modifierId)
    new ErgoHistory(fullBlockStorage, headerStorage, validators, settings)
  }

  override def openSurfaceIds(): Seq[ModifierId] = Seq(bestFullBlockId)

  override def applicable(modifier: ErgoBlock): Boolean = {
    val containsHeaderForFullBlock = modifier match {
      case f: ErgoFullBlock => contains(f.header)
      case _ => true
    }
    !contains(modifier) && containsHeaderForFullBlock &&
      storageOf(modifier).heightOf(modifier.parentId).exists(_ > fullBlocksHeight - settings.maxRollback)
  }

  override def contains(pm: ErgoBlock): Boolean = storageOf(pm).contains(pm.id)

  override def contains(id: ModifierId): Boolean = {
    //TODO what if we have Header but don't have FullBlock??
    fullBlockStorage.contains(id) && headerStorage.contains(id)
  }

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = {
    val bestcommonPoint: Int = from.flatMap(f => fullBlockStorage.heightOf(f._2)).max
    def until(b: ErgoBlock): Boolean = b.isGenesis || from.exists(_._2 sameElements b.id)
    val last = chainBack(size + 1, bestFullBlock, until)
    if (last.length > size) None
    else Some(last.map(b => (b.modifierTypeId, b.id)))
  }

  override def syncInfo(answer: Boolean): ErgoSyncInfo = ErgoSyncInfo(answer,
    lastBlocks(ErgoSyncInfo.MaxBlockIds).map(_.id))

  private def lastBlocks(count: Int): Seq[ErgoBlock] = {
    def until(b: ErgoBlock): Boolean = b.isGenesis
    chainBack(count, bestFullBlock, until)
  }

  private def chainBack(count: Int, startBlock: ErgoBlock, until: ErgoBlock => Boolean): Seq[ErgoBlock] = {
    @tailrec
    def loop(remain: Int, block: ErgoBlock, acc: Seq[ErgoBlock]): Seq[ErgoBlock] = {
      if (until(block) || remain == 0) {
        acc
      } else {
        modifierById(block.parentId) match {
          case Some(parent) =>
            loop(remain - 1, parent, acc :+ parent)
          case _ =>
            log.warn(s"No parent block in history for block ${block.encodedId}")
            acc
        }
      }
    }
    if (isEmpty) Seq()
    else loop(count, startBlock, Seq(startBlock))
  }

  override type NVCT = ErgoHistory
}

object ErgoHistory extends ScorexLogging {

  def readOrGenerate(settings: ErgoSettings): ErgoHistory = {
    val dataDir = settings.dataDir

    val fbFile = new File(s"$dataDir/blockchain")
    fbFile.mkdirs()
    val fullBlockDB = new LSMStore(fbFile, maxJournalEntryCount = 10000)

    val hFile = new File(s"$dataDir/headers")
    hFile.mkdirs()
    val headerDB = new LSMStore(hFile, maxJournalEntryCount = 10000)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        fullBlockDB.close()
        headerDB.close()
      }
    })

    val fbStorage = new HistoryStorage[ErgoFullBlock](fullBlockDB, settings.genesisId)
    val headerStorage = new HistoryStorage[ErgoHeader](headerDB, settings.genesisId)
    if (fbStorage.bestBlockId sameElements settings.genesisId) {
      fbStorage.insert(settings.genesisBlock, isBest = true)
      headerStorage.insert(settings.genesisBlock.header, isBest = true)
    }
    val validators = Seq()

    new ErgoHistory(fbStorage, headerStorage, validators, settings)
  }
}


