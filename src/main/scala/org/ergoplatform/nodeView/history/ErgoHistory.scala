package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.block.ErgoBlock
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

class ErgoHistory(storage: HistoryStorage, validators: Seq[BlockValidator[ErgoBlock]], settings: ErgoSettings)
  extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoBlock, ErgoSyncInfo, ErgoHistory]
    with ScorexLogging {

  val bestBlock: ErgoBlock = storage.bestBlock
  val bestBlockId: ModifierId = bestBlock.id
  val height: Int = storage.heightOf(bestBlockId).getOrElse(0)

  override def isEmpty: Boolean = storage.height == 0

  override def modifierById(id: ModifierId): Option[ErgoBlock] = storage.modifierById(id)

  override def append(block: ErgoBlock): Try[(ErgoHistory, ProgressInfo[ErgoBlock])] = Try {
    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    require(applicable(block))
    validate(block).get
    val progress: ProgressInfo[ErgoBlock] = if (block.parentId sameElements bestBlockId) {
      //new block at the end of a chain
      storage.insert(block, isBest = true)
      ProgressInfo(None, Seq(), Seq(block))
    } else if (storage.heightOf(block.parentId) == storage.heightOf(bestBlockId)) {
      log.debug(s"New best fork with block ${block.encodedId}")
      processFork(block).get
    } else {
      log.debug(s"New orphaned PoW block ${block.encodedId}")
      storage.insert(block, isBest = false)
      ProgressInfo(None, Seq(), Seq()) //todo: fix
    }
    (new ErgoHistory(storage, validators, settings), progress)
  }

  private def processFork(block: ErgoBlock): Try[ProgressInfo[ErgoBlock]] = Try {
    //TODO don't put settings.maxRollback blocks in memory
    val currentChain = lastBlocks(settings.maxRollback)
    val parent = modifierById(block.parentId).get
    def until(b: ErgoBlock): Boolean = isGenesis(b) || currentChain.exists(_.id sameElements b.id)
    val toApply = chainBack(settings.maxRollback, parent, until)
    storage.insert(block, isBest = true)
    val bestCommon = toApply.head
    val i = currentChain.indexWhere(_.id sameElements bestCommon.id)
    val toRollback = currentChain.takeRight(currentChain.length - i)
    assert(toRollback.head.id sameElements bestCommon.id)
    ProgressInfo(Some(bestCommon.id), toRollback, toApply)
  }

  override def compare(other: ErgoSyncInfo): HistoryComparisonResult.Value = {
    def until(b: ErgoBlock): Boolean = isGenesis(b) || other.lastBlockIds.exists(_ sameElements b.id)
    chainBack(settings.maxRollback + 1, bestBlock, until) match {
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
    storage.drop(modifierId)
    new ErgoHistory(storage, validators, settings)
  }

  override def openSurfaceIds(): Seq[ModifierId] = Seq(bestBlockId)

  override def applicable(modifier: ErgoBlock): Boolean = !contains(modifier.id) &&
    storage.heightOf(modifier.parentId).exists(_ > height - settings.maxRollback)

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = {
    val bestcommonPoint: Int = from.flatMap(f => storage.heightOf(f._2)).max
    def until(b: ErgoBlock): Boolean = isGenesis(b) || from.exists(_._2 sameElements b.id)
    val last = chainBack(size + 1, bestBlock, until)
    if (last.length > size) None
    else Some(last.map(b => (b.modifierTypeId, b.id)))
  }

  override def syncInfo(answer: Boolean): ErgoSyncInfo = ErgoSyncInfo(answer,
    lastBlocks(ErgoSyncInfo.MaxBlockIds).map(_.id))

  private def lastBlocks(count: Int): Seq[ErgoBlock] = {
    def until(b: ErgoBlock): Boolean = isGenesis(b)
    chainBack(count, bestBlock, until)
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

  def isGenesis(b: ErgoBlock): Boolean = b.id sameElements settings.genesisId

  override type NVCT = ErgoHistory
}

object ErgoHistory extends ScorexLogging {

  def readOrGenerate(settings: ErgoSettings): ErgoHistory = {
    val dataDir = settings.dataDir

    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        blockStorage.close()
      }
    })

    val storage = new HistoryStorage(blockStorage, settings)
    if(storage.bestBlockId sameElements settings.genesisId) storage.insert(settings.genesisBlock, isBest = true)
    val validators = Seq()

    new ErgoHistory(storage, validators, settings)
  }
}


