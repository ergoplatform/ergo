package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.block.BlockValidator
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.util.Try

class ErgoHistory(storage: HistoryStorage, validators: Seq[BlockValidator[ErgoBlock]], settings: ErgoSettings)
  extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoBlock, ErgoSyncInfo, ErgoHistory]
    with ScorexLogging {

  val bestBlock: ErgoBlock = storage.bestBlock
  val bestBlockId: ModifierId = bestBlock.id
  val height: Int = storage.heightOf(bestBlockId).get

  override def isEmpty: Boolean = storage.height == 1

  override def modifierById(id: ModifierId): Option[ErgoBlock] = storage.modifierById(id)

  override def append(modifier: ErgoBlock): Try[(ErgoHistory, ProgressInfo[ErgoBlock])] = ???

  override def drop(modifierId: ModifierId): ErgoHistory = ???

  override def openSurfaceIds(): Seq[ModifierId] = Seq(bestBlockId)

  override def applicable(modifier: ErgoBlock): Boolean = !contains(modifier.id) &&
    storage.heightOf(modifier.parentId).exists(_ > height - settings.maxRollback)

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  override def syncInfo(answer: Boolean): ErgoSyncInfo = ErgoSyncInfo(answer,
    lastBlocks(ErgoSyncInfo.MaxBlockIds, bestBlock).map(_.id))

  def lastBlocks(count: Int, startBlock: ErgoBlock): Seq[ErgoBlock] = {
    @tailrec
    def loop(remain: Int, block: ErgoBlock, acc: Seq[ErgoBlock]): Seq[ErgoBlock] = {
      if (isGenesis(block) || remain == 0) {
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

  override def compare(other: ErgoSyncInfo): HistoryComparisonResult.Value = ???

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
    val validators = Seq()

    new ErgoHistory(storage, validators, settings)
  }
}


