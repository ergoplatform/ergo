package org.ergoplatform.nodeView.history

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalatest.OptionValues
import scorex.db.ByteArrayWrapper
import scorex.util.ModifierId

/**
  * Tests for history database repair (ErgoHistory.repairIfNeeded) after corruptions
  * similar to ones caused by disk overflow (lost or partial writes)
  */
class RepairAfterCorruptionSpecification extends ErgoCorePropertyTest with OptionValues {

  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  /**
    * History with full blocks applied up to `fullBlocks` height, and headers applied up to `headers` height
    */
  private def historyWithGap(fullBlocks: Int, headers: Int) = {
    var history = genHistory()
    val chain = genChain(headers, history)
    history = applyChain(history, chain.take(fullBlocks))
    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(chain(fullBlocks - 1).header)
    }
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header).drop(fullBlocks)))
    history.bestFullBlockOpt.value.header.height shouldBe fullBlocks
    history.headersHeight shouldBe headers
    (history, chain)
  }

  property("repair should not damage healthy history") {
    val (history, _) = historyWithGap(fullBlocks = 5, headers = 8)
    val fullBlockBefore = history.bestFullBlockOpt.value
    val headerBefore = history.bestHeaderOpt.value

    ErgoHistory.repairIfNeeded(history) shouldBe false

    history.bestFullBlockOpt.value shouldBe fullBlockBefore
    history.bestHeaderOpt.value shouldBe headerBefore
    history.headersHeight shouldBe 8
  }

  property("repair should clear invalid mark of unapplied block section and remove it for re-download") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // block at fullHeight + 1 gets its transactions marked as invalid (as after failed state application,
    // when corresponding header is invalidated and the mark is written for all its sections)
    val nextBlock = chain(5)
    history.historyStorage.insert(
      Array(history.validityKey(nextBlock.blockTransactions.id) -> Array(0.toByte)),
      Array.empty[BlockSection]).get

    ErgoHistory.repairIfNeeded(history) shouldBe true

    // validity mark cleared, so the section can be re-downloaded and applied
    history.historyStorage.getIndex(history.validityKey(nextBlock.blockTransactions.id)) shouldBe None
    // header is kept
    history.historyStorage.contains(nextBlock.header.id) shouldBe true
    history.bestHeaderOpt.value shouldBe chain.last.header
  }

  property("repair should remove corrupted (unparsable) block section records") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // corrupt block transactions record of unapplied block (as after partial write on disk overflow)
    val nextBlock = chain(5)
    history.historyStorage
      .insert(nextBlock.blockTransactions.serializedId, Array.fill(100)(0.toByte)).get

    history.historyStorage.contains(nextBlock.blockTransactions.id) shouldBe true
    // corrupted record can not be parsed, so full block can not be assembled
    history.typedModifierById[BlockTransactions](nextBlock.blockTransactions.id) shouldBe None

    ErgoHistory.repairIfNeeded(history) shouldBe true

    // corrupted record is removed, to be re-downloaded
    history.historyStorage.contains(nextBlock.blockTransactions.id) shouldBe false
  }

  property("repair should remove sections of complete but unapplied block") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // block at fullHeight + 1 has all the required sections stored, but was never applied
    // (as after interrupted history index update)
    val nextBlock = chain(5)
    val noIndexes = Array.empty[(ByteArrayWrapper, Array[Byte])]
    history.historyStorage.insert(noIndexes, Array[BlockSection](nextBlock.blockTransactions)).get
    history.historyStorage.insert(noIndexes, Array[BlockSection](nextBlock.extension)).get
    history.bestFullBlockOpt.value.header.height shouldBe 5

    ErgoHistory.repairIfNeeded(history) shouldBe true

    // sections removed, so the block is requested and processed anew
    history.historyStorage.contains(nextBlock.blockTransactions.id) shouldBe false
    history.historyStorage.contains(nextBlock.extension.id) shouldBe false
  }

  property("repair should restore lost height -> header ids index") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // height index of an unapplied header is lost (as after interrupted index write)
    val lostHeight = 7
    history.headerIdsAtHeight(lostHeight) shouldBe Seq(chain(lostHeight - 1).header.id)
    // heightIdsKey(height) == Algos.hash(Ints.toByteArray(height))
    val heightIdsKey = ByteArrayWrapper(Algos.hash(Ints.toByteArray(lostHeight)))
    history.historyStorage.remove(Array(heightIdsKey), Array.empty[ModifierId]).get
    history.headerIdsAtHeight(lostHeight) shouldBe Seq.empty

    ErgoHistory.repairIfNeeded(history) shouldBe true

    history.headerIdsAtHeight(lostHeight) shouldBe Seq(chain(lostHeight - 1).header.id)
  }

  property("repair should truncate headers chain with corrupted header record") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // header record at height 7 is corrupted (as after partial write on disk overflow)
    val damagedHeader = chain(6).header
    history.historyStorage.insert(damagedHeader.serializedId, Array.fill(100)(0.toByte)).get
    // the record in the database is corrupted now (cache bypassing read fails to parse it)
    history.historyStorage.modifierByIdFromDb(damagedHeader.id) shouldBe None

    ErgoHistory.repairIfNeeded(history) shouldBe true

    // headers chain truncated to height 6 (just below the damaged header at height 7),
    // corrupted header and headers above it are removed
    history.bestHeaderOpt.value.height shouldBe 6
    history.bestHeaderOpt.value shouldBe chain(5).header
    history.headersHeight shouldBe 6
    history.historyStorage.contains(damagedHeader.id) shouldBe false
    history.historyStorage.contains(chain(7).header.id) shouldBe false
    // headers below the damaged one are kept
    history.historyStorage.contains(chain(5).header.id) shouldBe true
    // full chain is not touched
    history.bestFullBlockOpt.value.header.height shouldBe 5
  }

  property("repair should truncate headers chain with missing header record") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // header record at height 6 is lost completely
    val damagedHeader = chain(5).header
    history.historyStorage.remove(
      Array(history.validityKey(damagedHeader.id)),
      Array(damagedHeader.id)).get

    ErgoHistory.repairIfNeeded(history) shouldBe true

    history.bestHeaderOpt.value.height shouldBe 5
    history.headersHeight shouldBe 5
    history.historyStorage.contains(damagedHeader.id) shouldBe false
    history.historyStorage.contains(chain(6).header.id) shouldBe false
    history.bestFullBlockOpt.value.header.height shouldBe 5
  }

  property("repaired history allows full blocks syncing to proceed") {
    val (history, chain) = historyWithGap(fullBlocks = 5, headers = 8)

    // block at fullHeight + 1 is marked as invalid, its section records are corrupted
    val nextBlock = chain(5)
    history.historyStorage.insert(
      Array(history.validityKey(nextBlock.header.id) -> Array(0.toByte)),
      Array.empty[BlockSection]).get
    history.historyStorage
      .insert(nextBlock.blockTransactions.serializedId, Array.fill(100)(0.toByte)).get

    ErgoHistory.repairIfNeeded(history) shouldBe true

    // after repair, sections of the next block are requested for download
    val toDownload = history
      .nextModifiersToDownload(10, (tid, mid) =>
        !history.historyStorage.contains(mid) || history.historyStorage.modifierById(mid).isEmpty)
    val requestedIds = toDownload.values.flatten.toSeq
    requestedIds should contain(nextBlock.blockTransactions.id)
    requestedIds should contain(nextBlock.extension.id)
  }

}
