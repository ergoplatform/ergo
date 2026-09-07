package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyHeaderChain, genHeaderChain}
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

import scala.util.Try

class SyncInfoV2HistorySpecification extends ErgoCorePropertyTest {

  private def newHistory(): ErgoHistory =
    generateHistory(
      verifyTransactions = false,
      stateType = StateType.Digest,
      PoPoWBootstrap = false,
      blocksToKeep = 0,
      epochLength = 1000
    )

  private def checkRoundtrip(summary: ErgoSyncInfoV2): Unit = {
    val bytes = ErgoSyncInfoSerializer.toBytes(summary)
    val decoded = ErgoSyncInfoSerializer.parseBytes(bytes).asInstanceOf[ErgoSyncInfoV2]
    decoded.lastHeaders.map(_.id) shouldBe summary.lastHeaders.map(_.id)
    decoded.height shouldBe summary.height
    ErgoSyncInfoSerializer.toBytes(decoded).sameElements(bytes) shouldBe true
  }

  // These snapshots exercise summary selection, not header or chain validity.
  private def snapshotReader(headers: Seq[Header]): ErgoHistoryReader = new ErgoHistoryReader {
    override protected[history] val historyStorage: HistoryStorage = null
    override protected val settings: ErgoSettings = null
    override val powScheme: AutolykosPowScheme = null
    override protected def requireProofs: Boolean = false
    override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
      fail("Summary snapshots do not process block sections")
    override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
      fail("Summary snapshots do not validate block sections")
    override def bestHeaderOpt: Option[Header] = headers.sortBy(_.height).lastOption
    override def headersHeight: Int = bestHeaderOpt.map(_.height).getOrElse(0)
    override def isEmpty: Boolean = headers.isEmpty
    override def bestHeaderAtHeight(height: Int): Option[Header] = headers.find(_.height == height)
  }

  property("full and reduced sync summaries remain empty for empty history") {
    val history = newHistory()
    try {
      Seq(true, false).foreach { full =>
        val summary = history.syncInfoV2(full)
        summary.lastHeaders shouldBe empty
        summary.nonEmpty shouldBe false
        summary.height shouldBe None
        checkRoundtrip(summary)
      }
    } finally {
      history.closeStorage()
    }
  }

  property("linear history summaries retain recent samples and exactly one genesis anchor") {
    var history = newHistory()
    try {
      val chain = genHeaderChain(17, history, diffBitsOpt = None, useRealTs = false)
      val expectedHeights = Seq(
        1 -> Seq(1),
        2 -> Seq(2, 1),
        10 -> Seq(10, 1),
        17 -> Seq(17, 1)
      )
      var appliedHeight = 0
      expectedHeights.foreach { case (height, heights) =>
        history = applyHeaderChain(history, HeaderChain(chain.headers.slice(appliedHeight, height)))
        appliedHeight = height
        history.headersHeight shouldBe height

        val full = history.syncInfoV2(full = true)
        val expectedIds = heights.map(h => chain.headers(h - 1).id)
        full.lastHeaders.map(_.id) shouldBe expectedIds
        full.lastHeaders.map(_.height) shouldBe heights
        full.lastHeaders.head.id shouldBe history.bestHeaderOpt.get.id
        full.lastHeaders.last.id shouldBe chain.head.id
        full.lastHeaders.count(_.id == chain.head.id) shouldBe 1
        full.lastHeaders.map(_.id).distinct.size shouldBe full.lastHeaders.size
        full.lastHeaders.size should be <= 5
        full.lastHeaders.size should be <= ErgoSyncInfoSerializer.MaxHeadersAllowed
        full.height shouldBe Some(height)
        checkRoundtrip(full)

        val reduced = history.syncInfoV2(full = false)
        reduced.lastHeaders.map(_.id) shouldBe Seq(chain.headers(height - 1).id)
        reduced.height shouldBe Some(height)
        checkRoundtrip(reduced)
      }
    } finally {
      history.closeStorage()
    }
  }

  property("full summary snapshots preserve sparse samples and deduplicate the genesis anchor") {
    val template = defaultHeaderGen.sample.get
    val cases = Seq(
      Seq(129, 113, 1),
      Seq(513, 497, 385, 1),
      Seq(600, 584, 472, 88, 1)
    )
    cases.foreach { heights =>
      val headers = heights.map(height => template.copy(height = height))
      val reader = snapshotReader(headers)
      val full = reader.syncInfoV2(full = true)
      full.lastHeaders.map(_.id) shouldBe headers.map(_.id)
      full.lastHeaders.map(_.height) shouldBe heights
      full.lastHeaders.count(_.height == 1) shouldBe 1
      full.lastHeaders.map(_.id).distinct.size shouldBe full.lastHeaders.size
      full.lastHeaders.size should be <= 5
      full.lastHeaders.size should be <= ErgoSyncInfoSerializer.MaxHeadersAllowed
      full.height shouldBe Some(heights.head)
      checkRoundtrip(full)

      val reduced = reader.syncInfoV2(full = false)
      reduced.lastHeaders.map(_.id) shouldBe Seq(headers.head.id)
      checkRoundtrip(reduced)
    }
  }

  property("full summary preserves available samples when genesis is unavailable") {
    val template = defaultHeaderGen.sample.get
    val headers = Seq(600, 584, 472, 88).map(height => template.copy(height = height))
    val reader = snapshotReader(headers)
    reader.bestHeaderAtHeight(1) shouldBe None
    val full = reader.syncInfoV2(full = true)
    full.lastHeaders.map(_.id) shouldBe headers.map(_.id)
    full.height shouldBe Some(600)
    checkRoundtrip(full)
    val reduced = reader.syncInfoV2(full = false)
    reduced.lastHeaders.map(_.id) shouldBe Seq(headers.head.id)
    checkRoundtrip(reduced)
  }
}
