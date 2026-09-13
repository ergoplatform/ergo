package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NetworkObjectTypeId, NonHeaderBlockSection}
import org.ergoplatform.modifiers.history.{BlockTransactions, HeaderChain}
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen
import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators.invalidErgoTransactionGen
import scorex.util.ModifierId

import scala.reflect.ClassTag
import scala.util.Try

class BlockDownloadSchedulingSpecification extends ErgoCorePropertyTest {

  // These in-memory records exercise scheduling, not block or chain validation.
  private val template = defaultHeaderGen.sample.get
  private val transaction = invalidErgoTransactionGen.sample.get
  private val maximumHeightHeader = template.copy(height = Int.MaxValue)
  private val bestChain = (1 to 300).foldLeft(Vector.empty[Header]) { (chain, height) =>
    chain :+ template.copy(height = height, parentId = chain.lastOption.map(_.id).getOrElse(Header.GenesisParentId))
  }
  private val oldChain = (21 to 60).foldLeft(bestChain.take(20)) { (chain, height) =>
    chain :+ template.copy(height = height, parentId = chain.last.id, timestamp = template.timestamp + 1)
  }

  private class SchedulingReader(fullHeader: Header,
                                 retainedVersions: Int = 50,
                                 minimumHeight: Int = 1,
                                 synced: Boolean = true,
                                 verify: Boolean = true,
                                 tipHeight: Int = 300,
                                 missingHeader: Option[ModifierId] = None) extends ErgoHistoryReader {
    override protected[history] val historyStorage: HistoryStorage = null
    override protected val settings: ErgoSettings = initSettings.copy(nodeSettings = initSettings.nodeSettings.copy(
      keepVersions = retainedVersions, verifyTransactions = verify, stateType = StateType.Utxo))
    override val powScheme: AutolykosPowScheme = null
    override protected def requireProofs: Boolean = false
    override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
      fail("Scheduling snapshots do not process sections")
    override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
      fail("Scheduling snapshots do not validate sections")

    private val headersById = (bestChain ++ oldChain :+ maximumHeightHeader).map(h => h.id -> h).toMap
    var traversals: Vector[(Int, ModifierId)] = Vector.empty
    var heightLookups: Vector[Int] = Vector.empty
    override def bestFullBlockOpt: Option[ErgoFullBlock] = Some(ErgoFullBlock(fullHeader,
      BlockTransactions(fullHeader.id, fullHeader.version, Seq(transaction)), Extension(fullHeader.id, Seq.empty), None))
    override def bestHeaderIdOpt: Option[ModifierId] = Some(bestChain(tipHeight - 1).id)
    override def estimatedTip(): Option[Int] = Some(tipHeight)
    override def minimalFullBlockHeight: Int = minimumHeight
    override def isHeadersChainSynced: Boolean = synced
    override def isInBestChain(id: ModifierId): Boolean = bestChain.exists(_.id == id)
    override def headerIdsAtHeight(height: Int): Seq[ModifierId] = {
      heightLookups :+= height
      (bestChain :+ maximumHeightHeader).find(_.height == height).map(_.id).toSeq
    }
    override def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T] =
      headersById.get(id).filterNot(h => missingHeader.contains(h.id)).collect { case section: T => section }
    override def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
      traversals :+= limit -> startHeader.id
      super.headerChainBack(limit, startHeader, until)
    }
  }

  private def expected(reader: SchedulingReader, heights: Range): Map[NetworkObjectTypeId.Value, Seq[ModifierId]] =
    heights.flatMap(h => reader.requiredModifiersForHeader(bestChain(h - 1)))
      .groupBy(_._1).map { case (kind, sections) => kind -> sections.map(_._2) }

  private val acceptAll: (NetworkObjectTypeId.Value, ModifierId) => Boolean = (_, _) => true

  property("linear far-behind downloads retain the forward window without walking history") {
    val reader = new SchedulingReader(bestChain(59))
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 61 to 252)
    reader.traversals shouldBe empty
  }

  property("ancestor scheduling extends backward without shortening the forward window") {
    val reader = new SchedulingReader(oldChain.last, retainedVersions = 40)
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 21 to 252)
    reader.traversals shouldBe Vector(41 -> oldChain.last.id)
  }

  property("a truncated traversal does not infer a common ancestor") {
    val reader = new SchedulingReader(oldChain.last, retainedVersions = 39)
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 61 to 252)
    reader.traversals shouldBe Vector(40 -> oldChain.last.id)
  }

  property("non-positive retention does not walk to a parent") {
    Seq(0, -1, Int.MinValue).foreach { retained =>
      val reader = new SchedulingReader(oldChain.last, retainedVersions = retained)
      reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 61 to 252)
      reader.traversals shouldBe Vector(1 -> oldChain.last.id)
    }
  }

  property("a missing parent does not turn a partial traversal into a common ancestor") {
    val reader = new SchedulingReader(oldChain.last, missingHeader = Some(oldChain(39).id))
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 61 to 252)
    reader.traversals shouldBe Vector(51 -> oldChain.last.id)
  }

  property("ancestor scheduling respects the minimum retained full-block height") {
    Seq(30, 253).foreach { minimumHeight =>
      val reader = new SchedulingReader(oldChain.last, minimumHeight = minimumHeight)
      reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, minimumHeight to 252)
    }
  }

  property("maximum retention does not overflow the traversal bound") {
    val reader = new SchedulingReader(oldChain.last, retainedVersions = Int.MaxValue)
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 21 to 252)
    reader.traversals shouldBe Vector(60 -> oldChain.last.id)
  }

  property("a download window ending at the maximum height terminates without wrapping") {
    val fullHeader = oldChain.last.copy(height = Int.MaxValue - 191)
    val reader = new SchedulingReader(fullHeader, minimumHeight = Int.MaxValue) {
      override def estimatedTip(): Option[Int] = Some(Int.MaxValue)
    }
    val sections = reader.requiredModifiersForHeader(maximumHeightHeader)
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe
      sections.groupBy(_._1).map { case (kind, entries) => kind -> entries.map(_._2) }
    reader.heightLookups shouldBe Vector(Int.MaxValue)
  }

  property("ancestor scheduling preserves per-type caps and the section filter") {
    val reader = new SchedulingReader(oldChain.last)
    val firstSections = reader.requiredModifiersForHeader(bestChain(20))
    val excluded = firstSections.head._2
    val result = reader.nextModifiersToDownload(2, (_, id) => id != excluded)
    val all = expected(reader, 21 to 22)
    result shouldBe all.map { case (kind, ids) => kind -> ids.filterNot(_ == excluded) }
    result.values.foreach(_.size should be <= 2)
  }

  property("unsynced and non-verifying readers do not schedule sections or walk history") {
    Seq(new SchedulingReader(oldChain.last, synced = false), new SchedulingReader(oldChain.last, verify = false))
      .foreach { reader =>
        reader.nextModifiersToDownload(1000, acceptAll) shouldBe empty
        reader.traversals shouldBe empty
      }
  }

  property("near-tip scheduling retains its existing lookback without walking history") {
    val reader = new SchedulingReader(oldChain.last, tipHeight = 100)
    reader.nextModifiersToDownload(1000, acceptAll) shouldBe expected(reader, 1 to 300)
    reader.traversals shouldBe empty
  }
}
