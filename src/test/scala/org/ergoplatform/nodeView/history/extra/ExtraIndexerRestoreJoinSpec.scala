package org.ergoplatform.nodeView.history.extra

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import scorex.util.{ModifierId, bytesToId}
import sigma.ast.ErgoTree

import scala.concurrent.duration._

class ExtraIndexerRestoreJoinSpec extends ErgoCorePropertyTest with NodeViewTestOps {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.ErgoCoreTestConstants.{defaultProver, emptyDataBoxes, emptyProverResult}

  private case class RollBackTo(header: Header)
  private case object IndexerReady

  Seq(false, true).foreach { rent =>
    property(s"later founders spend reopens and rolls back with rent collection $rent") {
      val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
      val indexed = base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true, storageRentCollection = rent))
      new NodeViewFixture(indexed, parameters).apply { fixture =>
        import fixture._
        val (sourceState, boxes) = createUtxoState(settings)
        try {
          val first = validFullBlock(None, sourceState, boxes)
          applyBlock(first).get
          val afterFirst = sourceState.applyModifier(first, None)(_ => ()).get
          val founders = ErgoState.genesisBoxes(settings.chainSettings).last
          val height = afterFirst.stateContext.currentHeight
          val remaining = emission.remainingFoundationRewardAtHeight(height)
          val unsigned = new UnsignedErgoTransaction(
            IndexedSeq(Input(founders.id, emptyProverResult)), IndexedSeq.empty,
            IndexedSeq(
              new ErgoBoxCandidate(remaining, founders.ergoTree, height, founders.additionalTokens, founders.additionalRegisters),
              new ErgoBoxCandidate(founders.value - remaining,
                ErgoTree.fromSigmaBoolean(defaultProver.hdKeys.last.publicImage), height)))
          val tx = ErgoTransaction(defaultProver.sign(unsigned, IndexedSeq(founders), emptyDataBoxes, afterFirst.stateContext).get)
          val second = validFullBlock(Some(first), afterFirst, Seq(tx))
          applyBlock(second).get
          org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
            getCurrentState.version shouldBe idToVersion(second.id)
          }
          def startIndexer() = actorSystem.actorOf(Props(new ExtraIndexer(
            settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), rent) {
            override implicit val segmentThreshold: Int = 1
            override def receive: Receive = ({
              case IndexerReady => sender().tell(_history != null, this.self)
            }: Receive).orElse(super.receive)
            override protected def loaded(state: IndexerState): Receive = ({
              case RollBackTo(header) => beginRollback(state, header, resume = false)
            }: Receive).orElse(super.loaded(state))
          }))
          val probe = TestProbe()(actorSystem)
          var indexer = startIndexer()
          probe.watch(indexer)
          org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
            IndexerState.fromHistory(getHistory).indexedHeight shouldBe 2
          }
          actorSystem.stop(indexer)
          probe.expectTerminated(indexer, 10.seconds)
          stopNodeViewHolder()
          startNodeViewHolder()
          IndexerState.fromHistory(getHistory).indexedHeight shouldBe 2
          val foundersId = bytesToId(founders.id)
          val stored = getHistory.typedExtraIndexById[IndexedErgoTransaction](tx.id).get
          stored.retrieveBody(getHistory.getReader).inputs.map(_.id) shouldBe IndexedSeq(foundersId)
          stored.inputNums.toSeq shouldBe Seq(3L)
          val addressId = IndexedErgoAddressSerializer.hashErgoTree(founders.ergoTree)
          getHistory.typedExtraIndexById[IndexedErgoAddress](addressId).get.balanceInfo.get.nanoErgs shouldBe remaining
          getHistory.typedExtraIndexById[IndexedErgoAddress](addressId).get.boxSegmentCount shouldBe 1
          indexer = startIndexer()
          probe.watch(indexer)
          probe.awaitAssert({
            probe.send(indexer, IndexerReady)
            probe.expectMsg(200.millis, true)
          }, 10.seconds, 50.millis)
          indexer ! RollBackTo(first.header)
          org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
            IndexerState.fromHistory(getHistory).indexedHeight shouldBe 1
          }
          actorSystem.stop(indexer)
          probe.expectTerminated(indexer, 10.seconds)
          val restored = NumericBoxIndex.getBoxByNumber(getHistory, 3L).get
          restored.id shouldBe foundersId
          restored.isSpent shouldBe false
          val address = getHistory.typedExtraIndexById[IndexedErgoAddress](addressId).get
          address.balanceInfo.get.nanoErgs shouldBe founders.value
          address.boxes should contain(3L)
          address.retrieveBoxes(getHistory.getReader, 0, 10)(1).map(_.id) should contain(foundersId)
          getHistory.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue).exists(_.globalIndex == 3L) shouldBe rent
          getHistory.typedExtraIndexById[IndexedErgoTransaction](tx.id) shouldBe None
          stopNodeViewHolder()
          startNodeViewHolder()
          IndexerState.fromHistory(getHistory).indexedHeight shouldBe 1
          NumericBoxIndex.getBoxByNumber(getHistory, 3L).get.isSpent shouldBe false
        } finally sourceState.closeStorage()
      }
    }
  }

  property("configured genesis inputs retain their identity through indexing and reopen") {
    val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val indexed = base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true))
    new NodeViewFixture(indexed, parameters).apply { fixture =>
      import fixture._
      val (sourceState, boxes) = createUtxoState(settings)
      try {
        val block = validFullBlock(None, sourceState, boxes)
        applyBlock(block).get
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          getCurrentState.version shouldBe idToVersion(block.id)
        }
        val indexer = actorSystem.actorOf(Props(new ExtraIndexer(
          settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true)))
        val probe = TestProbe()(actorSystem)
        probe.watch(indexer)
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          IndexerState.fromHistory(getHistory).indexedHeight shouldBe 1
        }
        val actualInputs = block.transactions.map { tx =>
          getHistory.typedExtraIndexById[IndexedErgoTransaction](tx.id).get
            .retrieveBody(getHistory.getReader).inputs.map(_.id)
        }
        actorSystem.stop(indexer)
        probe.expectTerminated(indexer, 10.seconds)
        val before = IndexerState.fromHistory(getHistory)
        stopNodeViewHolder()
        startNodeViewHolder()
        IndexerState.fromHistory(getHistory) shouldBe before
        actualInputs shouldBe block.transactions.map(_.inputs.map(i => bytesToId(i.boxId)))
        block.transactions.foreach { tx =>
          val stored = getHistory.typedExtraIndexById[IndexedErgoTransaction](tx.id).get
          stored.inputNums.forall(_ > 0L) shouldBe true
          stored.retrieveBody(getHistory.getReader).inputs.map(_.id) shouldBe tx.inputs.map(i => bytesToId(i.boxId))
        }
      } finally sourceState.closeStorage()
    }
  }

  property("extra indexer indexes a block with missing validity mark during catch-up after restart") {
    val baseSettings = NodeViewTestConfig(
      StateType.Utxo,
      verifyTransactions = true,
      popowBootstrap = false
    ).toSettings
    val indexedSettings: ErgoSettings = baseSettings.copy(
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true)
    )

    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(settings)
      try {
        val block = validFullBlock(None, sourceState, boxHolder)
        applyBlock(block).get
        org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
          getCurrentState.version shouldBe idToVersion(block.id)
        }
        getHistory.isSemanticallyValid(block.header.id) shouldBe ModifierSemanticValidity.Valid

        getHistory.historyStorage.remove(
          Array(getHistory.validityKey(block.header.id)),
          Array.empty[ModifierId]
        ).get
        getHistory.isSemanticallyValid(block.header.id) shouldBe ModifierSemanticValidity.Unknown

        stopNodeViewHolder()
        startNodeViewHolder()
        // no repair during startup: the validity row is still missing
        getHistory.isSemanticallyValid(block.header.id) shouldBe ModifierSemanticValidity.Unknown

        val indexer = actorSystem.actorOf(Props(new ExtraIndexer(
          settings.cacheSettings,
          settings.chainSettings.addressEncoder,
          Some(nodeViewHolderRef),
          true
        )))
        try {
          org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
            IndexerState.fromHistory(getHistory).indexedHeight shouldBe block.header.height
          }
          // the indexer does not gate on semantic validity: the block was indexed
          // with its validity mark still missing
          getHistory.isSemanticallyValid(block.header.id) shouldBe ModifierSemanticValidity.Unknown
        } finally {
          actorSystem.stop(indexer)
        }
      } finally {
        sourceState.closeStorage()
      }
    }
  }
}
