package org.ergoplatform.nodeView.viewholder

import org.ergoplatform.consensus.ModifierSemanticValidity.Valid
import org.ergoplatform.core.idToVersion
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators._
import scorex.util.encode.Base16

class ShorterHeavierHolderRestartSpecification extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  property("holder restart retains an applied taller fork after a shorter heavier switch") {
    val initialDifficulty = BigInt(1)
    val baseSettings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
      popowBootstrap = false).toSettings
    val settings = baseSettings.copy(
      chainSettings = baseSettings.chainSettings.copy(
        epochLength = 3,
        useLastEpochs = 3,
        initialDifficultyHex = Base16.encode(initialDifficulty.toByteArray)
      ),
      nodeSettings = baseSettings.nodeSettings.copy(
        blocksToKeep = 100,
        keepVersions = 100,
        extraIndex = false
      )
    )

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      val (initialState, boxes) = createUtxoState(fixture.settings)
      var producerState = WrappedUtxoState(initialState, boxes, fixture.settings)
      val interval = getHistory.difficultyCalculator.desiredInterval.toMillis
      val baseTime = System.currentTimeMillis() - 5 * interval

      def append(parent: Option[ErgoFullBlock], halfIntervals: Long): ErgoFullBlock = {
        val timestamp = baseTime + halfIntervals * interval / 2
        val transactions = CandidateGenerator.collectRewards(
          producerState.emissionBoxOpt,
          producerState.stateContext.currentHeight,
          Seq.empty,
          defaultMinerPk,
          producerState.stateContext
        )
        transactions.size shouldBe 1
        val candidate = validFullBlock(parent, producerState, transactions, Some(timestamp))
        val nBits = parent match {
          case Some(p) => DifficultySerializer.encodeCompactBits(getHistory.requiredDifficultyAfter(p.header))
          case None => DifficultySerializer.encodeCompactBits(initialDifficulty)
        }
        val block = powScheme.proveBlock(
          parent.map(_.header), Header.InitialVersion, nBits,
          candidate.header.stateRoot, candidate.adProofs.get.proofBytes,
          candidate.blockTransactions.txs, timestamp,
          ExtensionCandidate(candidate.extension.fields), candidate.header.votes,
          defaultMinerSecretNumber
        ).get
        producerState = producerState.applyModifier(block)(_ => ()).get
        applyBlock(block).isSuccess shouldBe true
        block
      }

      val genesis = append(None, 0)
      val a1 = append(Some(genesis), 2)
      val a2 = append(Some(a1), 4)
      val a3 = append(Some(a2), 6)
      val a4 = append(Some(a3), 8)
      val a5 = append(Some(a4), 12)
      val a6 = append(Some(a5), 14)
      val a7 = append(Some(a6), 16)
      val a8 = append(Some(a7), 18)

      getHistory.bestFullBlockIdOpt shouldBe Some(a8.id)
      getCurrentState.version shouldBe idToVersion(a8.id)
      getHistory.isSemanticallyValid(a8.id) shouldBe Valid

      producerState = producerState.rollbackTo(idToVersion(genesis.id)).get
      val b1 = append(Some(genesis), 1)
      val b2 = append(Some(b1), 2)
      val b3 = append(Some(b2), 3)
      val b4 = append(Some(b3), 4)
      val b5 = append(Some(b4), 5)
      val b6 = append(Some(b5), 6)
      val b7 = append(Some(b6), 7)

      b7.height shouldBe a8.height - 1
      getHistory.scoreOf(b7.id).get should be > getHistory.scoreOf(a8.id).get
      getHistory.bestHeaderIdOpt shouldBe Some(b7.id)
      getHistory.bestFullBlockIdOpt shouldBe Some(b7.id)
      getCurrentState.version shouldBe idToVersion(b7.id)
      getHistory.isSemanticallyValid(a8.id) shouldBe Valid
      getHistory.headerIdsAtHeight(a8.height) should contain(a8.id)
      getHistory.modifierById(a8.id).map(_.modifierTypeId) shouldBe
        Some(Header.modifierTypeId)
      info(s"pre-restart: Holder applied A8 ${a8.id} and B7 ${b7.id}; " +
        "A8 is marked valid and stored")

      stopNodeViewHolder()
      startNodeViewHolder()

      getHistory.bestFullBlockIdOpt shouldBe Some(b7.id)
      getCurrentState.version shouldBe idToVersion(b7.id)
      val reopened = getHistory
      val rowPresent = reopened.headerIdsAtHeight(a8.height).contains(a8.id)
      val storedType = reopened.modifierById(a8.id).map(_.modifierTypeId)
      val validity = reopened.isSemanticallyValid(a8.id)
      info(s"post-restart: row=$rowPresent, stored type=$storedType, validity=$validity")
      rowPresent shouldBe true
      storedType shouldBe Some(Header.modifierTypeId)
      validity shouldBe Valid
    }
  }
}
