package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.mining.{CandidateGenerator, InputBlockFields}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessOrderingBlock
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncement
import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadRequest
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, StateType}
import org.ergoplatform.settings.Constants
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.{
  ErgoCorePropertyTest, HistoryTestHelpers, NodeViewTestConfig, NodeViewTestOps, RandomWrapper
}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genHeaderChain}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState, validFullBlock, validTransactionsFromBoxHolder
}
import org.ergoplatform.{ErgoBoxCandidate, Input, OrderingBlockFound}
import scorex.util.bytesToId

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class OrderingBlockReconstructionSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{
    defaultMinerSecret, defaultMinerSecretNumber, emptyProverResult, parameters, powScheme
  }

  private val timeout: FiniteDuration = 5.seconds

  Seq(
    ("normal", "reconstruct the parent's input chain before ordering transactions without downloading"),
    ("missing", "download the full body when the parent input chain body is missing"),
    ("longer", "rebuild the committed prefix when the follower's input chain has outrun it"),
    ("absent", "rebuild a block that commits no input chain while the follower holds one"),
    ("fallback", "rebuild a block whose extension names an input block but which commits none (emission-only fallback)"),
    ("unprocessed", "rebuild when the miner named an input block it had not yet processed"),
    ("fork", "rebuild from a named input block on a fork the follower does not rank best")
  ).foreach { case (scenario, description) =>
    val missingBody = scenario == "missing"
    val longerChain = scenario == "longer"
    property(description) {
      val fixture = new NodeViewFixture(
        NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings,
        parameters)
      import fixture._
      val initial = createUtxoState(fixture.settings)
      var state = initial._1
      var minerHistory = HistoryTestHelpers.generateHistory(
        verifyTransactions = true, stateType = StateType.Utxo,
        PoPoWBootstrap = false, blocksToKeep = 100)
      try {
        def build(parent: Option[ErgoFullBlock], boxes: BoxHolder,
                  seed: Int, time: Long): (ErgoFullBlock, BoxHolder) = {
          val (txs, nextBoxes) = validTransactionsFromBoxHolder(boxes, new RandomWrapper(Some(seed)))
          val block = validFullBlock(parent, state, txs, Some(time))
          state = state.applyModifier(block, None)(_ => ()).get
          applyBlock(block).isSuccess shouldBe true
          block -> nextBoxes
        }

        val start = System.currentTimeMillis() - 10000
        val (root, rootBoxes) = build(None, initial._2, 1, start)
        val (parent, parentBoxes) = build(Some(root), rootBoxes, 2, start + 1)
        minerHistory = applyChain(minerHistory, Seq(root, parent))
        val parentOutputIds = parent.transactions.flatMap(_.outputs).map(_.id.toSeq).toSet
        val input = parentBoxes.boxes.values.find { box =>
          box.ergoTree == Constants.TrueTree && parentOutputIds.contains(box.id.toSeq)
        }.get
        val inputTx = ErgoTransaction(
          IndexedSeq(Input(input.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(input.value, Constants.TrueTree,
            parent.height, input.additionalTokens)))
        inputTx.statelessValidity().get

        // Use the same real input-tree fixture as CandidateRetryReorgSpec.
        val inputHeader = genHeaderChain(1, minerHistory, diffBitsOpt = None, useRealTs = false).last
        val inputBlock = InputBlockAnnouncement(1, inputHeader, InputBlockFields.empty, None)
        val extraInput = inputTx.outputs.head
        val extraTx = ErgoTransaction(
          IndexedSeq(Input(extraInput.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(extraInput.value, Constants.TrueTree,
            parent.height, extraInput.additionalTokens)))
        val forkValue = extraInput.value / 2
        val forkTx = ErgoTransaction(
          IndexedSeq(Input(extraInput.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(
            new ErgoBoxCandidate(forkValue, Constants.TrueTree,
              parent.height, extraInput.additionalTokens),
            new ErgoBoxCandidate(extraInput.value - forkValue, Constants.TrueTree, parent.height)))
        def extraBlock(offset: Long): InputBlockAnnouncement = {
          val fields = InputBlockFields.empty
          val extraFields = new InputBlockFields(
            Some(inputHeader.serializedId), fields.transactionsDigest,
            fields.prevTransactionsDigest, fields.inputBlockFieldsProof)
          InputBlockAnnouncement(1, inputHeader.copy(timestamp = inputHeader.timestamp + offset),
            extraFields, None)
        }
        val nextInputBlock = extraBlock(1)
        val forkInputBlock = extraBlock(2)
        if (scenario != "absent") {
          minerHistory.applyInputBlock(inputBlock) shouldBe None
          minerHistory.applyInputBlockTransactions(inputBlock.id, Seq(inputTx), state)._1 should
            contain(inputBlock.id)
        }
        if (scenario == "unprocessed") {
          minerHistory.applyInputBlock(nextInputBlock) shouldBe None
        }
        if (scenario == "fork") {
          minerHistory.applyInputBlock(forkInputBlock) shouldBe None
          minerHistory.applyInputBlockTransactions(forkInputBlock.id, Seq(forkTx), state)._1 should
            contain(forkInputBlock.id)
        }
        val normalCandidate = CandidateGenerator.generateCandidate(
          minerHistory, state, ErgoMemPool.empty(fixture.settings),
          defaultMinerSecret.publicImage, Seq.empty, None, fixture.settings).get.get._1.candidateBlock
        val inputTxs = minerHistory.getCollectedInputBlocksTransactions(parent.id).getOrElse(Seq.empty)
        inputTxs.map(_.id) shouldBe (scenario match {
          case "absent" => Seq.empty
          case "fork" => Seq(inputTx.id, forkTx.id)
          case _ => Seq(inputTx.id)
        })
        val candidate = if (scenario == "fallback") {
          val ordering = CandidateGenerator.collectEmission(
            state, defaultMinerSecret.publicImage, state.stateContext).toSeq
          ordering should not be empty
          val (proof, digest) = state.proofsForTransactions(ordering).get
          normalCandidate.copy(transactions = ordering, orderingBlockTransactions = ordering,
            adProofBytes = proof, stateRoot = digest)
        } else normalCandidate
        val namedTip = candidate.extension.fields
          .find(_._1.sameElements(Extension.PrevInputBlockIdKey)).map(kv => bytesToId(kv._2))
        namedTip shouldBe (scenario match {
          case "absent" => None
          case "unprocessed" => Some(nextInputBlock.id)
          case "fork" => Some(forkInputBlock.id)
          case _ => Some(inputBlock.id)
        })
        candidate.orderingBlockTransactions should not be empty
        val committedInputTxs = if (scenario == "fallback") Seq.empty else inputTxs
        candidate.transactions.map(_.id) shouldBe
          (committedInputTxs ++ candidate.orderingBlockTransactions).map(_.id)
        val block = powScheme.proveBlock(
          candidate.parentOpt, candidate.version, candidate.nBits, candidate.stateRoot,
          candidate.adProofBytes, candidate.transactions, candidate.timestamp, candidate.extension,
          candidate.votes, defaultMinerSecretNumber, Long.MinValue, Long.MaxValue, parameters) match {
          case OrderingBlockFound(fullBlock) => fullBlock
          case other => fail(s"Expected a mined ordering block, got $other")
        }
        // Ensure reversing these nonempty groups really changes the committed root.
        if (committedInputTxs.nonEmpty) {
          BlockTransactions.transactionsRoot(
            candidate.orderingBlockTransactions ++ committedInputTxs, block.header.version).toSeq should not be
            block.header.transactionsRoot.toSeq
        }

        val followerHistory = getHistory
        followerHistory.applyInputBlock(inputBlock) shouldBe None
        if (!missingBody) {
          followerHistory.applyInputBlockTransactions(inputBlock.id, Seq(inputTx), getCurrentState)
            ._1 should contain(inputBlock.id)
          followerHistory.getCollectedInputBlocksTransactions(parent.id).get.map(_.id) shouldBe
            Seq(inputTx.id)
        } else {
          followerHistory.getCollectedInputBlocksTransactions(parent.id).get shouldBe empty
        }
        if (longerChain || scenario == "unprocessed" || scenario == "fork") {
          // The follower processes the competing or newer tip before the announcement.
          followerHistory.applyInputBlock(nextInputBlock) shouldBe None
          followerHistory.applyInputBlockTransactions(nextInputBlock.id, Seq(extraTx), getCurrentState)
            ._1 should contain(nextInputBlock.id)
          if (scenario == "fork") {
            followerHistory.applyInputBlock(forkInputBlock) shouldBe None
            followerHistory.applyInputBlockTransactions(forkInputBlock.id, Seq(forkTx), getCurrentState)
            followerHistory.getInputBlockTransactions(forkInputBlock.id).get.map(_.id) shouldBe
              Seq(forkTx.id)
          }
          val followerTxs = followerHistory.getCollectedInputBlocksTransactions(parent.id).get
          followerTxs.map(_.id) shouldBe Seq(inputTx.id, extraTx.id)
          BlockTransactions.transactionsRoot(
            followerTxs ++ candidate.orderingBlockTransactions, block.header.version).toSeq should not be
            block.header.transactionsRoot.toSeq
        }
        followerHistory.getCollectedInputBlocksTransactions(block.id) shouldBe None
        // Header synchronization independently schedules missing sections. Preload the
        // header before observing announcement reconstruction, without supplying a body.
        applyHeader(block.header).isSuccess shouldBe true
        getHistory.typedModifierById[BlockTransactions](block.header.transactionsId) shouldBe None
        val downloads = TestProbe()(actorSystem)
        downloads.ignoreMsg {
          case DownloadRequest(sections) =>
            !sections.get(BlockTransactions.modifierTypeId).exists(_.nonEmpty)
          case _ => false
        }
        actorSystem.eventStream.subscribe(downloads.ref, classOf[DownloadRequest])
        val announcement = OrderingBlockAnnouncement(
          1, block.header, candidate.orderingBlockTransactions, Seq.empty, block.extension.fields)
        val processing = TestProbe()(actorSystem)
        processing.send(nodeViewHolderRef, ProcessOrderingBlock(announcement))
        // A same-sender query is a barrier after announcement processing.
        processing.send(nodeViewHolderRef, GetDataFromCurrentView[ErgoState[_], Boolean](_ => true))
        processing.expectMsg(timeout, true)

        if (missingBody) {
          downloads.expectMsgType[DownloadRequest](timeout).modifiersToFetch shouldBe
            Map(BlockTransactions.modifierTypeId -> Seq(block.header.transactionsId))
          getHistory.typedModifierById[BlockTransactions](block.header.transactionsId) shouldBe None
        } else {
          downloads.expectNoMessage(500.millis)
          val reconstructed = getHistory
            .typedModifierById[BlockTransactions](block.header.transactionsId).get
          reconstructed.txs.map(_.id) shouldBe block.transactions.map(_.id)
          reconstructed.digest.toSeq shouldBe block.header.transactionsRoot.toSeq
          getBestFullBlockOpt.map(_.id) shouldBe Some(block.id)
        }
      } finally {
        minerHistory.closeStorage()
        state.closeStorage()
        Await.result(actorSystem.terminate(), 15.seconds)
      }
    }
  }
}
