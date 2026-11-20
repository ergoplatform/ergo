package org.ergoplatform.mining

import org.ergoplatform._
import org.ergoplatform.core.idToVersion
import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{BoxHolder, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, MonetarySettings, Parameters}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.{ModifierId, bytesToId}
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast._
import sigma.data.Digest32Coll
import sigma.interpreter.ProverResult

class ReemissionRewardCollectionSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  // Compact Mainnet-accurate EIP-27 schedule (`foundersInitialReward = 2.5 * oneEpochReduction`):
  //   h = 1:    16.5 ERG miner reward (12 ERG re-emitted)
  //   h = 2..4: 17.25, 18.0, 15.0 ERG miner reward (12 ERG re-emitted per block)
  //   h = 5..7: 12.0, 9.0, 6.0 ERG miner reward (9, 6, 3 ERG re-emitted)
  //   h = 8:    3.0 ERG miner reward (0 re-emitted; final original emission block via `lastCoins`)
  //   h >= 9:   original emission = 0, reemissionStartHeight = 9 (3 ERG/block from re-emission box)
  private val customMonetary = MonetarySettings(
    fixedRatePeriod = 2,
    epochLength = 1,
    fixedRate = 24L * CoinsInOneErgo,
    oneEpochReduction = 3L * CoinsInOneErgo,
    minerRewardDelay = 5,
    foundersInitialReward = 7500000000L
  )

  private val emissionNftBytes: Array[Byte] = Array.fill(32)(0x11.toByte)
  private val reemissionTokenBytes: Array[Byte] = Array.fill(32)(0x22.toByte)
  private val reemissionNftBytes: Array[Byte] = Array.fill(32)(0x33.toByte)

  private val emissionNftId: ModifierId = bytesToId(emissionNftBytes)
  private val reemissionTokenId: ModifierId = bytesToId(reemissionTokenBytes)
  private val reemissionNftId: ModifierId = bytesToId(reemissionNftBytes)

  // Injection box script: allows spending alongside input(0) (the emission box).
  private val injectionScript: ErgoTree = ErgoTree.fromProposition(
    GT(ExtractAmount(ByIndex(Inputs, IntConstant(0))), LongConstant(CoinsInOneErgo)).toSigmaProp
  )

  private val injectionBox: ErgoBox = new ErgoBox(
    value = 1000000L,
    ergoTree = injectionScript,
    additionalTokens = Colls.fromItems(
      (Digest32Coll @@ Colls.fromArray(emissionNftBytes)) -> 1L,
      (Digest32Coll @@ Colls.fromArray(reemissionTokenBytes)) -> (200L * CoinsInOneErgo)
    ),
    additionalRegisters = Map.empty,
    transactionId = bytesToId(Array.fill(32)(0x44.toByte)),
    index = 0: Short,
    creationHeight = 0
  )

  private val v2Parameters: Parameters = new Parameters(
    0,
    parameters.parametersTable + (Parameters.BlockVersion -> Header.HardeningVersion.toInt),
    emptyVSUpdate
  )

  private def mkReemissionSettings(reemissionStartHeight: Int): ErgoSettings = {
    val baseChain = settings.chainSettings
    val updatedReemission = baseChain.reemission.copy(
      checkReemissionRules = true,
      emissionNftId = emissionNftId,
      reemissionTokenId = reemissionTokenId,
      reemissionNftId = reemissionNftId,
      activationHeight = 1,
      reemissionStartHeight = reemissionStartHeight,
      injectionBoxBytesEncoded = Base16.encode(ErgoBoxSerializer.toBytes(injectionBox))
    )
    settings.copy(
      chainSettings = baseChain.copy(
        monetary = customMonetary,
        reemission = updatedReemission
      )
    )
  }

  private def mkBlock(parentOpt: Option[ErgoFullBlock],
                      state: UtxoState,
                      txs: Seq[ErgoTransaction],
                      ergoSettings: ErgoSettings): ErgoFullBlock = {
    val (adProofBytes, updStateDigest) = state.proofsForTransactions(txs).get
    val time = parentOpt.map(_.header.timestamp + 1).getOrElse(System.currentTimeMillis())
    val algos = new NipopowAlgos(ergoSettings.chainSettings)
    val interlinks = parentOpt.toSeq.flatMap { block =>
      algos.updateInterlinks(block.header, NipopowAlgos.unpackInterlinks(block.extension.fields).get)
    }
    val extension: ExtensionCandidate =
      v2Parameters.toExtensionCandidate ++
        algos.interlinksToExtension(interlinks) ++
        state.stateContext.validationSettings.toExtensionCandidate
    val votes = Array.fill(3)(0: Byte)
    ergoSettings.chainSettings.powScheme.proveBlock(
      parentOpt.map(_.header),
      Header.HardeningVersion,
      ergoSettings.chainSettings.initialNBits,
      updStateDigest,
      adProofBytes,
      txs,
      time,
      extension,
      votes,
      defaultMinerSecretNumber
    ).get
  }

  private def mkUpcomingContext(parentOpt: Option[ErgoFullBlock], state: UtxoState) = {
    val time = parentOpt.map(_.header.timestamp + 1).getOrElse(System.currentTimeMillis())
    state.stateContext.upcoming(
      defaultMinerPk.value,
      time,
      state.stateContext.currentParameters.blockVersion,
      Array.fill(3)(0: Byte),
      emptyVSUpdate,
      Header.HardeningVersion
    )
  }

  property("collect rewards across emission depletion into EIP-27 re-emission with checkReemissionRules = true") {
    val s = mkReemissionSettings(reemissionStartHeight = 9)
    val cs = s.chainSettings
    val rr = cs.reemission.reemissionRules
    val reemissionProp = rr.reemissionBoxProp(cs.monetary)
    val minerPropBytes = ErgoTreePredef.rewardOutputScript(cs.monetary.minerRewardDelay, defaultMinerPk).bytes

    cs.emissionRules.blocksTotal shouldBe 8
    val totalMinersCoins = cs.emissionRules.minersCoinsTotal
    totalMinersCoins shouldBe (94500000000L) // 94.5 ERG

    val genesisEmission = new ErgoBox(
      value = totalMinersCoins,
      ergoTree = cs.monetary.emissionBoxProposition,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = ErgoBox.allZerosModifierId,
      index = 0: Short,
      creationHeight = 0
    )
    val initialReemissionBox = new ErgoBox(
      value = 10L * CoinsInOneErgo,
      ergoTree = reemissionProp,
      additionalTokens = Colls.fromItems((Digest32Coll @@ cs.reemission.reemissionNftIdBytes) -> 1L),
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(0x55.toByte)),
      index = 0: Short,
      creationHeight = 0
    )

    val bh = BoxHolder(Seq(genesisEmission, injectionBox, initialReemissionBox))
    var state = UtxoState.fromBoxHolder(bh, Some(genesisEmission), createTempDir, s, v2Parameters)
    var parentOpt: Option[ErgoFullBlock] = None
    state.emissionBoxOpt.map(_.value) shouldBe Some(totalMinersCoins)

    // Blocks 1..7:
    // - Block 1 (activationHeight = 1) injects NFT + 200 ERG re-emission tokens into emissionBox
    // - Blocks 2..7 continue charging re-emission tokens while emissionBox < 100k ERG
    //   and leave surplus unissued re-emission tokens in emissionBox
    (1 to 7).foreach { h =>
      val tx = CandidateGenerator.collectEmission(state, defaultMinerPk, mkUpcomingContext(parentOpt, state)).get
      val block = mkBlock(parentOpt, state, Seq(tx), s)
      block.height shouldBe h
      state = state.applyModifier(block, None)(_ => ()).get
      parentOpt = Some(block)
    }
    state.emissionBoxOpt.map(_.value) shouldBe Some(3L * CoinsInOneErgo)

    // Block 8 (height 8 == blocksTotal): final 3 ERG in emissionBox is depleted via `lastCoins`; emissionBoxOpt -> None
    val tx8 = CandidateGenerator.collectEmission(state, defaultMinerPk, mkUpcomingContext(parentOpt, state)).get
    tx8.outputs.length shouldBe 2
    tx8.outputs.map(_.value).sum shouldBe 3L * CoinsInOneErgo
    tx8.outputs.forall(o => java.util.Arrays.equals(o.propositionBytes, minerPropBytes)) shouldBe true
    tx8.outputs.forall(_.additionalTokens.isEmpty) shouldBe true
    val b8 = mkBlock(parentOpt, state, Seq(tx8), s)
    val us8 = state.applyModifier(b8, None)(_ => ()).get
    us8.emissionBoxOpt shouldBe None
    us8.reemissionBoxOpt.map(_.value) shouldBe Some(10L * CoinsInOneErgo)

    // Block 9 (height 9 == reemissionStartHeight): collects 3 ERG from re-emission box; wrong miner pk is rejected
    val upcoming9 = mkUpcomingContext(Some(b8), us8)
    val wrongPk = proveDlogGen.sample.get
    val badTx9 = CandidateGenerator.collectEmission(us8, wrongPk, upcoming9).get
    us8.applyModifier(mkBlock(Some(b8), us8, Seq(badTx9), s), None)(_ => ()).isFailure shouldBe true

    val tx9 = CandidateGenerator.collectEmission(us8, defaultMinerPk, upcoming9).get
    tx9.outputs.length shouldBe 2
    tx9.outputs(0).value shouldBe 7L * CoinsInOneErgo
    tx9.outputs(0).ergoTree shouldBe reemissionProp
    tx9.outputs(1).value shouldBe rr.reemissionRewardPerBlock
    tx9.outputs(1).propositionBytes shouldEqual minerPropBytes
    val b9 = mkBlock(Some(b8), us8, Seq(tx9), s)
    val us9 = us8.applyModifier(b9, None)(_ => ()).get
    us9.reemissionBoxOpt.map(_.value) shouldBe Some(7L * CoinsInOneErgo)

    // Block 10 (height 10): collects another 3 ERG -> 4 ERG remaining
    val tx10 = CandidateGenerator.collectEmission(us9, defaultMinerPk, mkUpcomingContext(Some(b9), us9)).get
    val b10 = mkBlock(Some(b9), us9, Seq(tx10), s)
    val us10 = us9.applyModifier(b10, None)(_ => ()).get
    us10.reemissionBoxOpt.map(_.value) shouldBe Some(4L * CoinsInOneErgo)

    // Block 11 (height 11): collects another 3 ERG -> 1 ERG remaining
    val tx11 = CandidateGenerator.collectEmission(us10, defaultMinerPk, mkUpcomingContext(Some(b10), us10)).get
    val b11 = mkBlock(Some(b10), us10, Seq(tx11), s)
    val us11 = us10.applyModifier(b11, None)(_ => ()).get
    us11.reemissionBoxOpt.map(_.value) shouldBe Some(1L * CoinsInOneErgo)

    // Block 12 (height 12): only 1 ERG left (< 3 ERG reward + min box value) -> returns None cleanly
    CandidateGenerator.collectEmission(us11, defaultMinerPk, mkUpcomingContext(Some(b11), us11)) shouldBe None
  }

  property("reemissionBoxOpt updates across payToReemission merge and rolls back on reorg") {
    val s = mkReemissionSettings(reemissionStartHeight = 2)
    val cs = s.chainSettings
    val rr = cs.reemission.reemissionRules
    val reemissionProp = rr.reemissionBoxProp(cs.monetary)
    val pay2ReemissionProp = rr.payToReemission
    val nftAsset = Colls.fromItems((Digest32Coll @@ cs.reemission.reemissionNftIdBytes) -> 1L)

    val initialReemissionBox = new ErgoBox(
      value = 5L * CoinsInOneErgo,
      ergoTree = reemissionProp,
      additionalTokens = nftAsset,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(0x61.toByte)),
      index = 0: Short,
      creationHeight = 0
    )
    val pay2ReemissionBox = new ErgoBox(
      value = 4L * CoinsInOneErgo,
      ergoTree = pay2ReemissionProp,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(0x62.toByte)),
      index = 0: Short,
      creationHeight = 0
    )

    val us0 = UtxoState.fromBoxHolder(
      BoxHolder(Seq(initialReemissionBox, pay2ReemissionBox)),
      None,
      createTempDir,
      s,
      v2Parameters
    )
    val v0 = us0.version
    us0.reemissionBoxOpt.map(_.id.toSeq) shouldBe Some(initialReemissionBox.id.toSeq)

    // Block 1 (height 1 < reemissionStartHeight): merge pay2ReemissionBox into reemissionBox
    val mergeFee = 1000000L
    val mergedReemissionCandidate = new ErgoBoxCandidate(
      initialReemissionBox.value + pay2ReemissionBox.value - mergeFee,
      reemissionProp,
      creationHeight = 1,
      additionalTokens = nftAsset
    )
    val feeCandidate = new ErgoBoxCandidate(mergeFee, cs.monetary.feeProposition, creationHeight = 1)
    val mergeTx = ErgoTransaction(
      IndexedSeq(
        new Input(initialReemissionBox.id, ProverResult.empty),
        new Input(pay2ReemissionBox.id, ProverResult.empty)
      ),
      IndexedSeq.empty,
      IndexedSeq(mergedReemissionCandidate, feeCandidate)
    )
    val feeTx = CandidateGenerator.collectFees(0, Seq(mergeTx), defaultMinerPk, mkUpcomingContext(None, us0)).get
    val b1 = mkBlock(None, us0, Seq(mergeTx, feeTx), s)
    val us1 = us0.applyModifier(b1, None)(_ => ()).get
    us1.reemissionBoxOpt.map(_.value) shouldBe Some(9L * CoinsInOneErgo - mergeFee)

    // Block 2 (height 2 == reemissionStartHeight): collects 3 ERG from the merged re-emission box
    val rewardTx2 = CandidateGenerator.collectEmission(us1, defaultMinerPk, mkUpcomingContext(Some(b1), us1)).get
    val b2 = mkBlock(Some(b1), us1, Seq(rewardTx2), s)
    val us2 = us1.applyModifier(b2, None)(_ => ()).get
    us2.reemissionBoxOpt.map(_.value) shouldBe Some(6L * CoinsInOneErgo - mergeFee)

    // Rollback to b1 restores the pre-withdrawal merged re-emission box, and rollback to v0 restores initialReemissionBox
    val rolledBack1 = us2.rollbackTo(idToVersion(b1.id)).get
    rolledBack1.reemissionBoxOpt.map(_.value) shouldBe Some(9L * CoinsInOneErgo - mergeFee)
    val rolledBack0 = rolledBack1.rollbackTo(v0).get
    rolledBack0.reemissionBoxOpt.map(_.id.toSeq) shouldBe Some(initialReemissionBox.id.toSeq)
  }

  property("falls back to re-emission box at reemissionStartHeight if original emission box missed historical blocks") {
    val s = mkReemissionSettings(reemissionStartHeight = 9)
    val cs = s.chainSettings
    val rr = cs.reemission.reemissionRules
    val reemissionProp = rr.reemissionBoxProp(cs.monetary)

    // Simulate height 8 (so nextHeight = 9 == reemissionStartHeight, where minersRewardAtHeight(9) == 0)
    // while the original emission box still holds > oneEpochReduction (6 ERG) due to a missed block.
    val uncollectedEmissionBox = new ErgoBox(
      value = 6L * CoinsInOneErgo,
      ergoTree = cs.monetary.emissionBoxProposition,
      additionalTokens = Colls.fromItems(
        (Digest32Coll @@ cs.reemission.emissionNftIdBytes) -> 1L,
        (Digest32Coll @@ cs.reemission.reemissionTokenIdBytes) -> (17L * CoinsInOneErgo)
      ),
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(0x71.toByte)),
      index = 0: Short,
      creationHeight = 1
    )
    val reemissionBox = new ErgoBox(
      value = 10L * CoinsInOneErgo,
      ergoTree = reemissionProp,
      additionalTokens = Colls.fromItems((Digest32Coll @@ cs.reemission.reemissionNftIdBytes) -> 1L),
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Array.fill(32)(0x72.toByte)),
      index = 0: Short,
      creationHeight = 1
    )

    val us = UtxoState.fromBoxHolder(
      BoxHolder(Seq(uncollectedEmissionBox, reemissionBox)),
      Some(uncollectedEmissionBox),
      createTempDir,
      s,
      v2Parameters
    )
    val txs = CandidateGenerator.collectRewards(
      Some(uncollectedEmissionBox),
      currentHeight = 8,
      txs = Seq.empty,
      minerPk = defaultMinerPk,
      stateContext = mkUpcomingContext(None, us),
      reemissionBoxOpt = Some(reemissionBox)
    )
    txs.length shouldBe 1
    txs.head.inputs.map(_.boxId.toSeq) shouldBe IndexedSeq(reemissionBox.id.toSeq)
    txs.head.outputs(0).value shouldBe 7L * CoinsInOneErgo
    txs.head.outputs(1).value shouldBe rr.reemissionRewardPerBlock
  }

  property("handles zero-reemission final epoch blocks before lastCoins depletion without zero-amount tokens") {
    // At height 7 (`nextHeight = 8`), `emissionAtHeight(8) == 3 ERG` so `reemissionForHeight(8) == 0L`.
    // Simulate `emissionBox.value == 6 ERG > oneEpochReduction` (as in Mainnet's 65,536-block final 3 ERG/block epoch):
    val baseS = mkReemissionSettings(reemissionStartHeight = 9)
    // Use epochLength = 2 so h = 14 (`emissionAtHeight = 3 ERG`, `reemissionForHeight = 0L`) has 6 ERG left in emissionBox
    val multiBlockMonetary = customMonetary.copy(epochLength = 2)
    val s = baseS.copy(
      chainSettings = baseS.chainSettings.copy(
        monetary = multiBlockMonetary,
        reemission = baseS.chainSettings.reemission.copy(reemissionStartHeight = 16)
      )
    )
    val cs = s.chainSettings
    cs.emissionRules.blocksTotal shouldBe 15

    val genesisEmission = new ErgoBox(
      value = cs.emissionRules.minersCoinsTotal,
      ergoTree = cs.monetary.emissionBoxProposition,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = ErgoBox.allZerosModifierId,
      index = 0: Short,
      creationHeight = 0
    )
    var state = UtxoState.fromBoxHolder(BoxHolder(Seq(genesisEmission, injectionBox)), Some(genesisEmission), createTempDir, s, v2Parameters)
    var parentOpt: Option[ErgoFullBlock] = None

    // Blocks 1..13 bring emissionBox down to 6 ERG (two 3-ERG blocks remaining in the final epoch)
    (1 to 13).foreach { _ =>
      val tx = CandidateGenerator.collectEmission(state, defaultMinerPk, mkUpcomingContext(parentOpt, state)).get
      val block = mkBlock(parentOpt, state, Seq(tx), s)
      state = state.applyModifier(block, None)(_ => ()).get
      parentOpt = Some(block)
    }
    state.emissionBoxOpt.map(_.value) shouldBe Some(6L * CoinsInOneErgo)

    // Block 14: emissionBox has 6 ERG (> 3 ERG), emission is 3 ERG, reemissionForHeight == 0L -> no 0-amount token in miner output
    val tx14 = CandidateGenerator.collectEmission(state, defaultMinerPk, mkUpcomingContext(parentOpt, state)).get
    tx14.outputs(1).additionalTokens.isEmpty shouldBe true
    val b14 = mkBlock(parentOpt, state, Seq(tx14), s)
    state = state.applyModifier(b14, None)(_ => ()).get
    parentOpt = Some(b14)
    state.emissionBoxOpt.map(_.value) shouldBe Some(3L * CoinsInOneErgo)

    // Block 15: final 3 ERG depleted via `lastCoins`; emissionBoxOpt -> None
    val tx15 = CandidateGenerator.collectEmission(state, defaultMinerPk, mkUpcomingContext(parentOpt, state)).get
    val b15 = mkBlock(parentOpt, state, Seq(tx15), s)
    state = state.applyModifier(b15, None)(_ => ()).get
    state.emissionBoxOpt shouldBe None
  }
}
