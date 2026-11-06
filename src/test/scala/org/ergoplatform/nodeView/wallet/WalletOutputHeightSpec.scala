package org.ergoplatform.nodeView.wallet

import org.ergoplatform.{ErgoBox, ErgoBoxAssetsHolder, ErgoBoxCandidate,
  ErgoLikeTransaction, Input}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry,
  WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState, validFullBlock}
import org.ergoplatform.utils.{ErgoCorePropertyTest, MempoolTestHelpers, WalletTestOps}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionResult, MinBoxValue}
import org.ergoplatform.wallet.boxes.{ErgoBoxSerializer,
  ReplaceCompactCollectBoxSelector, TrackedBox}
import org.scalatest.BeforeAndAfterAll
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.encode.Base16
import sigma.ast.ByteArrayConstant
import sigmastate.eval.Extensions._
import sigmastate.helpers.TestingHelpers.testBox

class WalletOutputHeightSpec
  extends ErgoCorePropertyTest
    with MempoolTestHelpers
    with WalletTestOps
    with ErgoWalletSupport
    with DBSpec
    with BeforeAndAfterAll {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{
    defaultProver, emptyProverResult, parameters}
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  override val ergoSettings: ErgoSettings = settings
  private implicit val fixture: WalletFixture =
    new WalletFixture(settings, parameters, getCurrentView(_).vault)
  private lazy val pks = getPublicKeys.toList

  override def afterAll(): Unit = try super.afterAll() finally fixture.stop()

  private def initialState(store: LDBKVStore,
                           versionedStore: LDBVersionedStore,
                           mempool: Option[ErgoMemPoolReader] = None): ErgoWalletState = {
    ErgoWalletState(
      new WalletStorage(store, settings),
      secretStorageOpt = Option.empty,
      new WalletRegistry(versionedStore)(settings.walletSettings),
      OffChainRegistry.empty,
      outputsFilter = Option.empty,
      WalletVars(Some(defaultProver), Seq.empty, None),
      stateReaderOpt = Option.empty,
      mempoolReaderOpt = mempool,
      utxoStateReaderOpt = Option.empty,
      parameters,
      maxInputsToUse = 1000,
      rescanInProgress = false
    )
  }

  property("wallet-derived outputs use the maximum selected spending height") {
    val inputHeight = 2
    val walletHeight = inputHeight - 1
    val source = testBox(2000000L, pks.head.script, inputHeight)
    val tracked = TrackedBox(
      ErgoLikeTransaction(IndexedSeq.empty, IndexedSeq.empty),
      creationOutIndex = 0,
      None,
      source,
      Set(PaymentsScanId)
    )
    val paymentValue = 800000L
    val derivedValue = source.value - paymentValue
    val payment = new ErgoBoxCandidate(paymentValue, pks.head.script, inputHeight)
    val change = new BoxSelectionResult(
      Seq(tracked), Seq(ErgoBoxAssetsHolder(derivedValue)), None)
    val reemission = new BoxSelectionResult(
      Seq(tracked), Seq.empty, Some(ErgoBoxAssetsHolder(derivedValue)))

    val normalControl = prepareUnsignedTransaction(
      Seq(payment), inputHeight, inputHeight, change, IndexedSeq.empty,
      Some(pks.head.pubkey)).get
    val reemissionControl = prepareUnsignedTransaction(
      Seq(payment), inputHeight, inputHeight, reemission, IndexedSeq.empty,
      Some(pks.head.pubkey)).get
    val expectedHeights = Seq(inputHeight, inputHeight)
    normalControl.outputCandidates.map(_.creationHeight) shouldBe expectedHeights
    reemissionControl.outputCandidates.map(_.creationHeight) shouldBe expectedHeights

    val normalLagged = prepareUnsignedTransaction(
      Seq(payment), inputHeight, walletHeight, change, IndexedSeq.empty,
      Some(pks.head.pubkey)).get
    val reemissionLagged = prepareUnsignedTransaction(
      Seq(payment), inputHeight, walletHeight, reemission, IndexedSeq.empty,
      Some(pks.head.pubkey)).get
    val lagged = Seq(normalLagged, reemissionLagged)
    lagged.map(_.outputCandidates.map(_.creationHeight)) shouldBe
      Seq(expectedHeights, expectedHeights)
    lagged.map(_.outputCandidates.map(_.value).sum) shouldBe
      Seq(source.value, source.value)
    normalLagged.outputCandidates.last.ergoTree shouldBe pks.head.script
    reemissionLagged.outputCandidates.last.ergoTree shouldBe
      settings.chainSettings.reemission.reemissionRules.payToReemission

    val older = TrackedBox(
      ErgoLikeTransaction(IndexedSeq.empty, IndexedSeq.empty),
      creationOutIndex = 0,
      None,
      testBox(source.value, pks.head.script, 0),
      Set(PaymentsScanId)
    )
    val mixedChange = older.box.value + source.value - paymentValue
    val mixed = new BoxSelectionResult(
      Seq(older, tracked), Seq(ErgoBoxAssetsHolder(mixedChange)), None)
    val olderPayment = new ErgoBoxCandidate(paymentValue, pks.head.script, walletHeight)
    val highDataInput = testBox(source.value, pks.head.script, inputHeight + 10)
    val mixedTx = prepareUnsignedTransaction(
      Seq(olderPayment), walletHeight, walletHeight, mixed,
      IndexedSeq(highDataInput), Some(pks.head.pubkey)).get
    mixedTx.outputCandidates.map(_.creationHeight) shouldBe expectedHeights
    mixedTx.outputCandidates.map(_.value).sum shouldBe older.box.value + source.value

    val walletAhead = prepareUnsignedTransaction(
      Seq(olderPayment), 1, 3, change, IndexedSeq.empty, Some(pks.head.pubkey)).get
    walletAhead.outputCandidates.map(_.creationHeight) shouldBe Seq(3, 3)

    val fullAhead = prepareUnsignedTransaction(
      Seq(olderPayment), 4, 1, change, IndexedSeq.empty, Some(pks.head.pubkey)).get
    fullAhead.outputCandidates.map(_.creationHeight) shouldBe Seq(4, 4)
  }

  property("automatic wallet selection can choose a newer off-chain input") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val (utxo, boxes) = createUtxoState(settings)
        try {
          val genesis = validFullBlock(None, utxo, boxes)
          val applied = utxo.applyModifier(genesis, None)(_ => ()).get
          val source = genesis.transactions.flatMap(_.outputs).find { b =>
            b.ergoTree == TrueTree && b.value > 4 * MinBoxValue &&
              applied.boxById(b.id).isDefined
          }.get
          val base = initialState(store, versionedStore)
          base.storage.updateStateContext(applied.stateContext).get
          val address = base.walletVars.publicKeyAddresses.head
          val fullHeight = base.fullHeight
          val walletHeight = base.getWalletHeight
          fullHeight should be > walletHeight

          val parent = ErgoTransaction(
            IndexedSeq(Input(source.id, emptyProverResult)), IndexedSeq.empty,
            IndexedSeq(new ErgoBoxCandidate(source.value, address.script, fullHeight,
              source.additionalTokens)))
          val parentOutput = parent.outputs.head
          val tracked = WalletScanLogic.extractWalletOutputs(
            parent, None, base.walletVars, settings.walletSettings.dustLimit)
          tracked.map(_.box.id.toSeq) shouldBe Seq(parentOutput.id.toSeq)
          val pool = ErgoMemPool.empty(settings).put(UnconfirmedTransaction(parent, None))
          val offChain = base.offChainRegistry.updateOnTransaction(
            tracked, Seq.empty, base.walletVars.externalScans)
          val service = new ErgoWalletServiceImpl(settings)
          val state = service.updateUtxoState(base.copy(
            offChainRegistry = offChain,
            stateReaderOpt = Some(applied),
            mempoolReaderOpt = Some(pool)))
          state.walletFilter(tracked.head) shouldBe true

          val selector = new ReplaceCompactCollectBoxSelector(
            settings.walletSettings.maxInputs,
            settings.walletSettings.optimalInputs,
            None)
          val payment = PaymentRequest(address, source.value / 2, Array.empty, Map.empty)
          val (unsigned, selected, _) = generateUnsignedTransaction(
            state, selector, Seq(payment), Seq.empty, Seq.empty).get
          selected.exists(_.id.sameElements(parentOutput.id)) shouldBe true
          unsigned.outputCandidates.size should be > 1
          unsigned.outputCandidates.map(_.creationHeight).distinct shouldBe
            Seq(fullHeight)
          unsigned.outputCandidates.last.ergoTree shouldBe address.script
          unsigned.outputCandidates.map(_.value).sum shouldBe parentOutput.value
        } finally utxo.closeStorage()
      }
    }
  }

  property("explicit inputs raise all outputs to the next block height") {
    withVersionedStore(2) { versionedStore =>
      withStore { store =>
        val state = initialState(store, versionedStore)
        val address = pks.head
        val upcomingHeight = state.stateContext.simplifiedUpcoming().currentHeight
        val source = testBox(4000000L, address.script, upcomingHeight)
        val payment = PaymentRequest(address, 1000000L, Array.empty, Map.empty)
        val issue = AssetIssueRequest(address = address, valueOpt = Some(1000000L),
          amount = 1L, name = "height", description = "height", decimals = 0,
          registers = Option.empty)
        val selector = new ReplaceCompactCollectBoxSelector(
          settings.walletSettings.maxInputs, settings.walletSettings.optimalInputs, None)

        val (tx, selected, _) = generateUnsignedTransaction(state, selector,
          Seq(payment, issue),
          Seq(Base16.encode(ErgoBoxSerializer.toBytes(source))), Seq.empty).get
        selected.map(_.id.toSeq) shouldBe Seq(source.id.toSeq)
        tx.outputCandidates.map(_.creationHeight) shouldBe Seq.fill(3)(upcomingHeight)
        tx.outputCandidates.map(_.value) shouldBe Seq(1000000L, 1000000L, 2000000L)
        tx.outputCandidates.map(_.ergoTree) shouldBe Seq.fill(3)(address.script)
        tx.outputCandidates.head.additionalTokens.toArray shouldBe empty
        tx.outputCandidates.head.additionalRegisters shouldBe empty
        val minted = tx.outputCandidates(1).additionalTokens.toArray
        minted should contain only (source.id.toTokenId -> 1L)
        tx.outputCandidates(1).additionalRegisters shouldBe Map(
          ErgoBox.R4 -> ByteArrayConstant("height".getBytes("UTF-8")),
          ErgoBox.R5 -> ByteArrayConstant("height".getBytes("UTF-8")),
          ErgoBox.R6 -> ByteArrayConstant("0".getBytes("UTF-8"))
        )
        tx.outputCandidates.last.additionalTokens.toArray shouldBe empty
        tx.outputCandidates.last.additionalRegisters shouldBe empty

        val tooNew = testBox(source.value, address.script, upcomingHeight + 1)
        val rejected = generateUnsignedTransaction(state, selector, Seq(payment, issue),
          Seq(Base16.encode(ErgoBoxSerializer.toBytes(tooNew))), Seq.empty)
        rejected.failed.get.getMessage should
          include("Output creation height exceeds next block height")

        val dusty = testBox(1000001L, address.script, upcomingHeight)
        val dustFailure = generateUnsignedTransaction(state, selector, Seq(payment),
          Seq(Base16.encode(ErgoBoxSerializer.toBytes(dusty))), Seq.empty)
        dustFailure.failed.get.getMessage should include("Minimal ERG value not met")
      }
    }
  }
}
