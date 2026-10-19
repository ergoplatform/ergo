package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import java.io.File
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.{BoxUtils, ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, TestCase}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedModifier}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.{CloseWallet, GetTransaction, GetWalletStatus, InitWallet, ReadPublicKeys, ScanInThePast, ScanOnChain, WalletStatus}
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, ErgoWallet}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ChainProgress
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome.Accepted
import org.ergoplatform.wallet.utils.FileUtils
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.util.{ModifierId, bytesToId}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.wallet.Constants.PaymentsScanId

import scala.concurrent.duration._

class ErgoNodeViewHolderSpec extends ErgoCorePropertyTest with NodeViewTestOps with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val t0 = TestCase("check chain is healthy") { fixture =>
    val (us, bh) = createUtxoState(settings)
    val block = validFullBlock(None, us, bh)

    val history = generateHistory(true, StateType.Utxo, false, 2)

    // too big chain update delay
    val notAcceptableDelay = System.currentTimeMillis() - (initSettings.nodeSettings.acceptableChainUpdateDelay.toMillis + 100)
    val invalidProgress = ChainProgress(block, 2, 3, notAcceptableDelay)
    ErgoNodeViewHolder.checkChainIsHealthy(invalidProgress, history, initSettings).isInstanceOf[ChainIsStuck] shouldBe true

    // acceptable chain update delay
    val acceptableDelay = System.currentTimeMillis() - 5
    val validProgress = ChainProgress(block, 2, 3, acceptableDelay)
    ErgoNodeViewHolder.checkChainIsHealthy(validProgress, history, initSettings) shouldBe ChainIsHealthy
  }


  private val t1 = TestCase("check genesis state") { fixture =>
    import fixture._
    getCurrentState.rootDigest shouldBe getGenesisStateDigest
  }

  private val t2 = TestCase("check history after genesis") { fixture =>
    import fixture._
    getBestHeaderOpt shouldBe None
  }

  private val t3 = TestCase("apply valid block header") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val block = validFullBlock(None, us, bh)

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    getHistoryHeight shouldBe GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(GenesisHeight)
    getLastHeadersLength(10) shouldBe 1
    getBestHeaderOpt shouldBe Some(block.header)
  }

  private val t3a = TestCase("do not apply block headers in invalid order") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val parentBlock = validFullBlock(None, us, bh)
    val block = validFullBlock(Some(parentBlock), us, bh)

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[SyntacticallySuccessfulModifier])

    //sending child header without parent header
    nodeViewHolderRef ! ModifiersFromRemote(List(block.header))
    expectNoMsg()

    // sende correct header sequence
    nodeViewHolderRef ! ModifiersFromRemote(List(parentBlock.header))
    expectMsgType[SyntacticallySuccessfulModifier]

    nodeViewHolderRef ! ModifiersFromRemote(List(block.header))
    expectMsgType[SyntacticallySuccessfulModifier]

    getHistoryHeight shouldBe 2
  }

  private val t4 = TestCase("apply valid block as genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    nodeViewHolderRef ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    if (verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      expectMsgType[SyntacticallySuccessfulModifier]
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.adProofs.value)
      expectMsgType[SyntacticallySuccessfulModifier]
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.extension)
      expectMsgType[SyntacticallySuccessfulModifier]
      getBestFullBlockOpt shouldBe Some(genesis)
    }
  }

  private val t5 = TestCase("apply full blocks after genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      }.get
    applyBlock(genesis) shouldBe 'success

    val block = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(block) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockOpt shouldBe Some(block)
    }

    getBestHeaderOpt shouldBe Some(block.header)
    getHistoryHeight shouldBe block.header.height
    getLastHeadersLength(10) shouldBe 2
  }

  private val t6 = TestCase("add transaction to memory pool") { fixture =>
    import fixture._
    if (stateType == Utxo) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      val boxes = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree)
      boxes.nonEmpty shouldBe true

      val tx = UnconfirmedTransaction(validTransactionFromBoxes(boxes.toIndexedSeq), None)
      subscribeEvents(classOf[FailedTransaction])
      nodeViewHolderRef ! LocallyGeneratedTransaction(tx)
      expectMsgType[Accepted]
      getPoolSize shouldBe 1
    }
  }

  private val t7 = TestCase("apply statefully invalid full block") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      }.get
    // TODO looks like another bug is still present here, see https://github.com/ergoplatform/ergo/issues/309
    if (verifyTransactions) {
      applyBlock(genesis) shouldBe 'success

      val block = validFullBlock(Some(genesis), wusAfterGenesis)
      val wusAfterBlock = wusAfterGenesis.applyModifier(block)(mod => nodeViewHolderRef ! mod).get

      applyBlock(block) shouldBe 'success
      getBestHeaderOpt shouldBe Some(block.header)
      if (verifyTransactions) {
        getRootHash shouldBe Algos.encode(wusAfterBlock.rootDigest)
      }
      getBestHeaderOpt shouldBe Some(block.header)

      val brokenBlock = generateInvalidFullBlock(Some(block), wusAfterBlock)
      applyBlock(brokenBlock) shouldBe 'success

      val brokenBlock2 = generateInvalidFullBlock(Some(block), wusAfterBlock)
      brokenBlock2.header should not be brokenBlock.header
      applyBlock(brokenBlock2) shouldBe 'success

      getBestFullBlockOpt shouldBe Some(block)
      getRootHash shouldBe Algos.encode(wusAfterBlock.rootDigest)
      getBestHeaderOpt shouldBe Some(block.header)
    }
  }

  /**
    * Generates statefuly invalid full block (contains invalid transactions).
    */
  private def generateInvalidFullBlock(parentBlockOpt: Option[ErgoFullBlock], parentState: WrappedUtxoState) = {
    val validInterlinks = nipopowAlgos.updateInterlinks(parentBlockOpt.map(_.header), parentBlockOpt.map(_.extension))
    val extensionIn = nipopowAlgos.interlinksToExtension(validInterlinks).toExtension(modifierIdGen.sample.get)
    val brokenBlockIn = validFullBlock(parentBlockOpt, parentState)
    val headTx = brokenBlockIn.blockTransactions.txs.head
    val wrongBoxId: ADKey = ADKey !@@ Algos.hash("wrong input")
    val newInput = headTx.inputs.head.copy(boxId = wrongBoxId)
    val brokenTransactionsIn = brokenBlockIn.blockTransactions
      .copy(txs = headTx.copy(inputs = newInput +: headTx.inputs.tail) +: brokenBlockIn.blockTransactions.txs.tail)
    val brokenHeader = brokenBlockIn.header
      .copy(transactionsRoot = brokenTransactionsIn.digest, extensionRoot = extensionIn.digest)
    val brokenTransactions = brokenTransactionsIn.copy(headerId = brokenHeader.id)
    val brokenProofs = brokenBlockIn.adProofs.value.copy(headerId = brokenHeader.id)
    val extension = extensionIn.copy(headerId = brokenHeader.id)
    ErgoFullBlock(brokenHeader, brokenTransactions, extension, Some(brokenProofs))
  }

  private val t8 = TestCase("switching for a better chain") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      }.get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootDigest)

    val chain1block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    val expectedBestFullBlockOpt = if (verifyTransactions) Some(chain1block1) else None
    applyBlock(chain1block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val chain2block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(chain2block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1)(mod => nodeViewHolderRef ! mod).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)
    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootDigest

    applyBlock(chain2block2) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockEncodedId shouldBe Some(chain2block2.header.encodedId)
    }

    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getRootHash shouldBe Algos.encode(chain2block2.header.stateRoot)
  }

  private val t9 = TestCase("UTXO state should generate adProofs and put them in history") { fixture =>
    import fixture._
    if (stateType == StateType.Utxo) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)

      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.header)
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedModifier(genesis.extension)

      getBestFullBlockOpt shouldBe Some(genesis)
      getModifierById(genesis.adProofs.value.id) shouldBe genesis.adProofs
    }
  }

  private val t10 = TestCase("NodeViewHolder start from inconsistent state") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      }.get
    applyBlock(genesis) shouldBe 'success

    val block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    applyBlock(block1) shouldBe 'success
    getBestFullBlockOpt shouldBe Some(block1)
    getRootHash shouldBe Algos.encode(block1.header.stateRoot)

    stopNodeViewHolder()
    val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
    this.deleteRecursive(stateDir)
    startNodeViewHolder()

    getRootHash shouldBe Algos.encode(block1.header.stateRoot)
  }

  private val t11 = TestCase("apply payload in incorrect order (excluding extension)") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      }.get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootDigest)

    val chain2block1 = validFullBlock(Some(genesis), wusAfterGenesis)
    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1)(mod => nodeViewHolderRef ! mod).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    nodeViewHolderRef ! LocallyGeneratedModifier(chain2block1.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    applyBlock(chain2block2, excludeExt = true) shouldBe 'success
    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getBestFullBlockEncodedId shouldBe Some(genesis.header.encodedId)

    applyPayload(chain2block1, excludeExt = true) shouldBe 'success
    getBestHeaderEncodedId shouldBe Some(chain2block2.header.encodedId)
  }

  private val t12 = TestCase("Do not apply txs with wrong header id") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val block = validFullBlock(None, us, bh)
    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    subscribeEvents(classOf[SyntacticallyFailedModification])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    expectMsgType[SyntacticallySuccessfulModifier]
    val currentHeight = getHistoryHeight
    currentHeight shouldBe GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(GenesisHeight)

    val randomId = modifierIdGen.sample.value
    val recoverableTxs = block.blockTransactions.copy(headerId = randomId)
    val invalidTxsWithWrongOutputs = {
      val txs = block.blockTransactions.transactions
      val tx = txs.head
      val wrongOutputs = tx.outputCandidates.map(o =>
        new ErgoBoxCandidate(o.value + 10L, o.ergoTree, currentHeight, o.additionalTokens, o.additionalRegisters)
      )
      val wrongTxs = tx.copy(outputCandidates = wrongOutputs) +: txs.tail
      block.blockTransactions.copy(txs = wrongTxs)
    }
    val invalidTxsWithWrongInputs = {
      val txs = block.blockTransactions.transactions
      val tx = txs.head
      val wrongInputs = tx.inputs.map { input =>
        input.copy(boxId = ADKey @@ input.boxId.reverse)
      }
      val wrongTxs = tx.copy(inputs = wrongInputs) +: txs.tail
      block.blockTransactions.copy(txs = wrongTxs)
    }

    nodeViewHolderRef ! LocallyGeneratedModifier(recoverableTxs)
    expectMsgType[RecoverableFailedModification]

    nodeViewHolderRef ! LocallyGeneratedModifier(invalidTxsWithWrongOutputs)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedModifier(invalidTxsWithWrongInputs)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedModifier(block.blockTransactions)
    expectMsgType[SyntacticallySuccessfulModifier]
  }

  private val t13 = TestCase("Do not apply wrong adProofs") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val block = validFullBlock(None, us, bh)
    getBestHeaderOpt shouldBe None

    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    subscribeEvents(classOf[SyntacticallyFailedModification])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    val randomId = modifierIdGen.sample.value
    val wrongProofsBytes = SerializedAdProof @@ block.adProofs.value.proofBytes.reverse
    val wrongProofs1 = block.adProofs.map(_.copy(headerId = randomId))
    val wrongProofs2 = block.adProofs.map(_.copy(proofBytes = wrongProofsBytes))

    nodeViewHolderRef ! LocallyGeneratedModifier(wrongProofs1.value)
    expectMsgType[RecoverableFailedModification]

    nodeViewHolderRef ! LocallyGeneratedModifier(wrongProofs2.value)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedModifier(block.adProofs.value)
    expectMsgType[SyntacticallySuccessfulModifier]
  }

  private val t14 = TestCase("do not apply genesis block header if " +
    "it's not equal to genesisId from config") { fixture =>
    import fixture._
    updateConfig(genesisIdConfig(modifierIdGen.sample))
    val (us, bh) = createUtxoState(fixture.settings)
    val block = validFullBlock(None, us, bh)

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    subscribeEvents(classOf[SyntacticallyFailedModification])

    //sending header
    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    expectMsgType[SyntacticallyFailedModification]
    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight
  }

  private val t15 = TestCase("apply genesis block header if it's equal to genesisId from config") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val block = validFullBlock(None, us, bh)
    updateConfig(genesisIdConfig(Some(block.header.id)))

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    subscribeEvents(classOf[SyntacticallyFailedModification])

    nodeViewHolderRef ! LocallyGeneratedModifier(block.header)
    expectMsgType[SyntacticallySuccessfulModifier]
    getHistoryHeight shouldBe GenesisHeight
    getHeightOf(block.header.id) shouldBe Some(GenesisHeight)
  }

  private val t16 = TestCase("apply forks that include genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val wusGenesis = WrappedUtxoState(us, bh, fixture.settings)


    val chain1block1 = validFullBlock(parentOpt = None, us, bh)
    val expectedBestFullBlockOpt = if (verifyTransactions) Some(chain1block1) else None
    applyBlock(chain1block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val chain2block1 = validFullBlock(parentOpt = None, us, bh)
    applyBlock(chain2block1) shouldBe 'success
    getBestFullBlockOpt shouldBe expectedBestFullBlockOpt
    getBestHeaderOpt shouldBe Some(chain1block1.header)

    val wusChain2Block1 = wusGenesis.applyModifier(chain2block1)(mod => nodeViewHolderRef ! mod).get
    val chain2block2 = validFullBlock(Some(chain2block1), wusChain2Block1)
    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootDigest

    applyBlock(chain2block2) shouldBe 'success
    if (verifyTransactions) {
      getBestFullBlockEncodedId shouldBe Some(chain2block2.header.encodedId)
    }

    getBestHeaderOpt shouldBe Some(chain2block2.header)
    getRootHash shouldBe Algos.encode(chain2block2.header.stateRoot)
  }

  private val t17 = TestCase("apply invalid genesis header") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(fixture.settings)
    val header = validFullBlock(None, us, bh).header.copy(parentId = bytesToId(Array.fill(32)(9: Byte)))

    getBestHeaderOpt shouldBe None
    getHistoryHeight shouldBe EmptyHistoryHeight

    subscribeEvents(classOf[RecoverableFailedModification])
    subscribeEvents(classOf[SyntacticallySuccessfulModifier])
    subscribeEvents(classOf[SyntacticallyFailedModification])

    nodeViewHolderRef ! LocallyGeneratedModifier(header)
    expectMsgType[SyntacticallyFailedModification]
    getHistoryHeight shouldBe EmptyHistoryHeight
    getHeightOf(header.id) shouldBe None
  }

  private val t18 = TestCase("apply syntactically invalid genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)

    val validBlock = validFullBlock(parentOpt = None, us, bh)
    val invalidBlock = validBlock.copy(header = validBlock.header.copy(parentId = bytesToId(Array.fill(32)(9: Byte))))

    applyBlock(invalidBlock) shouldBe 'failure
    getBestFullBlockOpt shouldBe None
    getBestHeaderOpt shouldBe None
  }

  private val t19 = TestCase("apply semantically invalid genesis block") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val wusGenesis = WrappedUtxoState(us, bh, fixture.settings)

    val invalidBlock = generateInvalidFullBlock(None, wusGenesis)

    if (verifyTransactions) {

      val initDigest = getCurrentState.rootDigest

      applyBlock(invalidBlock) shouldBe 'success

      getBestFullBlockOpt shouldBe None
      getBestHeaderOpt shouldBe None
      getCurrentState.rootDigest shouldEqual initDigest
    }
  }

  property("height-zero wallet scans retained blocks before the first direct delivery") {
    def observe(blocksToKeep: Int, probePendingBody: Boolean = false): (Boolean, Boolean) = {
      val ordinarySettings = NodeViewTestConfig(
        StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
      val testSettings = ordinarySettings.copy(
        nodeSettings = ordinarySettings.nodeSettings.copy(blocksToKeep = blocksToKeep))

      new NodeViewFixture(testSettings, parameters).apply { fixture =>
        import fixture._
        val observer = TestProbe()(actorSystem)
        val history = getCurrentView.history
        val walletActor = getCurrentView.vault.walletActor
        observer.awaitAssert({
          observer.send(walletActor, GetWalletStatus)
          observer.expectMsgType[WalletStatus].initialized shouldBe true
        }, 10.seconds, 100.millis)
        observer.send(walletActor, ReadPublicKeys(0, 1))
        val paymentScript = observer.expectMsgType[Seq[org.ergoplatform.P2PKAddress]].head.script

        val (producerState, producerBoxes) = createUtxoState(fixture.settings)
        val genesisState = WrappedUtxoState(producerState, producerBoxes, fixture.settings)
        val first = validFullBlock(None, genesisState)
        val afterFirst = genesisState.applyModifier(first)(_ => ()).get
        val minTxInputValue = 2 * BoxUtils.sufficientAmount(extendedParameters)
        val firstSpendable = afterFirst.takeBoxes(afterFirst.size)
          .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
        val creditTx = validTransactionFromBoxes(
          IndexedSeq(firstSpendable), issueNew = false, outputsProposition = paymentScript)
        val second = validFullBlock(Some(first), afterFirst, Seq(creditTx))
        var producer = afterFirst.applyModifier(second)(_ => ()).get
        var previous = second
        val laterBlocks = (3 to 22).map { _ =>
          val spendable = producer.takeBoxes(producer.size)
            .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
          val tx = validTransactionFromBoxes(
            IndexedSeq(spendable), issueNew = false, outputsProposition = TrueTree)
          val block = validFullBlock(Some(previous), producer, Seq(tx))
          producer = producer.applyModifier(block)(_ => ()).get
          previous = block
          block
        }

        (first +: second +: laterBlocks).foreach(block => applyHeader(block.header) shouldBe 'success)
        history.headersHeight shouldBe 22
        history.fullBlockHeight shouldBe 0

        Seq(first, second, laterBlocks.head).zipWithIndex.foreach { case (block, index) =>
          applyPayload(block) shouldBe 'success
          val height = index + 1
          observer.awaitAssert({
            history.fullBlockHeight shouldBe height
            getCurrentState.stateContext.currentHeight shouldBe height
          }, 10.seconds, 100.millis)
          if (height == 1) {
            history.bestFullBlockAt(2) shouldBe None
            if (probePendingBody) {
              observer.send(walletActor, ScanOnChain(first))
              observer.awaitAssert({
                observer.send(walletActor, GetWalletStatus)
                observer.expectMsgType[WalletStatus].height shouldBe 1
              }, 10.seconds, 100.millis)
            }
            // A header for h2 exists, but its body has not arrived. It must not advance the wallet.
            if (probePendingBody) observer.send(walletActor, ScanInThePast(2, rescan = false))
            observer.send(walletActor, GetWalletStatus)
            observer.expectMsgType[WalletStatus].height shouldBe (if (probePendingBody) 1 else 0)
          }
          if (height == 2 && probePendingBody) {
            observer.awaitAssert({
              observer.send(walletActor, GetWalletStatus)
              observer.expectMsgType[WalletStatus].height shouldBe 2
            }, 10.seconds, 100.millis)
          }
          if (height < 3 && !probePendingBody) {
            history.headersHeight - history.fullBlockHeight should be >= 20
            observer.send(walletActor, GetWalletStatus)
            observer.expectMsgType[WalletStatus].height shouldBe 0
          }
        }
        history.headersHeight - history.fullBlockHeight shouldBe 19
        observer.awaitAssert({
          observer.send(walletActor, GetWalletStatus)
          observer.expectMsgType[WalletStatus].height shouldBe 3
        }, 10.seconds, 100.millis)
        observer.send(walletActor, GetTransaction(creditTx.id))
        val credited = observer.expectMsgType[Option[AugWalletTransaction]]
          .exists(_.wtx.scanIds.contains(PaymentsScanId))
        history.getFullBlock(second.header).isDefined -> credited
      }
    }

    observe(1) shouldBe ((false, false))
    observe(3) shouldBe ((true, true))
    observe(-1) shouldBe ((true, true))
    observe(3, probePendingBody = true) shouldBe ((true, true))
  }

  property("height-zero wallet catches up on quiet reopen") {
    val ordinarySettings = NodeViewTestConfig(
      StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val testSettings = ordinarySettings.copy(
      nodeSettings = ordinarySettings.nodeSettings.copy(blocksToKeep = 3))

    new NodeViewFixture(testSettings, parameters).apply { fixture =>
      import fixture._
      val observer = TestProbe()(actorSystem)
      val history = getCurrentView.history
      val walletActor = getCurrentView.vault.walletActor
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        observer.expectMsgType[WalletStatus].initialized shouldBe true
      }, 10.seconds, 100.millis)
      observer.send(walletActor, ReadPublicKeys(0, 1))
      val paymentScript = observer.expectMsgType[Seq[org.ergoplatform.P2PKAddress]].head.script

      val (producerState, producerBoxes) = createUtxoState(fixture.settings)
      val genesisState = WrappedUtxoState(producerState, producerBoxes, fixture.settings)
      val first = validFullBlock(None, genesisState)
      val afterFirst = genesisState.applyModifier(first)(_ => ()).get
      val minTxInputValue = 2 * BoxUtils.sufficientAmount(extendedParameters)
      val firstSpendable = afterFirst.takeBoxes(afterFirst.size)
        .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
      val creditTx = validTransactionFromBoxes(
        IndexedSeq(firstSpendable), issueNew = false, outputsProposition = paymentScript)
      val second = validFullBlock(Some(first), afterFirst, Seq(creditTx))
      var producer = afterFirst.applyModifier(second)(_ => ()).get
      var previous = second
      val laterHeaders = (3 to 22).map { _ =>
        val spendable = producer.takeBoxes(producer.size)
          .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
        val tx = validTransactionFromBoxes(
          IndexedSeq(spendable), issueNew = false, outputsProposition = TrueTree)
        val block = validFullBlock(Some(previous), producer, Seq(tx))
        producer = producer.applyModifier(block)(_ => ()).get
        previous = block
        block.header
      }

      (Seq(first.header, second.header) ++ laterHeaders)
        .foreach(header => applyHeader(header) shouldBe 'success)
      applyPayload(first) shouldBe 'success
      applyPayload(second) shouldBe 'success
      observer.awaitAssert({
        history.fullBlockHeight shouldBe 2
        getCurrentState.stateContext.currentHeight shouldBe 2
      }, 10.seconds, 100.millis)
      observer.send(walletActor, GetWalletStatus)
      observer.expectMsgType[WalletStatus].height shouldBe 0
      history.getFullBlock(second.header).isDefined shouldBe true

      observer.watch(walletActor)
      observer.send(walletActor, CloseWallet)
      observer.expectTerminated(walletActor)
      val reopened = ErgoWallet.readOrGenerate(history, fixture.settings, parameters).walletActor
      observer.awaitAssert({
        observer.send(reopened, GetWalletStatus)
        observer.expectMsgType[WalletStatus].height shouldBe 2
      }, 10.seconds, 100.millis)
      observer.send(reopened, GetTransaction(creditTx.id))
      observer.expectMsgType[Option[AugWalletTransaction]]
        .exists(_.wtx.scanIds.contains(PaymentsScanId)) shouldBe true
      observer.send(reopened, CloseWallet)
    }
  }

  property("height-zero wallet catch-up follows the selected full chain when headers diverge") {
    val ordinarySettings = NodeViewTestConfig(
      StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val testSettings = ordinarySettings.copy(
      nodeSettings = ordinarySettings.nodeSettings.copy(blocksToKeep = 3))

    new NodeViewFixture(testSettings, parameters).apply { fixture =>
      import fixture._
      val observer = TestProbe()(actorSystem)
      val history = getCurrentView.history
      val walletActor = getCurrentView.vault.walletActor
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        observer.expectMsgType[WalletStatus].initialized shouldBe true
      }, 10.seconds, 100.millis)
      observer.send(walletActor, ReadPublicKeys(0, 1))
      val paymentScript = observer.expectMsgType[Seq[org.ergoplatform.P2PKAddress]].head.script

      val (producerState, producerBoxes) = createUtxoState(fixture.settings)
      val genesisState = WrappedUtxoState(producerState, producerBoxes, fixture.settings)
      val first = validFullBlock(None, genesisState)
      applyBlock(first) shouldBe 'success
      val afterFirst = genesisState.applyModifier(first)(_ => ()).get
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        observer.expectMsgType[WalletStatus].height shouldBe 1
      }, 10.seconds, 100.millis)

      val minTxInputValue = 2 * BoxUtils.sufficientAmount(extendedParameters)
      val spendable = afterFirst.takeBoxes(afterFirst.size)
        .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
      val creditTx = validTransactionFromBoxes(
        IndexedSeq(spendable), issueNew = false, outputsProposition = paymentScript)
      val second = validFullBlock(Some(first), afterFirst, Seq(creditTx))
      val afterSecond = afterFirst.applyModifier(second)(_ => ()).get
      val thirdInput = afterSecond.takeBoxes(afterSecond.size)
        .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
      val thirdTx = validTransactionFromBoxes(
        IndexedSeq(thirdInput), issueNew = false, outputsProposition = TrueTree)
      val third = validFullBlock(Some(second), afterSecond, Seq(thirdTx))

      val bForkState = afterSecond.rollbackTo(afterFirst.version).get
      val bSecondTx = validTransactionFromBoxes(
        IndexedSeq(spendable), issueNew = true, outputsProposition = TrueTree)
      val bSecond = validFullBlock(Some(first), bForkState, Seq(bSecondTx))
      bSecond.header.id should not be second.header.id
      var bState = bForkState.applyModifier(bSecond)(_ => ()).get
      var bPrevious = bSecond
      val bLater = (3 to 22).map { _ =>
        val bInput = bState.takeBoxes(bState.size)
          .filter(box => box.ergoTree == TrueTree && box.value >= minTxInputValue).maxBy(_.value)
        val bTx = validTransactionFromBoxes(
          IndexedSeq(bInput), issueNew = false, outputsProposition = TrueTree)
        val block = validFullBlock(Some(bPrevious), bState, Seq(bTx))
        bState = bState.applyModifier(block)(_ => ()).get
        bPrevious = block
        block
      }
      applyHeader(second.header) shouldBe 'success
      applyHeader(third.header) shouldBe 'success
      (bSecond +: bLater).foreach(block => applyHeader(block.header) shouldBe 'success)
      history.headersHeight shouldBe 22
      history.bestHeaderIdAtHeight(2) shouldBe Some(bSecond.header.id)

      applyPayload(second) shouldBe 'success
      observer.awaitAssert({
        history.fullBlockHeight shouldBe 2
        getCurrentState.stateContext.currentHeight shouldBe 2
      }, 10.seconds, 100.millis)
      history.isSelectedFullBlockId(second.header.id) shouldBe true
      history.getFullBlock(second.header).isDefined shouldBe true
      history.bestFullBlockAt(2) shouldBe None
      observer.send(walletActor, GetWalletStatus)
      observer.expectMsgType[WalletStatus].height shouldBe 1

      applyPayload(third) shouldBe 'success
      observer.awaitAssert({
        history.fullBlockHeight shouldBe 3
        getCurrentState.stateContext.currentHeight shouldBe 3
      }, 10.seconds, 100.millis)
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        observer.expectMsgType[WalletStatus].height shouldBe 3
      }, 10.seconds, 100.millis)
      observer.send(walletActor, GetTransaction(creditTx.id))
      observer.expectMsgType[Option[AugWalletTransaction]]
        .exists(_.wtx.scanIds.contains(PaymentsScanId)) shouldBe true
    }
  }

  property("height-zero wallet catches up after late initialization") {
    val ordinarySettings = NodeViewTestConfig(
      StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val testSettings = ordinarySettings.copy(
      nodeSettings = ordinarySettings.nodeSettings.copy(blocksToKeep = 3),
      walletSettings = ordinarySettings.walletSettings.copy(testMnemonic = None, testKeysQty = None))

    new NodeViewFixture(testSettings, parameters).apply { fixture =>
      import fixture._
      val observer = TestProbe()(actorSystem)
      val history = getCurrentView.history
      val walletActor = getCurrentView.vault.walletActor
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        observer.expectMsgType[WalletStatus].initialized shouldBe false
      }, 10.seconds, 100.millis)

      val (producerState, producerBoxes) = createUtxoState(fixture.settings)
      val genesisState = WrappedUtxoState(producerState, producerBoxes, fixture.settings)
      val first = validFullBlock(None, genesisState)
      val afterFirst = genesisState.applyModifier(first)(_ => ()).get
      val second = validFullBlock(Some(first), afterFirst)
      applyBlock(first) shouldBe 'success
      applyBlock(second) shouldBe 'success
      observer.awaitAssert({
        history.fullBlockHeight shouldBe 2
        getCurrentState.stateContext.currentHeight shouldBe 2
      }, 10.seconds, 100.millis)
      history.getFullBlock(first.header).isDefined shouldBe true
      history.getFullBlock(second.header).isDefined shouldBe true

      observer.send(walletActor, ChangedState(getCurrentState.getReader))
      observer.send(walletActor, GetWalletStatus)
      val before = observer.expectMsgType[WalletStatus]
      before.initialized shouldBe false
      before.height shouldBe 0

      observer.send(walletActor, InitWallet(
        org.ergoplatform.sdk.SecretString.create("height-zero-test-passphrase"), None))
      val initialized = observer.expectMsgType[scala.util.Try[org.ergoplatform.sdk.SecretString]]
      initialized.isSuccess shouldBe true
      initialized.foreach(_.erase())
      observer.awaitAssert({
        observer.send(walletActor, GetWalletStatus)
        val status = observer.expectMsgType[WalletStatus]
        status.initialized shouldBe true
        status.height shouldBe 2
      }, 10.seconds, 100.millis)
    }
  }

  val cases: List[TestCase] = List(t0, t1, t2, t3, t3a, t4, t5, t6, t7, t8, t9)

  NodeViewTestConfig.allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(parameters, c)
      }
    }
  }

  val verifyingTxCases: List[TestCase] = List(t10, t11, t12, t13)

  NodeViewTestConfig.verifyTxConfigs.foreach { c =>
    verifyingTxCases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(parameters, c)
      }
    }
  }

  val genesisIdTestCases = List(t14, t15, t16, t17, t18, t19)

  def genesisIdConfig(expectedGenesisIdOpt: Option[ModifierId])(protoSettings: ErgoSettings): ErgoSettings = {
    protoSettings.copy(chainSettings = protoSettings.chainSettings.copy(genesisId = expectedGenesisIdOpt))
  }

  genesisIdTestCases.foreach { t =>
    property(t.name) {
      t.run(parameters, NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = true))
    }
  }

}
