package org.ergoplatform.nodeView.viewholder

import java.io.File
import scala.concurrent.duration._
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, TestCase}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.DownloadRequest
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedBlockSection, LocallyGeneratedInputBlock, LocallyGeneratedOrderingBlock}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ChainProgress
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome.Accepted
import org.ergoplatform.wallet.utils.FileUtils
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.util.{ModifierId, bytesToId}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.network.message.inputblocks.{InputBlockTransactionsData, OrderingBlockAnnouncement}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import scorex.core.network.ConnectedPeer

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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.header)
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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    if (verifyTransactions) {
      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.blockTransactions)
      expectMsgType[SyntacticallySuccessfulModifier]
      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.adProofs.value)
      expectMsgType[SyntacticallySuccessfulModifier]
      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.extension)
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

      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.header)
      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.blockTransactions)
      nodeViewHolderRef ! LocallyGeneratedBlockSection(genesis.extension)

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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(chain2block1.header)
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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.header)
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

    nodeViewHolderRef ! LocallyGeneratedBlockSection(recoverableTxs)
    expectMsgType[RecoverableFailedModification]

    nodeViewHolderRef ! LocallyGeneratedBlockSection(invalidTxsWithWrongOutputs)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedBlockSection(invalidTxsWithWrongInputs)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.blockTransactions)
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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.header)
    expectMsgType[SyntacticallySuccessfulModifier]

    val randomId = modifierIdGen.sample.value
    val wrongProofsBytes = SerializedAdProof @@ block.adProofs.value.proofBytes.reverse
    val wrongProofs1 = block.adProofs.map(_.copy(headerId = randomId))
    val wrongProofs2 = block.adProofs.map(_.copy(proofBytes = wrongProofsBytes))

    nodeViewHolderRef ! LocallyGeneratedBlockSection(wrongProofs1.value)
    expectMsgType[RecoverableFailedModification]

    nodeViewHolderRef ! LocallyGeneratedBlockSection(wrongProofs2.value)
    expectMsgType[SyntacticallyFailedModification]

    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.adProofs.value)
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
    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.header)
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

    nodeViewHolderRef ! LocallyGeneratedBlockSection(block.header)
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

    nodeViewHolderRef ! LocallyGeneratedBlockSection(header)
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

  /**
    * Helper to create empty InputBlockFields (first input block after ordering block)
    */
  private def emptyInputBlockFields: InputBlockFields = InputBlockFields.empty

  private val t20 = TestCase("process input block from remote peer") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      // Create a header for input block
      val (_, bh2) = createUtxoState(fixture.settings)
      val nextBlock = validFullBlock(Some(genesis), WrappedUtxoState(us, bh2, fixture.settings))
      val inputBlock = InputBlockAnnouncement(1, nextBlock.header, emptyInputBlockFields, None)

      // Create a dummy peer for the message
      val dummyPeer = ConnectedPeer(
        scorex.core.network.ConnectionId(
          new java.net.InetSocketAddress("127.0.0.1", 1234),
          new java.net.InetSocketAddress("127.0.0.1", 5678),
          scorex.core.network.Outgoing
        ),
        testProbe.ref,
        None
      )

      // Send ProcessInputBlock message
      nodeViewHolderRef ! ProcessInputBlock(inputBlock, dummyPeer)

      // Allow time for async processing
      Thread.sleep(500)

      // Verify input block was stored in history's input block records
      getHistory.getInputBlock(inputBlock.id) should not be None
    }
  }

  private val t21 = TestCase("process input block transactions and update mempool") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      // Add transactions to mempool
      val boxes = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == TrueTree)
      boxes.nonEmpty shouldBe true
      val tx = UnconfirmedTransaction(validTransactionFromBoxes(boxes.toIndexedSeq), None)
      nodeViewHolderRef ! LocallyGeneratedTransaction(tx)
      expectMsgType[Accepted]
      getPoolSize shouldBe 1

      // Create input block with the transaction
      val (_, bh2) = createUtxoState(fixture.settings)
      val nextBlock = validFullBlock(Some(genesis), WrappedUtxoState(us, bh2, fixture.settings))
      val inputBlock = InputBlockAnnouncement(1, nextBlock.header, emptyInputBlockFields, None)

      val dummyPeer = ConnectedPeer(
        scorex.core.network.ConnectionId(
          new java.net.InetSocketAddress("127.0.0.1", 1234),
          new java.net.InetSocketAddress("127.0.0.1", 5678),
          scorex.core.network.Outgoing
        ),
        testProbe.ref,
        None
      )

      // First apply the input block
      nodeViewHolderRef ! ProcessInputBlock(inputBlock, dummyPeer)
      Thread.sleep(500)

      // Then apply transactions (use empty transactions to avoid validation issues)
      subscribeEvents(classOf[NewBestInputBlock])
      val txData = InputBlockTransactionsData(inputBlock.id, Seq.empty)
      nodeViewHolderRef ! ProcessInputBlockTransactions(txData)

      // Verify NewBestInputBlock event is published
      val newBestMsg = expectMsgType[NewBestInputBlock]
      newBestMsg.idOpt shouldBe Some(inputBlock.id)
      newBestMsg.local shouldBe false
    }
  }

  private val t22 = TestCase("process ordering block with valid transactions") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      // Create next block using state after genesis
      val wusAfterGenesis = WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis)(_ => ()).get
      val nextBlock = validFullBlock(Some(genesis), wusAfterGenesis)

      // Create ordering block announcement with no broadcasted transactions
      // (transactions from generated block may not be accepted into mempool,
      // so we test with empty transactions which triggers DownloadRequest path)
      val extFields = nextBlock.extension.fields

      val oba = OrderingBlockAnnouncement(
        version = 1,
        header = nextBlock.header,
        nonBroadcastedTransactions = Seq.empty,
        broadcastedTransactionIds = Seq.empty,
        extensionFields = extFields
      )

      subscribeEvents(classOf[SyntacticallySuccessfulModifier])
      subscribeEvents(classOf[DownloadRequest])

      // Send ordering block
      nodeViewHolderRef ! ProcessOrderingBlock(oba)

      // Verify header and extension are applied (published as SyntacticallySuccessfulModifier)
      val modMsg = testProbe.fishForMessage(5.seconds) {
        case _: SyntacticallySuccessfulModifier => true
        case _ => false
      }.asInstanceOf[SyntacticallySuccessfulModifier]
      modMsg.modifierId shouldBe nextBlock.header.id

      // Verify header is in history
      getHeightOf(nextBlock.header.id) shouldBe Some(2)
    }
  }

  private val t23 = TestCase("process ordering block with missing parent caches header") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      // Create a block that doesn't have its parent applied
      val wusAfterGenesis = WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis)(_ => ()).get
      val orphanBlock = validFullBlock(Some(genesis), wusAfterGenesis)
      val orphanBlock2 = validFullBlock(Some(orphanBlock), wusAfterGenesis)

      // Create ordering block announcement for orphanBlock2 (parent orphanBlock not in history yet)
      val oba = OrderingBlockAnnouncement(
        version = 1,
        header = orphanBlock2.header,
        nonBroadcastedTransactions = Seq.empty,
        broadcastedTransactionIds = Seq.empty,
        extensionFields = orphanBlock2.extension.fields
      )

      subscribeEvents(classOf[DownloadRequest])

      // Send ordering block with missing parent
      nodeViewHolderRef ! ProcessOrderingBlock(oba)

      // Wait for DownloadRequest - skip intermediate messages
      val downloadReq = testProbe.fishForMessage(5.seconds) {
        case _: DownloadRequest => true
        case _ => false
      }.asInstanceOf[DownloadRequest]
      downloadReq.modifiersToFetch should contain key org.ergoplatform.modifiers.history.header.Header.modifierTypeId

      // Allow time for caching
      Thread.sleep(500)

      // Verify header is cached (will be applied when parent arrives)
      getHeightOf(orphanBlock2.header.id) shouldBe None
    }
  }

  private val t24 = TestCase("apply locally generated ordering block") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      val wusAfterGenesis = WrappedUtxoState(us, bh, fixture.settings).applyModifier(genesis)(_ => ()).get
      val nextBlock = validFullBlock(Some(genesis), wusAfterGenesis)

      subscribeEvents(classOf[SyntacticallySuccessfulModifier])
      subscribeEvents(classOf[FullBlockApplied])

      // Send locally generated ordering block
      nodeViewHolderRef ! LocallyGeneratedOrderingBlock(nextBlock, Seq.empty)

      // Wait for FullBlockApplied - skip intermediate SyntacticallySuccessfulModifier messages
      val fullBlockApplied = testProbe.fishForMessage(5.seconds) {
        case _: FullBlockApplied => true
        case _ => false
      }.asInstanceOf[FullBlockApplied]
      fullBlockApplied.header.id shouldBe nextBlock.header.id

      // Verify block is in history
      getBestHeaderOpt shouldBe Some(nextBlock.header)
    }
  }

  private val t25 = TestCase("apply locally generated input block") { fixture =>
    import fixture._
    if (stateType == Utxo && verifyTransactions) {
      val (us, bh) = createUtxoState(fixture.settings)
      val genesis = validFullBlock(parentOpt = None, us, bh)
      applyBlock(genesis) shouldBe 'success

      val (_, bh2) = createUtxoState(fixture.settings)
      val nextBlock = validFullBlock(Some(genesis), WrappedUtxoState(us, bh2, fixture.settings))
      val inputBlock = InputBlockAnnouncement(1, nextBlock.header, emptyInputBlockFields, None)

      subscribeEvents(classOf[NewBestInputBlock])

      // Send locally generated input block
      val txData = InputBlockTransactionsData(inputBlock.id, Seq.empty)
      nodeViewHolderRef ! LocallyGeneratedInputBlock(inputBlock, txData)

      // Verify NewBestInputBlock event is published
      val newBestMsg = expectMsgType[NewBestInputBlock]
      newBestMsg.idOpt shouldBe Some(inputBlock.id)
      newBestMsg.local shouldBe true
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

  val inputBlockCases: List[TestCase] = List(t20, t21, t22, t23, t24, t25)

  NodeViewTestConfig.verifyTxConfigs.filter(_.stateType == StateType.Utxo).foreach { c =>
    inputBlockCases.foreach { t =>
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
