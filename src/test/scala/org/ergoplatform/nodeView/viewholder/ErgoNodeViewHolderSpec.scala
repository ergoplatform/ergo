package org.ergoplatform.nodeView.viewholder

import java.io.File
import org.ergoplatform.core.idToVersion
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.{ErgoFullBlock, SnapshotsInfoTypeId}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.transaction.TooHighCostError
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper, TestCase}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.validation.MalformedModifierError
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedModifier}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ChainProgress
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome.Accepted
import org.ergoplatform.wallet.utils.FileUtils
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.util.{ModifierId, bytesToId}
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}

class ErgoNodeViewHolderSpec extends ErgoCorePropertyTest with NodeViewTestOps with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._

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

  private val t20 = TestCase("SemanticallyFailedModification carries failing transaction id") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val wus = WrappedUtxoState(us, bh, fixture.settings)

    val genesis = validFullBlock(None, wus)

    // Apply genesis through the standard NVH route first, so the next block can reference it.
    applyBlock(genesis) shouldBe 'success
    val wusAfterGenesis = wus.applyModifier(genesis)(_ => ()).get

    val box = wusAfterGenesis.takeBoxes(1).head
    val validTx = validTransactionFromBoxes(IndexedSeq(box), new RandomWrapper)
    val invalidOutputs = validTx.outputCandidates.map { out =>
      new ErgoBoxCandidate(-1, out.ergoTree, out.creationHeight, out.additionalTokens, out.additionalRegisters)
    }
    val invalidTx = validTx.copy(outputCandidates = invalidOutputs)

    val (adProofBytes, adDigest) = wusAfterGenesis.proofsForTransactions(Seq(invalidTx)).get
    val time = genesis.header.timestamp + 1
    val parentOpt = Some(genesis.header)
    val parentExtensionOpt = wusAfterGenesis.stateContext.lastExtensionOpt
    val nipopowAlgos = new NipopowAlgos(settings.chainSettings)
    val extension = parameters.toExtensionCandidate ++
      nipopowAlgos.interlinksToExtension(nipopowAlgos.updateInterlinks(parentOpt, parentExtensionOpt))

    val invalidBlock = settings.chainSettings.powScheme.proveBlock(
      parentOpt,
      Header.InitialVersion,
      settings.chainSettings.initialNBits,
      adDigest,
      adProofBytes,
      Seq(invalidTx),
      time,
      extension,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get

    subscribeEvents(classOf[SemanticallyFailedModification])

    if (verifyTransactions) {
      applyBlock(invalidBlock) shouldBe 'success

      val semFailed = expectMsgType[SemanticallyFailedModification]
      ErgoNodeViewHolder.extractFailedTxId(semFailed.error) shouldBe Some(invalidTx.id)
    }
  }

  private val t21 = TestCase("txScriptFailure carries failing transaction id") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(fixture.settings)
    val wus = WrappedUtxoState(us, bh, fixture.settings)

    val genesis = validFullBlock(None, wus)

    // Apply genesis through the standard NVH route first.
    applyBlock(genesis) shouldBe 'success
    val wusAfterGenesis = wus.applyModifier(genesis)(_ => ()).get

    // Create a valid tx that pays to a FalseTree output.
    val box = wusAfterGenesis.takeBoxes(1).head
    val validTx = validTransactionFromBoxes(IndexedSeq(box), outputsProposition = FalseTree)

    val validBlock = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    // Apply valid block and advance wrapped state.
    applyBlock(validBlock) shouldBe 'success
    val wusAfterValidBlock = wusAfterGenesis.applyModifier(validBlock)(_ => ()).get

    // Create a tx spending the FalseTree output; prover cannot sign it, so it has empty proofs.
    val falseTreeBox = validTx.outputs.head
    val invalidTx = validTransactionFromBoxes(IndexedSeq(falseTreeBox))

    val invalidBlock = validFullBlock(Some(validBlock), wusAfterValidBlock, Seq(invalidTx))

    subscribeEvents(classOf[SemanticallyFailedModification])

    if (verifyTransactions) {
      applyBlock(invalidBlock) shouldBe 'success

      val semFailed = expectMsgType[SemanticallyFailedModification]
      ErgoNodeViewHolder.extractFailedTxId(semFailed.error) shouldBe Some(invalidTx.id)
    }
  }

  /**
    * Applies a valid NiPoPoW proof from a separately generated chain to an empty node view holder.
    * With utxoBootstrap enabled, the node must start UTXO set snapshot bootstrap right after the proof
    * (headers chain marked as synced, no full blocks downloaded yet). Without utxoBootstrap, normal
    * full blocks downloading must be started instead.
    */
  private val t22 = TestCase("apply nipopow proof to empty holder") { fixture =>
    import fixture._

    // sender history: generate a chain and a NiPoPoW proof for it
    val senderHistory = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1)
    val senderChain = genChain(5000, senderHistory)
    val updSenderHistory = applyChain(senderHistory, senderChain)
    val popowProof = updSenderHistory.nipopowSerializer.parseBytes(updSenderHistory.popowProofBytes().get)

    // the holder must expect sender's genesis id, as with nipopow bootstrapping
    updateConfig(genesisIdConfig(updSenderHistory.bestHeaderAtHeight(1).map(_.id)))

    subscribeEvents(classOf[ChangedHistory])

    nodeViewHolderRef ! ProcessNipopow(popowProof)
    expectMsgType[ChangedHistory]

    getHistory.headersHeight shouldBe updSenderHistory.headersHeight
    getHistory.isHeadersChainSynced shouldBe true

    val toDownloadMap = getHistory.nextModifiersToDownload(1, (_, id) => !getHistory.contains(id))
    if (settings.nodeSettings.utxoSettings.utxoBootstrap) {
      // no full blocks must be downloaded before UTXO set snapshot is applied, ask peers for snapshots
      toDownloadMap shouldBe Map(SnapshotsInfoTypeId.value -> Seq.empty)
    } else {
      // normal nipopow bootstrap: full blocks downloading is started, no snapshot request
      toDownloadMap.contains(SnapshotsInfoTypeId.value) shouldBe false
    }

    // second proof must not be applied as history is not empty anymore
    nodeViewHolderRef ! ProcessNipopow(popowProof)
    expectNoMsg()
    getHistory.headersHeight shouldBe updSenderHistory.headersHeight
  }

  val cases: List[TestCase] = List(t0, t1, t2, t3, t3a, t4, t5, t6, t7, t8, t9)

  NodeViewTestConfig.allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(parameters, c)
      }
    }
  }

  val verifyingTxCases: List[TestCase] = List(t10, t11, t12, t13, t20, t21)

  NodeViewTestConfig.verifyTxConfigs.foreach { c =>
    verifyingTxCases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(parameters, c)
      }
    }
  }

  property("preserve a prepared UTXO snapshot state across restart") {
    val protoSettings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false)
      .toSettings
    val snapshotSettings = protoSettings.copy(
      nodeSettings = protoSettings.nodeSettings.copy(
        utxoSettings = protoSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )

    new NodeViewFixture(snapshotSettings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(settings)
      val snapshotBlock = validFullBlock(None, sourceState, boxHolder)
      val sourceAtSnapshot = WrappedUtxoState(sourceState, boxHolder, settings)
        .applyModifier(snapshotBlock)(_ => ())
        .get
      val nextBlock = validFullBlock(Some(snapshotBlock), sourceAtSnapshot)

      applyHeader(snapshotBlock.header).get
      getHistory.onUtxoSnapshotApplied(snapshotBlock.height)
      stopNodeViewHolder()

      val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
      fixture.deleteRecursive(stateDir)
      stateDir.mkdirs() shouldBe true
      val persistedGenesis = ErgoState
        .generateGenesisUtxoState(stateDir, settings, Some(parameters))
        ._1
      val persistedSnapshot = persistedGenesis
        .applyModifier(snapshotBlock, None)(_ => ())
        .get
      persistedSnapshot.closeStorage()

      startNodeViewHolder()

      getRootHash shouldBe Algos.encode(snapshotBlock.header.stateRoot)
      applyBlock(nextBlock) shouldBe 'success
      getRootHash shouldBe Algos.encode(nextBlock.header.stateRoot)

      sourceAtSnapshot.closeStorage()
    }
  }

  property("reject a prepared UTXO snapshot state from a noncanonical fork on restart") {
    val protoSettings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false)
      .toSettings
    val snapshotSettings = protoSettings.copy(
      nodeSettings = protoSettings.nodeSettings.copy(
        utxoSettings = protoSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )

    new NodeViewFixture(snapshotSettings, parameters).apply { fixture =>
      import fixture._

      val (sourceState, boxHolder) = createUtxoState(settings)
      val transactions = validTransactionsFromBoxHolder(boxHolder, new RandomWrapper)._1
      val firstTimestamp = System.currentTimeMillis()
      val firstBlock = validFullBlock(None, sourceState, transactions, Some(firstTimestamp))
      val secondBlock = validFullBlock(None, sourceState, transactions, Some(firstTimestamp + 1))
      firstBlock.id should not be secondBlock.id
      java.util.Arrays.equals(firstBlock.header.stateRoot, secondBlock.header.stateRoot) shouldBe true

      applyHeader(firstBlock.header).get
      applyHeader(secondBlock.header).get
      val canonicalHeader = getHistory.bestHeaderAtHeight(firstBlock.height).get
      val noncanonicalBlock = Seq(firstBlock, secondBlock).find(_.id != canonicalHeader.id).get
      java.util.Arrays.equals(noncanonicalBlock.header.stateRoot, canonicalHeader.stateRoot) shouldBe true
      getHistory.onUtxoSnapshotApplied(noncanonicalBlock.height)
      stopNodeViewHolder()

      val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
      fixture.deleteRecursive(stateDir)
      stateDir.mkdirs() shouldBe true
      val persistedGenesis = ErgoState
        .generateGenesisUtxoState(stateDir, settings, Some(parameters))
        ._1
      val persistedForkState = persistedGenesis
        .applyModifier(noncanonicalBlock, None)(_ => ())
        .get
      persistedForkState.version shouldBe idToVersion(noncanonicalBlock.id)
      persistedForkState.version should not be idToVersion(canonicalHeader.id)
      java.util.Arrays.equals(persistedForkState.rootDigest, canonicalHeader.stateRoot) shouldBe true
      persistedForkState.closeStorage()

      startNodeViewHolder()

      getRootHash shouldBe Algos.encode(settings.chainSettings.genesisStateDigest)

      sourceState.closeStorage()
    }
  }

  property("require every prepared UTXO snapshot trust signal") {
    val (state, boxHolder) = createUtxoState(settings)

    try {
      val header = validFullBlock(None, state, boxHolder).header
      val matchingVersion = idToVersion(header.id)
      val mismatchedVersion = idToVersion(Header.GenesisParentId)
      val mismatchedRoot = header.stateRoot.clone()
      mismatchedRoot(0) = (mismatchedRoot(0) ^ 1).toByte

      val cases = Seq(
        ("all signals match", true, true, true, matchingVersion, header.stateRoot, Some(header), true),
        ("state is not UTXO", false, true, true, matchingVersion, header.stateRoot, Some(header), false),
        ("UTXO bootstrap disabled", true, false, true, matchingVersion, header.stateRoot, Some(header), false),
        ("snapshot marker absent", true, true, false, matchingVersion, header.stateRoot, Some(header), false),
        ("canonical header absent", true, true, true, matchingVersion, header.stateRoot, None, false),
        ("state version mismatch", true, true, true, mismatchedVersion, header.stateRoot, Some(header), false),
        ("state root mismatch", true, true, true, matchingVersion, mismatchedRoot, Some(header), false)
      )

      cases.foreach { case (clue, stateIsUtxo, utxoBootstrap, snapshotApplied, stateVersion, stateRoot, headerOpt, expected) =>
        withClue(clue) {
          ErgoNodeViewHolder.isPreparedUtxoSnapshotState(
            stateIsUtxo,
            utxoBootstrap,
            snapshotApplied,
            stateVersion,
            stateRoot,
            headerOpt) shouldBe expected
        }
      }

      var utxoBootstrapRead = false
      var snapshotMarkerRead = false
      var snapshotHeaderRead = false
      ErgoNodeViewHolder.isPreparedUtxoSnapshotState(
        stateIsUtxo = false,
        utxoBootstrap = {
          utxoBootstrapRead = true
          true
        },
        snapshotApplied = {
          snapshotMarkerRead = true
          true
        },
        stateVersion = matchingVersion,
        stateRoot = header.stateRoot,
        snapshotHeaderOpt = {
          snapshotHeaderRead = true
          Some(header)
        }) shouldBe false
      utxoBootstrapRead shouldBe false
      snapshotMarkerRead shouldBe false
      snapshotHeaderRead shouldBe false

      ErgoNodeViewHolder.isPreparedUtxoSnapshotState(
        stateIsUtxo = true,
        utxoBootstrap = false,
        snapshotApplied = {
          snapshotMarkerRead = true
          true
        },
        stateVersion = matchingVersion,
        stateRoot = header.stateRoot,
        snapshotHeaderOpt = {
          snapshotHeaderRead = true
          Some(header)
        }) shouldBe false
      snapshotMarkerRead shouldBe false
      snapshotHeaderRead shouldBe false

      ErgoNodeViewHolder.isPreparedUtxoSnapshotState(
        stateIsUtxo = true,
        utxoBootstrap = true,
        snapshotApplied = false,
        stateVersion = matchingVersion,
        stateRoot = header.stateRoot,
        snapshotHeaderOpt = {
          snapshotHeaderRead = true
          Some(header)
        }) shouldBe false
      snapshotHeaderRead shouldBe false
    } finally {
      state.closeStorage()
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

  property("nipopow proof starts utxo snapshot bootstrap when utxoBootstrap enabled") {
    t22.run(parameters, NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = true, utxoBootstrap = true))
  }

  property("nipopow proof starts full blocks downloading when utxoBootstrap disabled") {
    t22.run(parameters, NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = true, utxoBootstrap = false))
  }

  property("extractFailedTxId should extract failing transaction id from validation error shapes") {
    forAll(invalidErgoTransactionGen) { tx =>
      // transaction-level error tagged with the transaction id
      val txError =
        new MalformedModifierError("tx failed", tx.id, ErgoTransaction.modifierTypeId)
      ErgoNodeViewHolder.extractFailedTxId(txError) shouldBe Some(tx.id)

      // block-level error with non-transaction modifier id should be ignored
      val blockError = new MalformedModifierError(
        "block failed",
        bytesToId(Array.fill(32)(0.toByte)),
        BlockTransactions.modifierTypeId
      )
      ErgoNodeViewHolder.extractFailedTxId(blockError) shouldBe None

      // header-level error should be ignored
      val headerError = new MalformedModifierError("header failed", tx.id, Header.modifierTypeId)
      ErgoNodeViewHolder.extractFailedTxId(headerError) shouldBe None

      // too high cost error carries the transaction itself
      ErgoNodeViewHolder.extractFailedTxId(TooHighCostError(tx, Some(1000))) shouldBe Some(tx.id)

      // errors wrapped into other exceptions are found via the cause chain
      val wrapped = new Exception("wrapper", new RuntimeException(txError))
      ErgoNodeViewHolder.extractFailedTxId(wrapped) shouldBe Some(tx.id)

      // unrelated exception
      ErgoNodeViewHolder.extractFailedTxId(new Exception("unrelated")) shouldBe None
    }
  }

}
