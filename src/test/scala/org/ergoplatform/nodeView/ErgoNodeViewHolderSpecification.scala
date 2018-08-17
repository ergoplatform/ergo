package org.ergoplatform.nodeView

import java.io.File

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType.Utxo
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoNodeViewHolderTestHelpers
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.crypto.authds.{ADKey, SerializedAdProof}


class ErgoNodeViewHolderSpecification extends ErgoNodeViewHolderTestHelpers {

  private val t1 = TestCase("check genesis state") { fixture =>
    import fixture._
    nodeViewRef ! checkAfterGenesisState(nodeViewConfig)
    expectMsg(true)
  }

  private val t2 = TestCase("check history after genesis") { fixture =>
    import fixture._
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)
  }

  private val t3 = TestCase("apply valid block header") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])

    //sending header
    nodeViewRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(0)

    nodeViewRef ! heightOf(block.header.id, nodeViewConfig)
    expectMsg(Some(0))

    nodeViewRef ! lastHeadersLength(10, nodeViewConfig)
    expectMsg(1)

    nodeViewRef ! openSurfaces(nodeViewConfig)
    expectMsg(Seq(block.header.id))

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(block.header))
  }

  private val t4 = TestCase("apply valid block as genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)

      nodeViewConfig.stateType match {
        case StateType.Digest =>
          expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
          expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
        case StateType.Utxo =>
          expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
          expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      }

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))
    }
  }

  private val t5 = TestCase("apply full blocks after genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(block.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(block.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(block.aDProofs.get)
      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(block.header))

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(1)

    nodeViewRef ! lastHeadersLength(10, nodeViewConfig)
    expectMsg(2)
  }

  private val t6 = TestCase("add transaction to memory pool") { fixture =>
    import fixture._
    if (nodeViewConfig.stateType == Utxo) {
      val (_, bh) = createUtxoState(Some(nodeViewRef))
      val tx = validTransactionsFromBoxHolder(bh)._1.head
      subscribeEvents(classOf[FailedTransaction[_]])
      nodeViewRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      expectNoMsg()
      nodeViewRef ! poolSize(nodeViewConfig)
      expectMsg(1)
    }
  }

  private val t7 = TestCase("apply invalid full block") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
    // TODO looks like another bug is still present here, see https://github.com/ergoplatform/ergo/issues/309
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.header)
      if (nodeViewConfig.verifyTransactions) {
        nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
        nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
      }

      val block = validFullBlock(Some(genesis.header), wusAfterGenesis)
      val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

      nodeViewRef ! LocallyGeneratedModifier(block.header)
      if (nodeViewConfig.verifyTransactions) {
        nodeViewRef ! LocallyGeneratedModifier(block.blockTransactions)
        nodeViewRef ! LocallyGeneratedModifier(block.aDProofs.get)
        nodeViewRef ! rootHash(nodeViewConfig)
        expectMsg(Algos.encode(wusAfterBlock.rootHash))
      }

      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(block.header))

      val brokenBlock = generateInvalidFullBlock(block.header, wusAfterBlock)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock.header)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock.aDProofs.get)

      val brokenBlock2 = generateInvalidFullBlock(block.header, wusAfterBlock)
      brokenBlock2.header should not be brokenBlock.header
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock2.header)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock2.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(brokenBlock2.aDProofs.get)

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block))
      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(wusAfterBlock.rootHash))
      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(block.header))

    }
  }

  private def generateInvalidFullBlock(parentHeader: Header, parentState: WrappedUtxoState) = {
    val brokenBlockIn = validFullBlock(Some(parentHeader), parentState)
    val headTx = brokenBlockIn.blockTransactions.txs.head
    val newInput = headTx.inputs.head.copy(boxId = ADKey @@ Algos.hash("wrong input"))
    val brokenTransactionsIn = brokenBlockIn.blockTransactions
      .copy(txs = headTx.copy(inputs = newInput +: headTx.inputs.tail) +: brokenBlockIn.blockTransactions.txs.tail)
    val brokenHeader = brokenBlockIn.header.copy(transactionsRoot = brokenTransactionsIn.digest)
    val brokenTransactions = brokenTransactionsIn.copy(headerId = brokenHeader.id)
    val brokenProofs = brokenBlockIn.aDProofs.get.copy(headerId = brokenHeader.id)
    ErgoFullBlock(brokenHeader, brokenTransactions, Some(brokenProofs))
  }

  private val t8 = TestCase("switching for a better chain") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(wusAfterGenesis.rootHash))

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(chain1block1.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain1block1.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain1block1.aDProofs.get)
    }

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    nodeViewRef ! LocallyGeneratedModifier(chain2block1.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.aDProofs.get)
    }

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    nodeViewRef ! LocallyGeneratedModifier(chain2block2.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.aDProofs.get)
      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(chain2block2.header.encodedId))
    }

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain2block2.header))

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(chain2block2.header.stateRoot))

  }

  private val t9 = TestCase("UTXO state should generate ADProofs and put them in history") { fixture =>
    import fixture._
    if (nodeViewConfig.stateType == StateType.Utxo) {
      val (us, bh) = createUtxoState(Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)

      nodeViewRef ! LocallyGeneratedModifier(genesis.header)

      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))

      nodeViewRef ! modifierById(genesis.aDProofs.get.id)
      expectMsg(genesis.aDProofs)
    }
  }

  private val t10 = TestCase("NodeViewHolder start from inconsistent state") { fixture =>
    import fixture._
    if (nodeViewConfig.verifyTransactions) {

      val (us, bh) = createUtxoState(Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

      nodeViewRef ! LocallyGeneratedModifier(genesis.header)
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)

      val block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

      nodeViewRef ! LocallyGeneratedModifier(block1.header)
      nodeViewRef ! LocallyGeneratedModifier(block1.aDProofs.get)
      nodeViewRef ! LocallyGeneratedModifier(block1.blockTransactions)
      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block1))

      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(block1.header.stateRoot))

      system.stop(nodeViewRef)
      val stateDir = new File(s"${nodeViewDir.getAbsolutePath}/state")
      for (file <- stateDir.listFiles) file.delete

      withRecoveredNodeViewRef { nodeViewRef2 =>
        nodeViewRef2 ! rootHash(nodeViewConfig)
        expectMsg(Algos.encode(block1.header.stateRoot))
      }
    }
  }

  private val t11 = TestCase("apply blocks in incorrect order") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    nodeViewRef ! LocallyGeneratedModifier(genesis.header)
    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(genesis.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(genesis.aDProofs.get)
    }

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(wusAfterGenesis.rootHash))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)
    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    nodeViewRef ! LocallyGeneratedModifier(chain2block1.header)
    nodeViewRef ! LocallyGeneratedModifier(chain2block2.header)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain2block2.header))

    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block2.aDProofs.get)
      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(genesis.header.encodedId))

    }

    if (nodeViewConfig.verifyTransactions) {
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.blockTransactions)
      nodeViewRef ! LocallyGeneratedModifier(chain2block1.aDProofs.get)
      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(chain2block2.header.encodedId))
    }
  }

  private val t12 = TestCase("Do not apply txs with wrong header id") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)
    logger.error(s"HERE ${block.aDProofs.toString}")

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(0)

    nodeViewRef ! heightOf(block.header.id, nodeViewConfig)
    expectMsg(Some(0))

    val randomId = modifierIdGen.sample.get
    val wrongTxs1 = block.blockTransactions.copy(headerId = randomId)
    val wrongTxs2 = {
      val txs = block.blockTransactions.transactions
      val tx = txs.head
      val wrongOutputs = tx.outputCandidates.map(o =>
        new ErgoBoxCandidate(o.value + 10L, o.proposition, o.additionalTokens, o.additionalRegisters)
      )
      val wrongTxs = tx.copy(outputCandidates = wrongOutputs) +: txs.tail
      block.blockTransactions.copy(txs = wrongTxs)
    }


    nodeViewRef ! LocallyGeneratedModifier[BlockTransactions](wrongTxs1)
    expectMsgType[SyntacticallyFailedModification[BlockTransactions]]

    nodeViewRef ! LocallyGeneratedModifier[BlockTransactions](wrongTxs2)
    expectMsgType[SyntacticallyFailedModification[BlockTransactions]]

    nodeViewRef ! LocallyGeneratedModifier[BlockTransactions](block.blockTransactions)
    expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
  }

  private val t13 = TestCase("Do not apply wrong AdProofs") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)
    logger.error(s"HERE ${block.aDProofs.toString}")

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    val randomId = modifierIdGen.sample.get
    val wrongProofsBytes = SerializedAdProof @@ block.aDProofs.get.proofBytes.reverse
    val wrongProofs1 = block.aDProofs.map(_.copy(headerId = randomId))
    val wrongProofs2 = block.aDProofs.map(_.copy(proofBytes = wrongProofsBytes))

    nodeViewRef ! LocallyGeneratedModifier[ADProofs](wrongProofs1.get)
    expectMsgType[SyntacticallyFailedModification[ADProofs]]
    nodeViewRef ! LocallyGeneratedModifier[ADProofs](wrongProofs2.get)
    expectMsgType[SyntacticallyFailedModification[ADProofs]]

    nodeViewRef ! LocallyGeneratedModifier[ADProofs](block.aDProofs.get)
    expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
  }

  val cases: List[TestCase] = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)

  allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }

  val verifyingTxCases = List(t12, t13)

  val verifyTxConfigs = allConfigs.filter(_.verifyTransactions)

  verifyTxConfigs.foreach { c =>
    verifyingTxCases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }

  private val expectedGenesisIdOpt: Option[ModifierId] = modifierIdGen.sample
  private val expectedGenesisIdString = expectedGenesisIdOpt.map(Algos.encode)

  private val genesisIdFromConfigCheck1 = TestCase("do not apply genesis block header if " +
    "it's not equal to genesisId from config") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)

    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    //sending header
    nodeViewRef ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallyFailedModification[Header]]
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef ! historyHeight(nodeViewConfig)
    expectMsg(-1)
  }

  private val genesisIdFromConfigCheck2 = TestCase("apply genesis block header if " +
    "it's equal to genesisId from config") { fixture =>
    import fixture._

    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val block = validFullBlock(None, us, bh)

    val nodeViewDir1: java.io.File = createTempDir
    val nodeViewRef1: ActorRef = actorRef(
      nodeViewConfig.copy(genesisId = Some(Algos.encode(block.header.id))),
      Option(nodeViewDir1)
    )

    nodeViewRef1 ! bestHeaderOpt(nodeViewConfig)
    expectMsg(None)

    nodeViewRef1 ! historyHeight(nodeViewConfig)
    expectMsg(-1)

    subscribeEvents(classOf[SyntacticallySuccessfulModifier[_]])
    subscribeEvents(classOf[SyntacticallyFailedModification[_]])

    nodeViewRef1 ! LocallyGeneratedModifier[Header](block.header)
    expectMsgType[SyntacticallySuccessfulModifier[Header]]

    nodeViewRef1 ! historyHeight(nodeViewConfig)
    expectMsg(0)

    nodeViewRef1 ! heightOf(block.header.id, nodeViewConfig)
    expectMsg(Some(0))
  }

  property(genesisIdFromConfigCheck1.name) {
    genesisIdFromConfigCheck1.run(NodeViewHolderConfig(
      Digest,
      false,
      false,
      expectedGenesisIdString
    ))
  }

  property(genesisIdFromConfigCheck2.name) {
    genesisIdFromConfigCheck2.run(NodeViewHolderConfig(
      Digest,
      false,
      false,
      expectedGenesisIdString
    ))
  }

}
