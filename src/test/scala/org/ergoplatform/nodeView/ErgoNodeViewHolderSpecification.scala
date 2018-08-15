package org.ergoplatform.nodeView

import java.io.File

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
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SyntacticallySuccessfulModifier}
import scorex.crypto.authds.ADKey

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
      expectMsgType[SyntacticallySuccessfulModifier[BlockTransactions]]
      nodeViewRef ! LocallyGeneratedModifier(genesis.adProofs.get)
      expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]
      nodeViewRef ! LocallyGeneratedModifier(genesis.extension)
      expectMsgType[SyntacticallySuccessfulModifier[ADProofs]]

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))
    }
  }

  private val t5 = TestCase("apply full blocks after genesis") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(nodeViewRef, genesis, nodeViewConfig)

    val block = validFullBlock(Some(genesis.header), wusAfterGenesis)

    applyBlock(nodeViewRef, block, nodeViewConfig)
    if (nodeViewConfig.verifyTransactions) {
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
      applyBlock(nodeViewRef, genesis, nodeViewConfig)

      val block = validFullBlock(Some(genesis.header), wusAfterGenesis)
      val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

      applyBlock(nodeViewRef, block, nodeViewConfig)
      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(block.header))

      if (nodeViewConfig.verifyTransactions) {
        nodeViewRef ! rootHash(nodeViewConfig)
        expectMsg(Algos.encode(wusAfterBlock.rootHash))
      }

      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(block.header))

      val brokenBlock = generateInvalidFullBlock(block.header, wusAfterBlock)
      applyBlock(nodeViewRef, brokenBlock, nodeViewConfig)

      val brokenBlock2 = generateInvalidFullBlock(block.header, wusAfterBlock)
      brokenBlock2.header should not be brokenBlock.header

      applyBlock(nodeViewRef, brokenBlock2, nodeViewConfig)

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(block))
      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(wusAfterBlock.rootHash))
      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(block.header))
    }
  }

  private def generateInvalidFullBlock(parentHeader: Header, parentState: WrappedUtxoState) = {
    val extensionIn = extensionGen.sample.get
    val brokenBlockIn = validFullBlock(Some(parentHeader), parentState)
    val headTx = brokenBlockIn.blockTransactions.txs.head
    val newInput = headTx.inputs.head.copy(boxId = ADKey @@ Algos.hash("wrong input"))
    val brokenTransactionsIn = brokenBlockIn.blockTransactions
      .copy(txs = headTx.copy(inputs = newInput +: headTx.inputs.tail) +: brokenBlockIn.blockTransactions.txs.tail)
    val brokenHeader = brokenBlockIn.header.copy(transactionsRoot = brokenTransactionsIn.digest, extensionRoot = extensionIn.digest)
    val brokenTransactions = brokenTransactionsIn.copy(headerId = brokenHeader.id)
    val brokenProofs = brokenBlockIn.adProofs.get.copy(headerId = brokenHeader.id)
    val extension = extensionIn.copy(headerId = brokenHeader.id)
    ErgoFullBlock(brokenHeader, brokenTransactions, extension, Some(brokenProofs))
  }

  private val t8 = TestCase("switching for a better chain") { fixture =>
    import fixture._
    val (us, bh) = createUtxoState(Some(nodeViewRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(nodeViewRef, genesis, nodeViewConfig)

    nodeViewRef ! rootHash(nodeViewConfig)
    expectMsg(Algos.encode(wusAfterGenesis.rootHash))

    val chain1block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    applyBlock(nodeViewRef, chain1block1, nodeViewConfig)

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

    applyBlock(nodeViewRef, chain2block1, nodeViewConfig)

    nodeViewRef ! GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, Option[ErgoFullBlock]] { v =>
      v.history.bestFullBlockOpt
    }
    if (nodeViewConfig.verifyTransactions) expectMsg(Some(chain1block1)) else expectMsg(None)
    nodeViewRef ! bestHeaderOpt(nodeViewConfig)
    expectMsg(Some(chain1block1.header))

    val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
    val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

    chain2block1.header.stateRoot shouldEqual wusChain2Block1.rootHash

    applyBlock(nodeViewRef, chain2block2, nodeViewConfig)
    if (nodeViewConfig.verifyTransactions) {
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
      nodeViewRef ! LocallyGeneratedModifier(genesis.extension)

      nodeViewRef ! bestFullBlock(nodeViewConfig)
      expectMsg(Some(genesis))

      nodeViewRef ! modifierById(genesis.adProofs.get.id)
      expectMsg(genesis.adProofs)
    }
  }

  private val t10 = TestCase("NodeViewHolder start from inconsistent state") { fixture =>
    import fixture._
    if (nodeViewConfig.verifyTransactions) {

      val (us, bh) = createUtxoState(Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

      applyBlock(nodeViewRef, genesis, nodeViewConfig)

      val block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)

      applyBlock(nodeViewRef, block1, nodeViewConfig)
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

  private val t11 = TestCase("apply payload in incorrect order") { fixture =>
    import fixture._
    if (nodeViewConfig.verifyTransactions) {
      val (us, bh) = createUtxoState(Some(nodeViewRef))
      val genesis = validFullBlock(parentOpt = None, us, bh)
      val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

      applyBlock(nodeViewRef, genesis, nodeViewConfig)

      nodeViewRef ! rootHash(nodeViewConfig)
      expectMsg(Algos.encode(wusAfterGenesis.rootHash))

      val chain2block1 = validFullBlock(Some(genesis.header), wusAfterGenesis)
      val wusChain2Block1 = wusAfterGenesis.applyModifier(chain2block1).get
      val chain2block2 = validFullBlock(Some(chain2block1.header), wusChain2Block1)

      nodeViewRef ! LocallyGeneratedModifier(chain2block1.header)
      applyBlock(nodeViewRef, chain2block2, nodeViewConfig)

      nodeViewRef ! bestHeaderOpt(nodeViewConfig)
      expectMsg(Some(chain2block2.header))

      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(genesis.header.encodedId))
      applyPayload(nodeViewRef, chain2block1, nodeViewConfig)

      nodeViewRef ! bestFullBlockEncodedId(nodeViewConfig)
      expectMsg(Some(chain2block2.header.encodedId))
    }
  }

  val cases: List[TestCase] = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)

  allConfigs.foreach { c =>
    cases.foreach { t =>
      property(s"${t.name} - $c") {
        t.run(c)
      }
    }
  }
}
