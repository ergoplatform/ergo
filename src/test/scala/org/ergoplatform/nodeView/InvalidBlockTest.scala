package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{Extension, Header}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils._
import scorex.crypto.authds.ADKey
import scorex.testkit.utils.NoShrink

class InvalidBlockTest extends ErgoPropertyTest with NodeViewTestOps with NoShrink {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

  val cfg: NodeViewTestConfig = NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = true)

  property("apply invalid full block") {
    forAll(extensionGen, extensionGen) { (extension1, extension2) =>
      NodeViewFixture(cfg.toSettings) { fixture =>
        import fixture._
        val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
        val genesis = validFullBlock(parentOpt = None, us, bh)
        val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get
        if (verifyTransactions) {
          applyBlock(genesis)

          val block = validFullBlock(Some(genesis.header), wusAfterGenesis)
          val wusAfterBlock = wusAfterGenesis.applyModifier(block).get

          applyBlock(block)
          getBestHeaderOpt shouldBe Some(block.header)
          if (verifyTransactions) {
            getRootHash shouldBe Algos.encode(wusAfterBlock.rootHash)
          }
          getBestHeaderOpt shouldBe Some(block.header)

          val brokenBlock = generateInvalidFullBlock(block.header, wusAfterBlock, extension1)
          applyBlock(brokenBlock)

          val brokenBlock2 = generateInvalidFullBlock(block.header, wusAfterBlock, extension2)
          brokenBlock2.header should not be brokenBlock.header
          applyBlock(brokenBlock2)

          getBestFullBlockOpt shouldBe Some(block)
          getRootHash shouldBe Algos.encode(wusAfterBlock.rootHash)
          getBestHeaderOpt shouldBe Some(block.header)
        }
      }
    }
  }

  private def generateInvalidFullBlock(parentHeader: Header, parentState: WrappedUtxoState,
                                       extensionIn: Extension): ErgoFullBlock = {
    val brokenBlockIn = validFullBlock(Some(parentHeader), parentState)
    val headTx = brokenBlockIn.blockTransactions.txs.head
    val wrongBoxId: ADKey = ADKey !@@ Algos.hash("wrong input")
    val newInput = headTx.inputs.head.copy(boxId = wrongBoxId)
    val brokenTransactionsIn = brokenBlockIn.blockTransactions
      .copy(txs = headTx.copy(inputs = newInput +: headTx.inputs.tail) +: brokenBlockIn.blockTransactions.txs.tail)
    val brokenHeader = brokenBlockIn.header
      .copy(transactionsRoot = brokenTransactionsIn.digest, extensionRoot = extensionIn.digest)
    val brokenTransactions = brokenTransactionsIn.copy(headerId = brokenHeader.id)
    val brokenProofs = brokenBlockIn.adProofs.get.copy(headerId = brokenHeader.id)
    val extension = extensionIn.copy(headerId = brokenHeader.id)
    ErgoFullBlock(brokenHeader, brokenTransactions, extension, Some(brokenProofs))
  }

}
