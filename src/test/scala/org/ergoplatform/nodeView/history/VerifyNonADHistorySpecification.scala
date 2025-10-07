package org.ergoplatform.nodeView.history

import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.modifiers.{ErgoFullBlock, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.{ErgoCorePropertyTest, MapPimp}

class VerifyNonADHistorySpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.tools.MinerBench._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private def genHistory() =
    generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

  property("block sections application in incorrect order") {
    var history = genHistory()
    val chain = genChain(6, history)
    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(chain.last.header)
    }
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    chain.foreach(fb => history.append(fb.extension).get)

    history = history.append(chain(1).blockTransactions).get._1
    history.bestFullBlockOpt shouldBe None
    val pi1 = history.append(chain(0).blockTransactions).get._2
    history.bestFullBlockOpt.value shouldBe chain(1)
    pi1.toApply.length shouldBe 2

    chain.drop(3).foreach(c => history.append(c.blockTransactions))
    history.bestFullBlockOpt.value.header.height shouldBe chain(1).header.height

    val (hi, pi) = history.append(chain(2).blockTransactions).get
    val expected = chain.drop(2)

    expected.forall(b => hi.asInstanceOf[FullBlockProcessor].isInBestFullChain(b.id)) shouldBe true

    pi.toApply.map(_.asInstanceOf[ErgoFullBlock]) shouldBe expected
  }

  property("full chain status updating") {

    def isInBestChain(b: ErgoFullBlock, h: ErgoHistory): Boolean = {
      h.asInstanceOf[FullBlockProcessor].isInBestFullChain(b.id)
    }

    var history = genHistory()
    val initChain = genChain(6, history)

    val stableChain = initChain.take(3)
    val altChain = genChain(8, stableChain.last).tail

    // apply initial initChain (1 to 6)
    history = applyChain(history, initChain)

    history.bestFullBlockIdOpt.get shouldEqual initChain.last.id
    initChain.forall(b => isInBestChain(b, history)) shouldBe true

    // apply better initChain forking initial one (1 to 3 (init initChain), 3 to 11 (new initChain))
    history = applyChain(history, altChain)

    history.bestFullBlockIdOpt.get shouldEqual altChain.last.id
    // first blocks from init chain are still marked as best chain
    stableChain.forall(b => isInBestChain(b, history)) shouldBe true
    // other blocks from init chain are no more in best chain
    initChain.drop(3).forall(b => !isInBestChain(b, history)) shouldBe true
    // all blocks from fork are marked as best chain
    altChain.forall(b => isInBestChain(b, history)) shouldBe true

    val invalidChainHead = altChain.head

    // invalidate modifier from fork
    history.reportModifierIsInvalid(invalidChainHead.blockTransactions,
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty))

    history.bestFullBlockIdOpt.get shouldEqual initChain.last.id

    // all blocks from init chain are marked as best chain again
    initChain.forall(b => isInBestChain(b, history)) shouldBe true
    // blocks from fork no longer marked as best chain
    altChain.forall(b => !isInBestChain(b, history)) shouldBe true
  }

  property("bootstrap from headers and last full blocks") {
    var history = genHistory()
    history.bestFullBlockOpt shouldBe None

    val chain = genChain(BlocksToKeep * 2)

    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    history.bestHeaderOpt.value shouldBe chain.last.header
    history.bestFullBlockOpt shouldBe None

    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(chain.last.header)
    }

    // Until UTXO snapshot synchronization is implemented, we should always start to apply full blocks from genesis
    val fullBlocksToApply = chain

    history = history.append(fullBlocksToApply.head.blockTransactions).get._1
    history = history.append(fullBlocksToApply.head.extension).get._1
    history.bestFullBlockOpt.get.header shouldBe fullBlocksToApply.head.header
  }

  property("nextModifiersToDownload") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep)
    history = applyBlock(history, chain.head)
    history.bestFullBlockOpt.value shouldBe chain.head
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header).tail))

    val missedChain = chain.tail.toList
    val missedBS = missedChain.flatMap { fb =>
      Seq((BlockTransactions.modifierTypeId, fb.blockTransactions.encodedId), (Extension.modifierTypeId, fb.extension.encodedId))
    }.foldLeft(Map.empty[NetworkObjectTypeId.Value, Seq[String]]) { case (newAcc, (mType, mId)) =>
      newAcc.adjust(mType)(_.fold(Seq(mId))(_ :+ mId))
    }

    history.nextModifiersToDownload(1, (_, id) => !history.contains(id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS.mapValues(_.take(1)).view.force

    history.nextModifiersToDownload(2 * (BlocksToKeep - 1), (_, id) => !history.contains(id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS

    history.nextModifiersToDownload(2, (_, id) => !history.contains(id) && (id != missedChain.head.blockTransactions.id))
      .map(id => (id._1, id._2.map(Algos.encode))) shouldEqual missedBS.mapValues(_.take(2).filter( _ != missedChain.head.blockTransactions.id)).view.force
  }

  property("append header as genesis") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history, diffBitsOpt = None, useRealTs = false).head
    val updHistory = history.append(header).get._1
    updHistory.bestHeaderOpt shouldBe Some(header)
    val restoredHeader = updHistory.modifierById(header.id)
    restoredHeader shouldBe Some(header)

    val bytesFromSerializer = HeaderSerializer.toBytes(header)
    val bytesFromDb = updHistory.modifierBytesById(header.id).get
    bytesFromSerializer.sameElements(bytesFromDb) shouldBe true
  }

  property("append header as genesis - via applyHeaderChain") {
    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = genHeaderChain(1, history, diffBitsOpt = None, useRealTs = false).head

    val updHistory = applyHeaderChain(history, HeaderChain(Seq(header)))
    updHistory.bestHeaderOpt shouldBe Some(header)
    updHistory.modifierById(header.id) shouldBe Some(header)
  }

  property("append header to genesis - 2") {
    val (us, bh) = createUtxoState(settings)

    val block = validFullBlock(None, us, bh)

    val history = genHistory()
    history.bestHeaderOpt shouldBe None
    val header = block.header

    HeaderSerializer.parseBytes(HeaderSerializer.toBytes(header)) shouldBe header

    val actualHeader = history.append(header).get._1.bestHeaderOpt.value
    actualHeader shouldBe header
  }

  property("Appended headers and transactions blocks to best chain in tx history") {
    var history = genHistory()

    history = applyChain(history, genChain(BlocksInChain, history))

    genChain(BlocksInChain, history).tail.foreach { fullBlock =>
      val startFullBlock = history.bestFullBlockOpt.value

      val header = fullBlock.header
      val txs = fullBlock.blockTransactions
      val extension = fullBlock.extension
      history.contains(header) shouldBe false
      history.contains(txs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe true
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe false
      history.contains(extension) shouldBe false
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe true
      history.applicable(extension) shouldBe true
      history.bestHeaderOpt.value shouldBe header

      history.bestFullBlockOpt.value shouldBe startFullBlock

      history = history.append(txs).get._1
      history = history.append(extension).get._1

      history.contains(header) shouldBe true
      history.contains(txs) shouldBe true
      history.contains(extension) shouldBe true
      history.applicable(header) shouldBe false
      history.applicable(txs) shouldBe false
      history.applicable(extension) shouldBe false
      history.bestHeaderOpt.value shouldBe header
      history.bestFullBlockOpt.value.header shouldBe fullBlock.header
    }
  }

  property("chain reorganization scenarios - testing isLinkable indirectly") {
    var history = genHistory()
    
    // Create initial chain
    val chain = genChain(6, history)
    history = applyChain(history, chain)
    
    // Create a fork that extends from block 3
    val forkPoint = chain.take(3).last
    val forkChain = genChain(4, forkPoint).tail
    
    // Apply the fork chain - this should trigger chain reorganization
    // The isLinkable method is used internally to check if blocks can be linked
    history = applyChain(history, forkChain)
    
    // Verify that the best chain has been updated to the fork
    history.bestFullBlockIdOpt.get shouldEqual forkChain.last.id
    
    // Verify that blocks from the original chain that are not in the best chain anymore
    // are still accessible but not marked as best chain
    chain.drop(3).forall { block =>
      !history.asInstanceOf[FullBlockProcessor].isInBestFullChain(block.id)
    } shouldBe true
    
    // Verify that blocks from the fork are marked as best chain
    forkChain.forall { block =>
      history.asInstanceOf[FullBlockProcessor].isInBestFullChain(block.id)
    } shouldBe true
  }

  property("multiple fork scenarios - testing isLinkable comprehensively") {
    var history = genHistory()
    
    // Test 1: Block extending best chain directly
    val chain = genChain(8, history)
    // Apply headers first
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))
    // Then apply full blocks
    history = applyChain(history, chain)
    
    // Update best full block to allow applying new blocks
    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(chain.last.header)
    }
    
    // Create a block that directly extends the best chain
    val directExtension = genChain(1, chain.last).head
    
    // This should be applicable as it directly extends the best chain
    // The isLinkable method would return true for this case
    history.applicable(directExtension.header) shouldBe true
    
    // Test 2: Multiple forks from different points
    val forkPoint1 = chain.take(4).last
    val forkChain1 = genChain(3, forkPoint1).tail
    
    val forkPoint2 = chain.take(6).last  
    val forkChain2 = genChain(2, forkPoint2).tail
    
    // Apply first fork - apply headers first
    history = applyHeaderChain(history, HeaderChain(forkChain1.map(_.header)))
    history = applyChain(history, forkChain1)
    
    // Apply second fork - apply headers first
    history = applyHeaderChain(history, HeaderChain(forkChain2.map(_.header)))
    history = applyChain(history, forkChain2)
    
    // Verify best chain is updated to the longest fork
    history.bestFullBlockIdOpt.get shouldEqual forkChain2.last.id
    
    // Test 3: Orphan blocks with no connection to existing chain
    // Create an independent chain that doesn't connect to the existing history
    val independentChain = genChain(3)
    val orphanBlock = independentChain.last
    
    // Orphan blocks should not be applicable as they don't connect to existing chain
    // The isLinkable method would return false for this case
    // Note: The orphan block's parent doesn't exist in our history
    history.applicable(orphanBlock.header) shouldBe false
    
    // Test 4: Blocks extending non-best chains (forks)
    val forkExtension = genChain(1, forkChain1.last).head
    
    // Blocks extending existing forks should be applicable
    // The isLinkable method would return true for this case
    history.applicable(forkExtension.header) shouldBe true
  }

  property("complex chain structure - testing isLinkable with deep forks") {
    var history = genHistory()
    
    // Create a main chain
    val mainChain = genChain(10, history)
    // Apply headers first
    history = applyHeaderChain(history, HeaderChain(mainChain.map(_.header)))
    // Then apply full blocks
    history = applyChain(history, mainChain)
    
    // Update best full block to allow applying new blocks
    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(mainChain.last.header)
    }
    
    // Create multiple forks at different heights
    val forkAtHeight3 = genChain(5, mainChain.take(3).last).tail
    val forkAtHeight6 = genChain(4, mainChain.take(6).last).tail
    val forkAtHeight8 = genChain(3, mainChain.take(8).last).tail
    
    // Apply forks in order - each should trigger chain reorganization
    history = applyChain(history, forkAtHeight3)
    history.bestFullBlockIdOpt.get shouldEqual forkAtHeight3.last.id
    
    history = applyChain(history, forkAtHeight6)
    history.bestFullBlockIdOpt.get shouldEqual forkAtHeight6.last.id
    
    history = applyChain(history, forkAtHeight8)
    history.bestFullBlockIdOpt.get shouldEqual forkAtHeight8.last.id
    
    // Verify that all blocks from forks are properly linked
    forkAtHeight3.forall { block =>
      history.contains(block.id)
    } shouldBe true
    
    forkAtHeight6.forall { block =>
      history.contains(block.id)
    } shouldBe true
    
    forkAtHeight8.forall { block =>
      history.contains(block.id)
    } shouldBe true
    
    // Create a new fork that extends from an old fork
    val forkFromOldFork = genChain(2, forkAtHeight3.last).tail
    
    // This should be applicable as it extends an existing fork
    history.applicable(forkFromOldFork.head.header) shouldBe true
    
    // Apply the fork from old fork
    history = applyChain(history, forkFromOldFork)
    
    // The new fork should become best chain if it has higher cumulative difficulty
    // (in this test setup, longer chains typically have higher difficulty)
    history.bestFullBlockIdOpt.get shouldEqual forkFromOldFork.last.id
  }

  property("edge cases for chain linking - testing isLinkable robustness") {
    var history = genHistory()
    
    // Test 1: Single block chain (genesis extension)
    val singleBlock = genChain(1, history).head
    // Apply headers first
    history = applyHeaderChain(history, HeaderChain(Seq(singleBlock.header)))
    // Then apply full blocks
    history = applyChain(history, Seq(singleBlock))
    
    // Update best full block to allow applying new blocks
    if (!history.isHeadersChainSynced) {
      history.updateBestFullBlock(singleBlock.header)
    }
    
    // Verify genesis block is properly linked
    history.bestFullBlockIdOpt.get shouldEqual singleBlock.id
    
    // Test 2: Very short fork
    val shortChain = genChain(3, history)
    history = applyChain(history, shortChain)
    
    val veryShortFork = genChain(1, shortChain.take(1).last).tail
    history = applyChain(history, veryShortFork)
    
    // Main chain should remain best (short fork has lower cumulative difficulty)
    history.bestFullBlockIdOpt.get shouldEqual shortChain.last.id
    
    // Test 3: Blocks with same parent (competing blocks)
    val parentBlock = shortChain.last
    val competingBlock1 = genChain(1, parentBlock).head
    val competingBlock2 = genChain(1, parentBlock).head
    
    // Both competing blocks should be applicable
    history.applicable(competingBlock1.header) shouldBe true
    history.applicable(competingBlock2.header) shouldBe true
    
    // Apply first competing block
    history = applyChain(history, Seq(competingBlock1))
    
    // Second competing block should still be applicable (but will create a fork)
    history.applicable(competingBlock2.header) shouldBe true
    
    // Test 4: Chain with gaps (simulating partial synchronization)
    val gapChain = genChain(5)
    
    // Only apply some blocks from the chain
    history = applyChain(history, Seq(gapChain.head, gapChain.last))
    
    // Middle blocks should still be applicable (they can be linked through headers)
    gapChain.drop(1).dropRight(1).forall { block =>
      history.applicable(block.header)
    } shouldBe true
  }

}
