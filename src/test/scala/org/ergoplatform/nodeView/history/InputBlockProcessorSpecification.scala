package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.subblocks.InputBlockInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import scorex.util.idToBytes

class InputBlockProcessorSpecification extends ErgoCorePropertyTest {

  property("apply first input block after ordering block") {
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    val c1 = genChain(2, h)
    applyChain(h, c1)
    h.bestFullBlockOpt.get.id shouldBe c1.last.id
    
    val c2 = genChain(2, h).tail
    val ib = InputBlockInfo(1, c2(0).header, None, transactionsDigest = null, merkleProof = null)
    val r = h.applyInputBlock(ib)
    r shouldBe None
  }

  property("apply child input block of best input block") {
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    val c1 = genChain(height = 2, history = h).toList
    applyChain(h, c1)
    
    val c2 = genChain(2, h).tail
    c2.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id

    val ib1 = InputBlockInfo(1, c2(0).header, None, transactionsDigest = null, merkleProof = null)
    val r1 = h.applyInputBlock(ib1)
    r1 shouldBe None

    val c3 = genChain(height = 2, history = h).tail
    c3.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id
    
    val ib2 = InputBlockInfo(1, c3(0).header, Some(idToBytes(ib1.id)), transactionsDigest = null, merkleProof = null)
    val r = h.applyInputBlock(ib2)
    r shouldBe None
  }

  property("apply input block with parent input block not available (out of order application)") {
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    val c1 = genChain(height = 2, history = h).toList
    applyChain(h, c1)
    
    val c2 = genChain(2, h).tail
    c2.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id

    // Generate parent and child input blocks
    val parentIb = InputBlockInfo(1, c2(0).header, None, transactionsDigest = null, merkleProof = null)
    val c3 = genChain(2, h).tail
    val childIb = InputBlockInfo(1, c3(0).header, Some(idToBytes(parentIb.id)), transactionsDigest = null, merkleProof = null)

    // Apply child first - should return parent id as needed
    val r1 = h.applyInputBlock(childIb)
    r1 shouldBe Some(parentIb.id)

    // Now apply parent
    val r2 = h.applyInputBlock(parentIb)
    r2 shouldBe None

    // Apply child again - should now succeed as parent is available
    val r3 = h.applyInputBlock(childIb)
    r3 shouldBe None
  }

  property("apply input block with parent ordering block not available") {

  }

  property("apply input block with parent ordering block in the past") {

  }

  property("apply input block with non-best parent input block") {

  }

  property("apply new best input block (input blocks chain switch) - same ordering block") {

  }

  property("apply new best input block on another ordering block") {

  }

}
