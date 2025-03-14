package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.subblocks.InputBlockInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import scorex.util.{bytesToId, idToBytes}

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

    h.bestInputBlocksChain() shouldBe Seq()
    h.applyInputBlockTransactions(ib.id, Seq.empty) shouldBe Seq(ib.id)
    h.bestInputBlocksChain() shouldBe Seq(ib.id)
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
    h.getInputBlock(ib1.id) shouldBe Some(ib1)
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib1.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 1
    h.isAncestor(ib1.id, ib1.id).isEmpty shouldBe true

    val c3 = genChain(height = 2, history = h).tail
    c3.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id
    
    val ib2 = InputBlockInfo(1, c3(0).header, Some(idToBytes(ib1.id)), transactionsDigest = null, merkleProof = null)
    val r = h.applyInputBlock(ib2)
    r shouldBe None
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib2.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 2
    h.isAncestor(ib2.id, ib1.id).contains(ib2.id) shouldBe true
    h.isAncestor(ib2.id, ib2.id).isEmpty shouldBe true
    h.isAncestor(ib1.id, ib2.id).isEmpty shouldBe true

    // apply transactions
    // out-of-order application
    h.applyInputBlockTransactions(ib2.id, Seq.empty) shouldBe Seq()
    h.bestInputBlocksChain() shouldBe Seq()
    h.applyInputBlockTransactions(ib1.id, Seq.empty) shouldBe Seq(ib1.id, ib2.id)
    h.bestInputBlocksChain() shouldBe Seq(ib2.id, ib1.id)
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
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id) shouldBe None
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id) shouldBe None
    h.isAncestor(childIb.id, parentIb.id).isEmpty shouldBe true
    h.disconnectedWaitlist shouldBe Set(childIb)
    h.deliveryWaitlist shouldBe Set(bytesToId(childIb.prevInputBlockId.get))

    h.applyInputBlockTransactions(childIb.id, Seq.empty) shouldBe Seq()
    h.bestInputBlock() shouldBe None

    // Now apply parent
    val r2 = h.applyInputBlock(parentIb)
    r2 shouldBe None
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get shouldBe Set(childIb.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 2
    h.isAncestor(childIb.id, parentIb.id).contains(childIb.id) shouldBe true
    h.isAncestor(childIb.id, childIb.id).isEmpty shouldBe true
    h.isAncestor(parentIb.id, childIb.id).isEmpty shouldBe true

    h.applyInputBlockTransactions(parentIb.id, Seq.empty) shouldBe Seq(parentIb.id, childIb.id)
    h.bestInputBlock().get shouldBe childIb

    h.bestInputBlocksChain() shouldBe Seq(childIb.id, parentIb.id)
    h.inputBlocksChain(childIb.id) shouldBe Seq(childIb.id, parentIb.id)
  }

  property("input block - fork switching - disjoint forks") {
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
    h.getInputBlock(ib1.id) shouldBe Some(ib1)
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib1.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 1
    h.isAncestor(ib1.id, ib1.id).isEmpty shouldBe true

    h.applyInputBlockTransactions(ib1.id, Seq.empty) shouldBe Seq(ib1.id)

    val c3 = genChain(height = 2, history = h).tail
    c3.head.header.parentId shouldBe h.bestHeaderOpt.get.id

    val c4 = genChain(height = 2, history = h).tail
    c4.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 1

    val ib2 = InputBlockInfo(1, c3(0).header, None, transactionsDigest = null, merkleProof = null)
    val ib3 = InputBlockInfo(1, c4(0).header, Some(idToBytes(ib2.id)), transactionsDigest = null, merkleProof = null)
    h.applyInputBlock(ib2)
    val r = h.applyInputBlock(ib3)
    r shouldBe None
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib3.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 2
    h.isAncestor(ib2.id, ib1.id).isEmpty shouldBe true
    h.isAncestor(ib3.id, ib2.id).contains(ib3.id) shouldBe true
    h.isAncestor(ib1.id, ib2.id).isEmpty shouldBe true

    // apply transactions
    // todo: test out-of-order application, currently failing but maybe it is ok?
    h.applyInputBlockTransactions(ib2.id, Seq.empty) shouldBe Seq()
    h.applyInputBlockTransactions(ib3.id, Seq.empty) shouldBe Seq(ib2.id, ib3.id)

    h.bestInputBlocksChain() shouldBe Seq(ib3.id, ib2.id)
  }

  property("input block - fork switching - common root") {
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    val c1 = genChain(height = 2, history = h).toList
    applyChain(h, c1)

    val c2 = genChain(2, h).tail
    c2.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id

    val c3 = genChain(2, h).tail
    c3.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id

    val ib1 = InputBlockInfo(1, c2(0).header, None, transactionsDigest = null, merkleProof = null)
    val r1 = h.applyInputBlock(ib1)
    r1 shouldBe None
    h.getInputBlock(ib1.id) shouldBe Some(ib1)
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib1.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 1
    h.isAncestor(ib1.id, ib1.id).isEmpty shouldBe true

    h.applyInputBlockTransactions(ib1.id, Seq.empty) shouldBe Seq(ib1.id)


    val ib2 = InputBlockInfo(1, c3(0).header, Some(idToBytes(ib1.id)), transactionsDigest = null, merkleProof = null)
    val r2 = h.applyInputBlock(ib2)
    r2 shouldBe None
    h.applyInputBlockTransactions(ib2.id, Seq.empty) shouldBe Seq(ib2.id)
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib2.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 2

    val c4 = genChain(height = 2, history = h).tail
    c4.head.header.parentId shouldBe h.bestHeaderOpt.get.id

    val c5 = genChain(height = 2, history = h).tail
    c5.head.header.parentId shouldBe h.bestHeaderOpt.get.id
    h.bestFullBlockOpt.get.id shouldBe c1.last.id

    val ib3 = InputBlockInfo(1, c4(0).header, Some(idToBytes(ib1.id)), transactionsDigest = null, merkleProof = null)
    val r = h.applyInputBlock(ib3)
    r shouldBe None
    // both tips of depth == 2 are recognized now
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib2.id)
    h.getOrderingBlockTips(h.bestHeaderOpt.get.id).get should contain(ib3.id)
    h.getOrderingBlockTipHeight(h.bestHeaderOpt.get.id).get shouldBe 2

    // apply transactions
    // todo: test out-of-order application, currently failing but maybe it is ok?
    h.applyInputBlockTransactions(ib3.id, Seq.empty) shouldBe Seq()

    val ib4 = InputBlockInfo(1, c5(0).header, Some(idToBytes(ib3.id)), transactionsDigest = null, merkleProof = null)
    val r4 = h.applyInputBlock(ib4)
    r4 shouldBe None
    h.applyInputBlockTransactions(ib4.id, Seq.empty) shouldBe Seq(ib3.id, ib4.id)

    h.bestInputBlocksChain() shouldBe Seq(ib4.id, ib3.id, ib1.id)
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
