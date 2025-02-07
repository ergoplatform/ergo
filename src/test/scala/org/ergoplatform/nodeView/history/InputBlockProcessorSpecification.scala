package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.subblocks.InputBlockInfo
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}

class InputBlockProcessorSpecification extends ErgoCorePropertyTest {

  property("apply first input block after ordering block") {
    val h = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, blocksToKeep = -1,
      epochLength = 10000, useLastEpochs = 3, initialDiffOpt = None, None)
    val l = 3
    val c = genChain(l, h)
    applyChain(h, c.dropRight(1))

    val ib = InputBlockInfo(1, c(2).header, None, transactionsDigest = null, merkleProof = null)
    val r = h.applyInputBlock(ib)
    r should be (true -> None)
  }

  property("apply child input block of best input block") {

  }

  property("apply input block with parent input block not available (out of order application)") {

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
