package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoCorePropertyTest, NoShrink}
import org.ergoplatform.validation.{MalformedModifierError, RecoverableModifierError}

/**
  * A pruning node that has been offline longer than its `blocksToKeep` window must still be able to
  * catch up. While the node is offline the network tip advances past it by more than `blocksToKeep`.
  * On restart the headers re-sync and `minimalFullBlockHeight` jumps to roughly `tip - blocksToKeep + 1`,
  * which is now far above the node's local `bestFullBlockHeight`. To advance its state the node has to
  * apply the block sections of the intermediate heights.
  *
  * These intermediate sections sit below `minimalFullBlockHeight`, so the `bsTooOld` rule rejects them.
  * If that rejection is fatal the sections are marked permanently invalid and the node can never recover:
  *
  *   MalformedModifierError: Block section should correspond to a block header that is not pruned yet.
  *
  * This spec pins down the two properties that keep such a node unstuck: a block that is a successor of
  * the best full block stays applicable even below the window, and a genuinely too-old section is only
  * rejected recoverably.
  */
class PrunedHistoryCatchUpSpec extends ErgoCorePropertyTest with NoShrink {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private def prunedDigestHistory(): ErgoHistory =
    generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep)

  property("a catch-up block above the jumped minimalFullBlockHeight stays applicable") {
    val chainLen = 8
    val gap = 5

    // local chain: apply all but the last block, so bestFullBlock is one below the tip we still need
    val history0 = prunedDigestHistory()
    val chain = genChain(chainLen, history0)
    val history = applyChain(history0, chain.init)
    val nextBlock = chain.last // the very next block the node must apply to advance its state
    history.append(nextBlock.header).get._1

    history.bestFullBlockOpt.get.header.height shouldBe nextBlock.header.height - 1

    // simulate the long shutdown: the network tip moved far past the keep window, so on re-sync
    // minimalFullBlockHeight is now well above every block we still hold locally
    history.writeMinimalFullBlockHeight(nextBlock.header.height + gap)
    history.isHeadersChainSyncedVar = true

    // the next-needed block sits below minimalFullBlockHeight but is a successor of bestFullBlock,
    // so it must remain applicable - otherwise the pruning node can never catch up
    history.applicableTry(nextBlock.blockTransactions) shouldBe 'success
  }

  property("a genuinely too-old block section is rejected recoverably, not fatally") {
    val chainLen = 8
    val gap = 5

    // fresh pruning node (no full block applied yet): only the headers chain is known
    val history = prunedDigestHistory()
    val chain = genChain(chainLen, history)
    chain.foldLeft(history)((h, b) => h.append(b.header).get._1)

    history.writeMinimalFullBlockHeight(chain.last.header.height + gap)
    history.isHeadersChainSyncedVar = true

    val tooOld = chain.head // far below the keep window and not a successor of any best full block
    val result = history.applicableTry(tooOld.blockTransactions)

    result shouldBe 'failure
    // "too old" is a transient condition relative to sync progress, never an intrinsic defect:
    // the section must not be permanently blacklisted, or the node can never recover.
    result.failed.get should not be a[MalformedModifierError]
    result.failed.get shouldBe a[RecoverableModifierError]
  }

}
