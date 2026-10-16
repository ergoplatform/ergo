package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoCorePropertyTest, NoShrink}

import scala.util.Random

/**
  * Two chains with the same score are equal for the node. If the best header chain has no complete blocks, and a
  * tying chain has them, the best header moves to the tying chain, so the best header and the best full block
  * stay on one fork and the full chain never waits for a body that may not come.
  */
class BestHeaderTieSpecification extends ErgoCorePropertyTest with NoShrink {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  /** History with two full blocks, the second one is returned. */
  private def twoBlockHistory(): (ErgoHistory, ErgoFullBlock) = {
    var history = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, BlocksToKeep)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true
    val commonChain = genChain(2, history)
    history = applyChain(history, commonChain)
    (history, commonChain.last)
  }

  /** Blocks with the same parent, so with the same score. The sleep makes their headers differ. */
  private def siblings(count: Int, parent: ErgoFullBlock): Seq[ErgoFullBlock] =
    (1 to count).map { _ =>
      Thread.sleep(2)
      genChain(1, parent).tail.head
    }

  private def bestChainIds(history: ErgoHistory, heights: Range): Seq[String] =
    heights.map(h => history.bestHeaderIdAtHeight(h).get)

  property("a full chain that ties the best header chain takes the best header") {
    var (history, ancestor) = twoBlockHistory()
    val Seq(a3, b3) = siblings(2, ancestor)

    history = applyHeaderChain(history, HeaderChain(Seq(a3.header)))
    history.bestHeaderOpt.get shouldBe a3.header
    history.bestFullBlockOpt.get.header shouldBe ancestor.header

    history = applyBlock(history, b3)

    history.bestHeaderOpt.get shouldBe b3.header
    history.bestFullBlockOpt.get.header shouldBe b3.header
    history.headersHeight shouldBe 3
    history.bestHeaderIdAtHeight(3) shouldBe Some(b3.header.id)
    history.headerIdsAtHeight(3) shouldBe Seq(b3.header.id, a3.header.id)
    history.isInBestChain(b3.header) shouldBe true
    history.isInBestChain(a3.header) shouldBe false
  }

  property("the first sibling with a body is applied, a header without a body does not hold the full chain") {
    var (history, ancestor) = twoBlockHistory()
    val withheld +: honest = siblings(4, ancestor)

    history = applyHeaderChain(history, HeaderChain(Seq(withheld.header)))
    honest.foreach(b => history = applyBlock(history, b))

    history.fullBlockHeight shouldBe ancestor.header.height + 1
    history.bestFullBlockOpt.get.header shouldBe honest.head.header
    history.bestHeaderOpt.get shouldBe honest.head.header
    // the other tying blocks are stored, not applied
    honest.tail.foreach(b => history.bestHeaderOpt.get should not be b.header)
  }

  property("the full chain follows a block built on the chain that took the best header") {
    var (history, ancestor) = twoBlockHistory()
    val Seq(a3, b3) = siblings(2, ancestor)
    val b4 = genChain(1, b3).tail.head

    history = applyHeaderChain(history, HeaderChain(Seq(a3.header)))
    history = applyBlock(history, b3)
    history.bestHeaderOpt.get shouldBe b3.header
    history = applyBlock(history, b4)

    history.bestHeaderOpt.get shouldBe b4.header
    history.bestFullBlockOpt.get.header shouldBe b4.header
    history.isInBestChain(b3.header) shouldBe true
  }

  property("a heavier chain still wins after the best header moved to a tying chain") {
    var (history, ancestor) = twoBlockHistory()
    val Seq(a3, b3) = siblings(2, ancestor)
    val a4 = genChain(1, a3).tail.head

    history = applyHeaderChain(history, HeaderChain(Seq(a3.header)))
    history = applyBlock(history, b3)
    history.bestHeaderOpt.get shouldBe b3.header

    // the body of the displaced header arrives late: same score, nothing changes
    history = applyBlock(history, a3)
    history.bestHeaderOpt.get shouldBe b3.header
    history.bestFullBlockOpt.get.header shouldBe b3.header

    // a block on top of the displaced header is heavier
    history = applyBlock(history, a4)
    history.bestHeaderOpt.get shouldBe a4.header
    history.bestFullBlockOpt.get.header shouldBe a4.header
    history.headerIdsAtHeight(3) shouldBe Seq(a3.header.id, b3.header.id)
    history.isInBestChain(a3.header) shouldBe true
    history.isInBestChain(b3.header) shouldBe false
  }

  property("a tie of forks with several blocks moves the whole best header chain") {
    var (history, ancestor) = twoBlockHistory()
    val forkA = genChain(3, ancestor).tail
    Thread.sleep(2)
    val forkB = genChain(3, ancestor).tail
    forkB.head.header.id should not be forkA.head.header.id

    history = applyHeaderChain(history, HeaderChain(forkA.map(_.header)))
    history.bestHeaderOpt.get shouldBe forkA.last.header

    // a lighter full chain on fork B follows the data and does not move the best header
    history = applyChain(history, forkB.take(2))
    history.bestFullBlockOpt.get.header shouldBe forkB(1).header
    history.bestHeaderOpt.get shouldBe forkA.last.header

    // the last block of fork B ties the best header chain
    history = applyBlock(history, forkB(2))
    history.bestFullBlockOpt.get.header shouldBe forkB.last.header
    history.bestHeaderOpt.get shouldBe forkB.last.header
    bestChainIds(history, 3 to 5) shouldBe forkB.map(_.header.id)
    forkB.foreach(b => history.isInBestChain(b.header) shouldBe true)
    forkA.foreach(b => history.isInBestChain(b.header) shouldBe false)
    history.headerIdsAtHeight(5) shouldBe Seq(forkB.last.header.id, forkA.last.header.id)
  }

  property("a lighter full chain does not move the best header") {
    var (history, ancestor) = twoBlockHistory()
    val Seq(a3, b3) = siblings(2, ancestor)
    val a4 = genChain(1, a3).tail.head

    history = applyHeaderChain(history, HeaderChain(Seq(a3.header, a4.header)))
    history = applyBlock(history, b3)

    // the heavier header chain stays the best, the full chain follows the data
    history.bestHeaderOpt.get shouldBe a4.header
    history.bestFullBlockOpt.get.header shouldBe b3.header

    // when the bodies of the heavier chain come, the full chain moves to it
    history = applyChain(history, Seq(a3, a4))
    history.bestHeaderOpt.get shouldBe a4.header
    history.bestFullBlockOpt.get.header shouldBe a4.header
  }

  /**
    * Delivers headers and block sections of the given forks one by one, in the random order set by `seed`.
    * A header is delivered after the header of its parent, sections are delivered after their header.
    */
  private def deliverInRandomOrder(historyIn: ErgoHistory,
                                   forks: Seq[Seq[ErgoFullBlock]],
                                   seed: Int)(afterStep: ErgoHistory => Unit): ErgoHistory = {
    val rnd = new Random(seed)
    var history = historyIn
    var pending = for (f <- forks.indices; i <- forks(f).indices; isHeader <- Seq(true, false)) yield (f, i, isHeader)
    var headersDelivered = Set.empty[(Int, Int)]
    while (pending.nonEmpty) {
      val ready = pending.filter { case (f, i, isHeader) =>
        if (isHeader) i == 0 || headersDelivered((f, i - 1)) else headersDelivered((f, i))
      }
      val step @ (f, i, isHeader) = ready(rnd.nextInt(ready.size))
      pending = pending.filterNot(_ == step)
      val block = forks(f)(i)
      if (isHeader) {
        history = history.append(block.header).get._1
        headersDelivered += ((f, i))
      } else {
        block.blockSections.foreach { section =>
          if (!history.contains(section)) history = history.append(section).get._1
        }
      }
      afterStep(history)
    }
    history
  }

  property("the best full block is the best header whenever their heights are equal, for any arrival order") {
    (1 to 30).foreach { seed =>
      var (history, ancestor) = twoBlockHistory()

      // two forks of the same length, so that they have the same score
      val forkA = genChain(3, ancestor).tail
      Thread.sleep(2)
      val forkB = genChain(3, ancestor).tail
      forkB.head.header.id should not be forkA.head.header.id

      val finalHistory = deliverInRandomOrder(history, Seq(forkA, forkB), seed) { h =>
        if (h.fullBlockHeight == h.headersHeight) {
          withClue(s"seed $seed: ") {
            h.bestFullBlockOpt.get.header shouldBe h.bestHeaderOpt.get
          }
        }
      }

      // all headers and sections are known now
      finalHistory.fullBlockHeight shouldBe 5
      finalHistory.headersHeight shouldBe 5
      finalHistory.bestFullBlockOpt.get.header shouldBe finalHistory.bestHeaderOpt.get
      // the ids of the best chain at every height are the ids of its headers
      val bestChain = finalHistory.headerChainBack(5, finalHistory.bestHeaderOpt.get, _.height == 3).headers
      bestChain.foreach(h => finalHistory.bestHeaderIdAtHeight(h.height) shouldBe Some(h.id))
    }
  }

}
