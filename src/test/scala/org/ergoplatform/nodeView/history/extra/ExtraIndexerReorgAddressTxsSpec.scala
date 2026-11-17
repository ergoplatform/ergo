package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.txSegmentId
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper, untilTimeout}
import scorex.util.{ModifierId, bytesToId}
import sigma.ast.{ErgoTree, GE, Height, IntConstant}
import sigma.interpreter.ProverResult

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.{Success, Try}

/**
  * After a chain switch an address lists the transactions of the applied chain which create or
  * spend a box of it, and no other.
  *
  * The rollback of an abandoned block gives a spent box back to its address. The number of the
  * spending transaction has to leave the transaction list of that address too, also when the
  * address has no output in the abandoned blocks: the number goes to the next transaction that
  * is indexed. These tests run the production indexer against a real node view holder (history
  * and UTXO state, transactions verified), hand the node valid blocks only, let it switch to a
  * better branch and compare the transaction list of every address with the applied chain.
  */
class ExtraIndexerReorgAddressTxsSpec extends ErgoCorePropertyTest with NodeViewTestOps {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private val settleTimeout = 15.seconds

  private val indexedSettings: ErgoSettings = {
    val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true))
  }

  /** Transactions of a block, given the state before it. */
  private type BlockTxs = WrappedUtxoState => Seq[ErgoTransaction]

  /** A script which is true with an empty proof and is not the script the block generators pay to. */
  private val watchedTree: ErgoTree = ErgoTree.fromProposition(GE(Height, IntConstant(0)).toSigmaProp)

  /** The address the tests pay to and spend from themselves. */
  private val watched: ModifierId = hashErgoTree(watchedTree)

  /** Moves a box whose script is true with an empty proof to `tree`, in a transaction of its own. */
  private def move(box: ErgoBox, tree: ErgoTree): ErgoTransaction =
    ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)), IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(box.value, tree, box.creationHeight, box.additionalTokens)))

  /** Spends a box of the watched address without an output for that address. */
  private def spend(box: ErgoBox): ErgoTransaction = move(box, Constants.TrueTree)

  /** Transactions of `chain` in the order the indexer numbers them, with the height of their block. */
  private def numbered(chain: Seq[ErgoFullBlock]): Seq[(ErgoTransaction, Int)] =
    chain.flatMap(block => block.blockTransactions.txs.map(_ -> block.header.height))

  /**
    * Transaction numbers every address of `chain` has to list: those of the transactions which
    * create a box of the address or spend one. The indexer does not process the inputs of the
    * block at height 1.
    */
  private def expectedAddressTxs(chain: Seq[ErgoFullBlock]): Map[ModifierId, Seq[Long]] = {
    val txs = numbered(chain)
    val created: Map[ModifierId, ErgoTree] = txs.flatMap(_._1.outputs).map(box => bytesToId(box.id) -> box.ergoTree).toMap
    txs.zipWithIndex.flatMap { case ((tx, height), number) =>
      val spent = if (height > 1) tx.inputs.flatMap(input => created.get(bytesToId(input.boxId))) else Seq.empty
      (spent ++ tx.outputs.map(_.ergoTree)).map(hashErgoTree).distinct.map(_ -> number.toLong)
    }.groupBy(_._1).map { case (address, entries) => address -> entries.map(_._2) }
  }

  /** Transaction numbers an address row lists, in its segments and in the row. */
  private def listedTxs(history: ErgoHistory, row: IndexedErgoAddress): Seq[Long] =
    (0 until row.txSegmentCount).flatMap { n =>
      history.typedExtraIndexById[IndexedErgoAddress](txSegmentId(row.id, n)).toSeq.flatMap(_.txs)
    } ++ row.txs

  /**
    * Compares what the addresses of `chain` list with the chain: the transaction numbers of every
    * address row, and the transactions read through the row, newest first as the API serves them.
    *
    * @return the differences found, empty if every address lists exactly its transactions
    */
  private def addressTxDifferences(history: ErgoHistory, chain: Seq[ErgoFullBlock])
                                  (implicit segmentThreshold: Int): Seq[String] = {
    val txIds = numbered(chain).map(_._1.id).toIndexedSeq
    def show(numbers: Seq[Long]): String = numbers.mkString("[", ",", "]")
    expectedAddressTxs(chain).toSeq.flatMap { case (address, expected) =>
      val row = history.typedExtraIndexById[IndexedErgoAddress](address)
      val listed = row.map(listedTxs(history, _))
      val read = row.map(r => Try(r.retrieveTxs(history, 0, Int.MaxValue).map(_.id).toSeq))
      if (listed.contains(expected) && read.contains(Success(expected.reverse.map(n => txIds(n.toInt))))) None
      else Some(s"address $address lists transactions ${listed.map(show)}, on the applied chain its transactions are ${show(expected)}; " +
        s"reading the listed transactions gives ${read.map(_.map(_.map(id => txIds.indexOf(id))))}")
    }
  }

  /**
    * A real node view holder (history and UTXO state) with four applied blocks and the production
    * indexer caught up with them.
    *
    * @param segmentThresholdOpt - if given, the indexer splits rows into segments of that size
    *                              instead of 512; nothing else differs from the production class
    */
  private class IndexedNode(fixture: NodeViewFixture, segmentThresholdOpt: Option[Int]) {

    import fixture._

    implicit val segmentThreshold: Int = segmentThresholdOpt.getOrElse(512)

    /** Blocks the state has applied. */
    val chain: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]

    /** Blocks `stateCopy` has applied: a part of `chain`, then possibly blocks of a branch being built. */
    private val copyPath: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]

    /** Copy of the state to build blocks with. All its versions share one store. */
    private var stateCopy: WrappedUtxoState = {
      val (genesisState, boxHolder) = createUtxoState(settings)
      val genesis = validFullBlock(None, genesisState, boxHolder)
      val afterGenesis = WrappedUtxoState(genesisState, boxHolder, settings).applyModifier(genesis)(_ => ()).get
      applyBlock(genesis) shouldBe 'success
      chain += genesis
      copyPath += genesis
      afterGenesis
    }

    private var branchesBuilt: Int = 0
    (1 to 3).foreach(_ => extend())

    private var indexer: ActorRef = startIndexer()
    indexDifferences() shouldBe empty

    private def startIndexer(): ActorRef = actorSystem.actorOf(segmentThresholdOpt match {
      case Some(threshold) =>
        Props(new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true) {
          override implicit val segmentThreshold: Int = threshold
        })
      case None =>
        Props(new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true))
    })

    private def stopIndexer(): Unit = {
      val watcher = TestProbe()
      watcher.watch(indexer)
      actorSystem.stop(indexer)
      watcher.expectTerminated(indexer)
    }

    def close(): Unit = {
      actorSystem.stop(indexer)
      stateCopy.closeStorage()
    }

    /** Stops the indexer and the node view holder and starts both again on the same database. */
    def restart(): Unit = {
      stopIndexer()
      stopNodeViewHolder()
      startNodeViewHolder()
      indexer = startIndexer()
    }

    def history: ErgoHistory = getHistory

    def checkpoint: IndexerState = IndexerState.fromHistory(getHistory)

    /** Number of transactions on the applied chain, which is the number the next transaction gets. */
    def txCount: Long = chain.map(_.blockTransactions.txs.size).sum.toLong

    /** Takes the state copy to the end of `path`, blocks on top of each other from height 1. */
    private def stateAfter(path: Seq[ErgoFullBlock]): WrappedUtxoState = {
      val common = copyPath.zip(path).takeWhile { case (applied, wanted) => applied.id == wanted.id }.size
      if (common < copyPath.size) {
        stateCopy = stateCopy.rollbackTo(idToVersion(copyPath(common - 1).id)).get
        copyPath.remove(common, copyPath.size - common)
      }
      path.drop(common).foreach { block =>
        stateCopy = stateCopy.applyModifier(block)(_ => ()).get
        copyPath += block
      }
      stateCopy
    }

    /**
      * Generated valid transactions for a block on top of `state` which neither spend nor read a
      * box of the watched address, nor depend on a transaction which does: only the transactions
      * a test builds itself touch that address. The first of them is the reward transaction.
      *
      * @param atLeast - number of transactions wanted at least
      */
    def ordinaryTxs(state: WrappedUtxoState, atLeast: Int = 1): Seq[ErgoTransaction] = Iterator.continually {
      val avoided = mutable.Set.empty[ModifierId]
      avoided ++= state.versionedBoxHolder.boxes.values.filter(_.ergoTree == watchedTree).map(box => bytesToId(box.id))
      validTransactionsFromBoxHolder(state.versionedBoxHolder, new RandomWrapper)._1.filter { tx =>
        val avoid = (tx.inputs.map(_.boxId) ++ tx.dataInputs.map(_.boxId)).exists(id => avoided(bytesToId(id)))
        if (avoid) avoided ++= tx.outputs.map(out => bytesToId(out.id))
        !avoid
      }
    }.take(100).find(_.size >= atLeast).get

    /** A block of generated transactions, see [[ordinaryTxs]]. */
    def ordinary: BlockTxs = ordinaryTxs(_)

    /**
      * Valid blocks on top of each other, the first on top of `parent`, a block of `chain`.
      *
      * @param blocks - the transactions of each block
      */
    def branch(parent: ErgoFullBlock)(blocks: BlockTxs*): Seq[ErgoFullBlock] = {
      val kept = chain.take(chain.indexWhere(_.id == parent.id) + 1).toList
      branchesBuilt += 1
      blocks.foldLeft(Seq.empty[ErgoFullBlock]) { (built, txs) =>
        val state = stateAfter(kept ++ built)
        // every branch has another timestamp, so that competing blocks differ whatever they hold
        val time = if (built.isEmpty) Some(parent.header.timestamp + branchesBuilt) else None
        built :+ validFullBlock(Some(built.lastOption.getOrElse(parent)), state, txs(state), time)
      }
    }

    /**
      * Hands the node blocks built by [[branch]], each header first, then its sections, and waits
      * until the state is at the last of them: the chain got longer, or the node has switched to
      * a better branch.
      */
    def deliver(blocks: Seq[ErgoFullBlock]): Unit = {
      blocks.foreach(block => applyBlock(block) shouldBe 'success)
      untilTimeout(settleTimeout, 20.millis) {
        getHistory.bestFullBlockIdOpt shouldBe Some(blocks.last.id)
        getCurrentState.version shouldBe idToVersion(blocks.last.id)
      }
      val kept = chain.indexWhere(_.id == blocks.head.header.parentId) + 1
      chain.remove(kept, chain.size - kept)
      chain ++= blocks
    }

    /** One more block with generated transactions. */
    def extend(): Unit = deliver(branch(chain.last)(ordinary))

    /**
      * Extends the chain until `count` boxes are paid to the watched address, each by a
      * transaction of its own which has no input of that address.
      *
      * @return the boxes
      */
    def fund(count: Int): Seq[ErgoBox] = {
      val funded = ArrayBuffer.empty[ErgoBox]
      (1 to 20).iterator.takeWhile(_ => funded.size < count).foreach { _ =>
        deliver(branch(chain.last) { state =>
          val txs = ordinaryTxs(state)
          val spentInBlock = txs.flatMap(_.inputs).map(input => bytesToId(input.boxId)).toSet
          // outputs the generated transactions leave unspent; anyone can spend them
          val spare = txs.flatMap(_.outputs).filter(out => out.ergoTree == Constants.TrueTree && !spentInBlock(bytesToId(out.id)))
          val payments = spare.take(count - funded.size).map(move(_, watchedTree))
          funded ++= payments.map(_.outputs.head)
          txs ++ payments
        })
      }
      funded.size shouldBe count
      funded.toList
    }

    def watchedRow: IndexedErgoAddress = getHistory.typedExtraIndexById[IndexedErgoAddress](watched).get

    /** Transaction numbers of the watched address, parsed from the bytes of its row in the database. */
    def storedWatchedTxs: Seq[Long] = {
      val bytes = getHistory.historyStorage.modifierBytesById(watched).get
      ExtraIndexSerializer.parseBytes(bytes).asInstanceOf[IndexedErgoAddress].txs.toList
    }

    /** Transaction numbers the watched address has to list, see [[expectedAddressTxs]]. */
    def expectedWatchedTxs: Seq[Long] = expectedAddressTxs(chain).getOrElse(watched, Seq.empty)

    /** Number of a box in the index. */
    def boxNumber(box: ErgoBox): Long = getHistory.typedExtraIndexById[IndexedErgoBox](bytesToId(box.id)).get.globalIndex

    /** Waits until the index is written up to the last block of `chain`. */
    def awaitIndexed(): Unit = untilTimeout(settleTimeout, 20.millis) {
      checkpoint.indexedHeaderId shouldBe Some(chain.last.id)
    }

    /**
      * Differences between the index and `chain`, see [[addressTxDifferences]]. Gives the indexer
      * time to reach the applied tip first; an index which does not get there is a difference.
      */
    def indexDifferences(): Seq[String] = {
      Try(awaitIndexed())
      val reached = checkpoint
      val behind =
        if (reached.indexedHeaderId.contains(chain.last.id) && reached.rollbackTo == 0 && reached.globalTxIndex == txCount) None
        else Some(s"index is at height ${reached.indexedHeight}, block ${reached.indexedHeaderId}, with ${reached.globalTxIndex} transactions, " +
          s"rollback to ${reached.rollbackTo}; the applied chain is at height ${chain.last.header.height}, block ${chain.last.id}, with $txCount")
      behind.toSeq ++ addressTxDifferences(getHistory, chain)
    }
  }

  /**
    * @param segmentThreshold - see [[IndexedNode]]
    */
  private def withNode(segmentThreshold: Option[Int] = None)(test: IndexedNode => Unit): Unit =
    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      val node = new IndexedNode(fixture, segmentThreshold)
      try test(node) finally node.close()
    }

  /**
    * The node applies and indexes `abandoned`, then is handed `better`, a longer branch from the
    * same block of the chain, and switches to it. The index has to be the index of the applied
    * chain before and after.
    */
  private def switchBranch(node: IndexedNode, abandoned: Seq[ErgoFullBlock], better: Seq[ErgoFullBlock]): Unit = {
    abandoned.head.header.parentId shouldBe better.head.header.parentId
    better.size should be > abandoned.size
    node.deliver(abandoned)
    node.indexDifferences() shouldBe empty
    node.deliver(better)
    node.indexDifferences() shouldBe empty
  }

  property("an address with only an input in an abandoned block does not keep the number of that transaction") {
    withNode() { node =>
      val Seq(box) = node.fund(1)
      val fork = node.chain.last
      switchBranch(node,
        abandoned = node.branch(fork)(state => spend(box) +: node.ordinaryTxs(state)),
        better = node.branch(fork)(node.ordinary, node.ordinary))
      node.expectedWatchedTxs.size shouldBe 1

      // the number of the abandoned transaction belongs to another transaction now
      node.extend()
      node.indexDifferences() shouldBe empty
    }
  }

  /**
    * Both branches hold the transaction which spends the box of the watched address, the block
    * of one branch as its first transaction, the block of the other as its last.
    */
  private def sameTransactionOnBothBranches(firstInAbandoned: Boolean): Unit = withNode() { node =>
    val Seq(box) = node.fund(1)
    val tx = spend(box)
    val asFirst: BlockTxs = state => tx +: node.ordinaryTxs(state)
    val asLast: BlockTxs = state => node.ordinaryTxs(state) :+ tx
    val fork = node.chain.last
    switchBranch(node,
      abandoned = node.branch(fork)(if (firstInAbandoned) asFirst else asLast),
      better = node.branch(fork)(if (firstInAbandoned) asLast else asFirst, node.ordinary))
    node.expectedWatchedTxs.size shouldBe 2
  }

  property("a transaction which gets a higher number on the better branch is listed once, by that number") {
    sameTransactionOnBothBranches(firstInAbandoned = true)
  }

  property("a transaction which gets a lower number on the better branch is listed once, by that number") {
    sameTransactionOnBothBranches(firstInAbandoned = false)
  }

  property("the transactions of the address can be read when the better branch has fewer transactions") {
    withNode() { node =>
      import node.segmentThreshold
      val Seq(box) = node.fund(1)
      val fork = node.chain.last
      // the abandoned block ends with the spending transaction; the blocks of the better branch
      // hold the reward transaction only
      val abandoned = node.branch(fork)(state => node.ordinaryTxs(state, atLeast = 2) :+ spend(box))
      val abandonedNumber = node.txCount + abandoned.head.blockTransactions.txs.size - 1
      switchBranch(node,
        abandoned,
        better = node.branch(fork)(node.ordinaryTxs(_).take(1), node.ordinaryTxs(_).take(1)))

      // no transaction has the number of the abandoned transaction
      node.checkpoint.globalTxIndex should be <= abandonedNumber
      node.watchedRow.retrieveTxs(node.history, 0, 10).map(_.id).toSeq shouldBe Seq(box.transactionId)
    }
  }

  property("an address with inputs and an output in an abandoned block gets its boxes back and loses the transactions") {
    withNode() { node =>
      val Seq(box1, box2) = node.fund(2)
      val toItself = move(box2, watchedTree)
      val fork = node.chain.last
      switchBranch(node,
        abandoned = node.branch(fork)(state => Seq(spend(box1), toItself) ++ node.ordinaryTxs(state)),
        better = node.branch(fork)(node.ordinary, node.ordinary))

      val row = node.watchedRow
      row.balanceInfo.map(_.nanoErgs) shouldBe Some(box1.value + box2.value)
      row.boxes.toList shouldBe List(node.boxNumber(box1), node.boxNumber(box2)).sorted
      node.history.typedExtraIndexById[IndexedErgoBox](bytesToId(toItself.outputs.head.id)) shouldBe None

      // both boxes can be spent again
      node.deliver(node.branch(node.chain.last)(state => node.ordinaryTxs(state) ++ Seq(spend(box2), spend(box1))))
      node.indexDifferences() shouldBe empty
      node.watchedRow.balanceInfo.map(_.nanoErgs) shouldBe Some(0L)
    }
  }

  property("an address with inputs in two abandoned blocks does not keep the numbers of those transactions") {
    withNode() { node =>
      val Seq(box1, box2) = node.fund(2)
      val fork = node.chain.last
      switchBranch(node,
        abandoned = node.branch(fork)(state => spend(box1) +: node.ordinaryTxs(state), state => node.ordinaryTxs(state) :+ spend(box2)),
        better = node.branch(fork)(node.ordinary, node.ordinary, node.ordinary))
      node.expectedWatchedTxs.size shouldBe 2
    }
  }

  property("the row of the address in the database has no number of an abandoned transaction, also after a restart") {
    withNode() { node =>
      val Seq(box) = node.fund(1)
      val fork = node.chain.last
      switchBranch(node,
        abandoned = node.branch(fork)(state => spend(box) +: node.ordinaryTxs(state)),
        better = node.branch(fork)(node.ordinary, node.ordinary))
      val expected = node.expectedWatchedTxs
      node.storedWatchedTxs shouldBe expected
      val indexedBefore = node.checkpoint

      // the node starts on the stored index without rebuilding it
      node.restart()
      node.checkpoint shouldBe indexedBefore
      node.storedWatchedTxs shouldBe expected

      node.extend()
      node.indexDifferences() shouldBe empty
      node.storedWatchedTxs shouldBe expected
    }
  }

  property("the rollback takes the numbers of abandoned transactions out of a segment of the address") {
    // the production indexer with segments of 8 numbers
    withNode(segmentThreshold = Some(8)) { node =>
      val boxes = node.fund(6)
      val spends: Seq[ErgoTransaction] = boxes.take(5).map(spend)
      val fork = node.chain.last
      node.indexDifferences() shouldBe empty
      node.watchedRow.txSegmentCount shouldBe 0

      // 6 + 5 numbers: the first 8 are in a segment, two of them of the abandoned block
      val abandoned = node.branch(fork)(state => node.ordinaryTxs(state).take(1) ++ spends)
      node.deliver(abandoned)
      node.indexDifferences() shouldBe empty
      node.watchedRow.txSegmentCount shouldBe 1
      node.watchedRow.txs.size shouldBe 3
      node.history.typedExtraIndexById[IndexedErgoAddress](txSegmentId(watched, 0)).map(_.txs.size) shouldBe Some(8)

      node.deliver(node.branch(fork)(node.ordinary, node.ordinary))
      node.indexDifferences() shouldBe empty
      node.watchedRow.txSegmentCount shouldBe 0
      node.watchedRow.txs.size shouldBe 6
      node.history.typedExtraIndexById[IndexedErgoAddress](txSegmentId(watched, 0)) shouldBe None

      // the segment is written again when the address gets that many transactions once more
      node.deliver(node.branch(node.chain.last)(state => node.ordinaryTxs(state).take(1) ++ spends))
      node.indexDifferences() shouldBe empty
      node.watchedRow.txSegmentCount shouldBe 1
    }
  }

  property("a row written with the number of an abandoned transaction keeps it until a rollback below that number") {
    withNode() { node =>
      import node.segmentThreshold
      val Seq(box) = node.fund(1)
      node.indexDifferences() shouldBe empty
      val forkBelow = node.chain.last

      // The row as a rollback used to leave it when the address had only an input in the
      // abandoned block: one more number, the next one to be assigned. Indexes written by
      // earlier versions can hold such rows; they are not repaired.
      val stale = node.txCount
      val row = node.watchedRow
      row.txs += stale
      node.history.historyStorage.insertExtraTry(Array.empty, Array[ExtraIndex](row)).get
      val funding = node.expectedWatchedTxs
      node.storedWatchedTxs shouldBe funding :+ stale
      // no transaction has the number yet
      Try(node.watchedRow.retrieveTxs(node.history, 0, 10)).isFailure shouldBe true

      // the number goes to a transaction which does not touch the address
      node.extend()
      node.awaitIndexed()
      val forkAbove = node.chain.last
      node.storedWatchedTxs shouldBe funding :+ stale
      node.watchedRow.retrieveTxs(node.history, 0, 10).map(_.id).toSeq shouldBe Seq(forkAbove.blockTransactions.txs.head.id, box.transactionId)

      // a rollback to a block above the number removes the numbers of the abandoned block only
      val tx = spend(box)
      val abandonedNumber = node.txCount
      node.deliver(node.branch(forkAbove)(state => tx +: node.ordinaryTxs(state)))
      node.awaitIndexed()
      node.storedWatchedTxs shouldBe funding ++ Seq(stale, abandonedNumber)
      node.deliver(node.branch(forkAbove)(node.ordinary, state => node.ordinaryTxs(state) :+ tx))
      node.awaitIndexed()
      node.storedWatchedTxs shouldBe funding ++ Seq(stale, node.txCount - 1)

      // a rollback below the number, of a transaction with an input of the address, removes it
      node.deliver(node.branch(forkBelow)(Seq.fill(4)(node.ordinary): _*))
      node.indexDifferences() shouldBe empty
      node.storedWatchedTxs shouldBe funding
    }
  }

  /**
    * The row of the watched address as earlier versions can leave it: the numbers of its two
    * transactions and, between them, a higher number which is not its own (the second transaction
    * was indexed after the rollback which left that number). Then a block with a transaction of
    * the address is abandoned, the fork point being at or above the second number and below the
    * stale one. The rollback has to remove the stale number and the abandoned one, no other.
    *
    * @param alsoPaysAddress - whether the abandoned transaction, which spends a box of the
    *                          address, has an output for it too
    */
  private def rollbackOnRowOutOfOrder(alsoPaysAddress: Boolean): Unit = withNode() { node =>
    val Seq(box, _) = node.fund(2)
    node.indexDifferences() shouldBe empty
    val fork = node.chain.last
    val funding = node.expectedWatchedTxs
    funding.size shouldBe 2
    // the next number to be assigned: above the fork point, which is the last number of the chain
    val stale = node.txCount
    val row = node.watchedRow
    row.txs.clear()
    row.txs ++= Seq(funding.head, stale, funding.last)
    node.history.historyStorage.insertExtraTry(Array.empty, Array[ExtraIndex](row)).get
    node.storedWatchedTxs shouldBe Seq(funding.head, stale, funding.last)

    // the abandoned block ends with the transaction of the address
    val tx = if (alsoPaysAddress) move(box, watchedTree) else spend(box)
    val abandoned = node.branch(fork)(state => node.ordinaryTxs(state, atLeast = 2) :+ tx)
    val abandonedNumber = node.txCount + abandoned.head.blockTransactions.txs.size - 1
    node.deliver(abandoned)
    node.awaitIndexed()
    node.storedWatchedTxs shouldBe Seq(funding.head, stale, funding.last, abandonedNumber)

    node.deliver(node.branch(fork)(node.ordinary, node.ordinary))
    node.indexDifferences() shouldBe empty
    node.storedWatchedTxs shouldBe funding
  }

  property("a rollback on a row which is out of order keeps the numbers below the fork point when the address has only an input in the abandoned block") {
    rollbackOnRowOutOfOrder(alsoPaysAddress = false)
  }

  property("a rollback on a row which is out of order keeps the numbers below the fork point when the abandoned transaction also pays the address") {
    rollbackOnRowOutOfOrder(alsoPaysAddress = true)
  }

}
