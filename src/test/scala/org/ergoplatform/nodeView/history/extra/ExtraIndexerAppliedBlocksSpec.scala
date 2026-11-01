package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{RemoteBlockApplied, Rollback}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.GetSegmentThreshold
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.boxSegmentId
import org.ergoplatform.nodeView.state.{ErgoStateReader, StateType, UtxoStateReader}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createDigestState, createUtxoState, validFullBlock}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, untilTimeout}
import org.slf4j.LoggerFactory
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.interpreter.ProverResult
import sigmastate.eval.Extensions._

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

/**
  * The extra indexer indexes a block only after the state has applied it (#2703).
  *
  * History marks every block of a batch as best chain at append; the state applies the blocks
  * afterwards, one by one, and can reject one of them. These tests run the production indexer
  * against a real node view holder (history and UTXO state), hand the node blocks in orders the
  * P2P layer can produce, and compare the index with the chain the state has applied: checkpoint,
  * transaction and box rows, their numbering, spent flags and spending heights, address balances
  * and transaction lists, the box lists of contract-template rows, token rows with their box
  * lists, storage-rent rows.
  * Proof of work is not modelled: the node-view test settings use a fake PoW scheme.
  */
class ExtraIndexerAppliedBlocksSpec extends ErgoCorePropertyTest with NodeViewTestOps {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private val settleTimeout = 15.seconds

  private val indexedSettings: ErgoSettings = {
    val base = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    base.copy(nodeSettings = base.nodeSettings.copy(extraIndex = true))
  }

  /** Turns the transactions of a valid child of `parent` into transactions the state rejects. */
  private type Shape = (ErgoFullBlock, Seq[ErgoTransaction]) => Seq[ErgoTransaction]

  /** The last transaction spends a box the parent block has already spent. */
  private val spendsSpentBox: Shape = { (parent, txs) =>
    txs.init :+ txs.last.copy(inputs = parent.blockTransactions.txs.last.inputs.head +: txs.last.inputs.tail)
  }

  /** The last transaction spends a box that never existed. */
  private val spendsUnknownBox: Shape = { (parent, txs) =>
    val unknownId: Array[Byte] = Algos.hash(s"unknown box after ${parent.id}")
    txs.init :+ txs.last.copy(inputs = txs.last.inputs.head.copy(boxId = ADKey @@ unknownId) +: txs.last.inputs.tail)
  }

  /** The last transaction lists its first input twice. */
  private val spendsSameBoxTwice: Shape = { (_, txs) =>
    txs.init :+ txs.last.copy(inputs = txs.last.inputs :+ txs.last.inputs.head)
  }

  /** The block ends with a copy of the last transaction of its parent. */
  private val replaysParentsLastTx: Shape = (parent, txs) => txs :+ parent.blockTransactions.txs.last

  /** The block ends with a copy of the first transaction of its parent. */
  private val replaysParentsFirstTx: Shape = (parent, txs) => txs :+ parent.blockTransactions.txs.head

  /** The block ends with a transaction that has no inputs and an output carrying a token. */
  private val mintsWithoutInputs: Shape = { (parent, txs) =>
    val tokenId = parent.blockTransactions.txs.head.outputs.head.id.toTokenId
    val out = txs.last.outputs.head
    val minting = new ErgoBoxCandidate(out.value, out.ergoTree, out.creationHeight, Colls.fromArray(Array((tokenId, 1L))))
    txs :+ ErgoTransaction(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq(minting))
  }

  /**
    * The block ends with a transaction which mints the token of `mint`, a transaction of the
    * applied chain, once more: its first input is the first input of `mint`, a box which is
    * already spent, and its output carries the token.
    */
  private def mintsAgain(mint: ErgoTransaction): Shape = { (_, txs) =>
    val out = txs.last.outputs.head
    val tokenId = mint.inputs.head.boxId.toTokenId
    val minting = new ErgoBoxCandidate(out.value, out.ergoTree, out.creationHeight, Colls.fromArray(Array((tokenId, 7L))))
    txs :+ ErgoTransaction(IndexedSeq(mint.inputs.head), IndexedSeq.empty, IndexedSeq(minting))
  }

  /** Ids and amounts of the tokens a box carries. */
  private def tokensOf(box: ErgoBoxCandidate): Seq[(ModifierId, Long)] =
    box.additionalTokens.toArray.toSeq.map(token => bytesToId(token._1.toArray) -> token._2)

  /** Id of the token the transaction mints, if it mints one: the id of its first input. */
  private def mintedTokenOf(tx: ErgoTransaction): Option[ModifierId] =
    tx.inputs.headOption.map(input => bytesToId(input.boxId))
      .filter(id => tx.outputs.exists(out => tokensOf(out).exists(_._1 == id)))

  /** Box numbers a template or token row lists, in its segments and in the row; negative if spent. */
  private def listedBoxes[T <: Segment[T] : ClassTag](history: ErgoHistory, row: T): Seq[Long] =
    (0 until row.boxSegmentCount).flatMap { n =>
      history.typedExtraIndexById[T](row.factory(boxSegmentId(row.id, n)).id).toSeq.flatMap(_.boxes)
    } ++ row.boxes

  /** Transaction numbers an address row lists, in its segments and in the row. */
  private def listedTxs(history: ErgoHistory, row: IndexedErgoAddress): Seq[Long] =
    (0 until row.txSegmentCount).flatMap { n =>
      history.typedExtraIndexById[IndexedErgoAddress](SegmentSerializer.txSegmentId(row.id, n)).toSeq.flatMap(_.txs)
    } ++ row.txs

  /** Child of `parent` with valid headers and sections, whose transactions the state rejects. */
  private def rejectedChild(parent: ErgoFullBlock, stateAfterParent: WrappedUtxoState, shape: Shape): ErgoFullBlock = {
    val valid = validFullBlock(Some(parent), stateAfterParent)
    val txs = valid.blockTransactions.copy(txs = shape(parent, valid.blockTransactions.txs))
    val header = valid.header.copy(transactionsRoot = txs.digest)
    ErgoFullBlock(
      header,
      txs.copy(headerId = header.id),
      valid.extension.copy(headerId = header.id),
      valid.adProofs.map(_.copy(headerId = header.id)))
  }

  /**
    * Compares the index with the chain the state has applied: checkpoint, transaction and box
    * rows, their numbering, spent flags and spending heights, address balances and transaction
    * lists, the box lists of contract-template rows, token rows with their box lists, and
    * storage-rent rows. The box lists of address rows are not compared.
    *
    * @param chain   - blocks the state has applied, from height 1
    * @param foreign - transactions of blocks which are not on that chain
    * @return the differences found, empty if the index is exactly the index of `chain`
    */
  private def auditIndex(history: ErgoHistory,
                         state: UtxoStateReader,
                         chain: Seq[ErgoFullBlock],
                         foreign: Seq[ErgoTransaction]): Seq[String] = {
    val diffs = ArrayBuffer.empty[String]
    def check(ok: Boolean, difference: => String): Unit = if (!ok) diffs += difference
    def inputIds(tx: ErgoTransaction): Seq[ModifierId] = tx.inputs.map(input => bytesToId(input.boxId))

    val txs: Seq[(ErgoTransaction, ErgoFullBlock)] = chain.flatMap(block => block.blockTransactions.txs.map(_ -> block))
    val txIds: Set[ModifierId] = txs.map(_._1.id).toSet
    val created: Seq[ErgoBox] = txs.flatMap(_._1.outputs)
    val createdIds: Set[ModifierId] = created.map(box => bytesToId(box.id)).toSet
    val spentOnChain: Set[ModifierId] = txs.flatMap(tx => inputIds(tx._1)).toSet
    // the indexer does not process the inputs of the block at height 1
    val spender: Map[ModifierId, ModifierId] = txs.collect { case (tx, block) if block.header.height > 1 =>
      inputIds(tx).map(_ -> tx.id)
    }.flatten.toMap
    val spendingHeight: Map[ModifierId, Int] = txs.collect { case (tx, block) if block.header.height > 1 =>
      inputIds(tx).map(_ -> block.header.height)
    }.flatten.toMap

    val checkpoint = IndexerState.fromHistory(history)
    check(checkpoint.indexedHeight == chain.last.header.height && checkpoint.indexedHeaderId.contains(chain.last.id),
      s"index is at height ${checkpoint.indexedHeight}, block ${checkpoint.indexedHeaderId}; " +
        s"the applied chain is at height ${chain.last.header.height}, block ${chain.last.id}")
    check(checkpoint.rollbackTo == 0, s"rollback to ${checkpoint.rollbackTo} is pending")
    check(checkpoint.globalTxIndex == txs.size, s"${checkpoint.globalTxIndex} transactions indexed, the chain has ${txs.size}")
    check(checkpoint.globalBoxIndex == created.size, s"${checkpoint.globalBoxIndex} boxes indexed, the chain created ${created.size}")

    txs.zipWithIndex.foreach { case ((tx, block), number) =>
      history.typedExtraIndexById[IndexedErgoTransaction](tx.id) match {
        case Some(row) =>
          check(row.height == block.header.height && row.blockId == block.id && row.globalIndex == number,
            s"transaction ${tx.id} is #$number in block ${block.id} at height ${block.header.height}, " +
              s"its row says #${row.globalIndex} in block ${row.blockId} at height ${row.height}")
          val rowInputs = Try(row.retrieveBody(history).inputs.map(_.id))
          check(block.header.height == 1 || rowInputs.toOption.contains(inputIds(tx)),
            s"transaction ${tx.id} spends ${inputIds(tx)}, its row resolves to $rowInputs")
        case None =>
          diffs += s"transaction ${tx.id} of block ${block.id} has no row"
      }
      check(NumericTxIndex.getTxByNumber(history, number).exists(_.id == tx.id),
        s"transaction number $number does not resolve to ${tx.id}")
    }

    created.zipWithIndex.foreach { case (box, number) =>
      val id = bytesToId(box.id)
      // `chain` is what the state holds
      check(state.boxById(box.id).isDefined != spentOnChain.contains(id),
        s"box $id: spent on the chain = ${spentOnChain.contains(id)}, in the UTXO state = ${state.boxById(box.id).isDefined}")
      history.typedExtraIndexById[IndexedErgoBox](id) match {
        case Some(row) =>
          check(row.globalIndex == number, s"box $id is #$number, its row says #${row.globalIndex}")
          check(row.spendingTxIdOpt == spender.get(id), s"box $id (#$number) is spent by ${spender.get(id)}, its row says ${row.spendingTxIdOpt}")
          check(row.spendingHeightOpt == spendingHeight.get(id),
            s"box $id (#$number) is spent at height ${spendingHeight.get(id)}, its row says ${row.spendingHeightOpt}")
        case None =>
          diffs += s"box $id (#$number) has no row"
      }
      check(NumericBoxIndex.getBoxByNumber(history, number).exists(_.id == id), s"box number $number does not resolve to $id")
    }

    val chainTokens: Set[ModifierId] = created.flatMap(box => tokensOf(box).map(_._1)).toSet
    foreign.filterNot(tx => txIds.contains(tx.id)).foreach { tx =>
      check(history.typedExtraIndexById[IndexedErgoTransaction](tx.id).isEmpty, s"transaction ${tx.id} is not on the applied chain and has a row")
      tx.outputs.map(out => bytesToId(out.id)).filterNot(createdIds.contains).foreach { id =>
        check(history.typedExtraIndexById[IndexedErgoBox](id).isEmpty, s"box $id of transaction ${tx.id} is not on the applied chain and has a row")
      }
      tx.outputs.flatMap(out => tokensOf(out).map(_._1)).filterNot(chainTokens.contains).foreach { id =>
        check(history.typedExtraIndexById[IndexedToken](uniqueId(id)).isEmpty, s"token $id of transaction ${tx.id} is not on the applied chain and has a row")
      }
    }

    def unspentInIndex(box: ErgoBox): Boolean = !spender.contains(bytesToId(box.id))
    created.groupBy(box => hashErgoTree(box.ergoTree)).foreach { case (address, boxes) =>
      val expected = boxes.filter(unspentInIndex).map(_.value).sum
      val indexed = history.typedExtraIndexById[IndexedErgoAddress](address).flatMap(_.balanceInfo).map(_.nanoErgs)
      check(indexed.contains(expected), s"address $address holds $expected nanoErg, its row says $indexed")
    }

    // an address lists the transactions which create a box of it or spend one, by their numbers
    val addressOf: Map[ModifierId, ModifierId] = created.map(box => bytesToId(box.id) -> hashErgoTree(box.ergoTree)).toMap
    txs.zipWithIndex.flatMap { case ((tx, block), number) =>
      val spent = if (block.header.height > 1) inputIds(tx).flatMap(addressOf.get) else Seq.empty
      (spent ++ tx.outputs.map(out => hashErgoTree(out.ergoTree))).distinct.map(_ -> number.toLong)
    }.groupBy(_._1).foreach { case (address, entries) =>
      val expected = entries.map(_._2)
      val indexed = history.typedExtraIndexById[IndexedErgoAddress](address).map(row => listedTxs(history, row))
      check(indexed.contains(expected), s"address $address: transactions of the chain missing from its row " +
        s"${expected.diff(indexed.getOrElse(Seq.empty))}, entries of its row which are not its transactions on the chain " +
        s"${indexed.map(_.diff(expected))}, or the order differs")
    }

    // box numbers as template and token rows list them: negative if the index has the box as spent
    val listed: Seq[(ErgoBox, Long)] = created.zipWithIndex.map { case (box, number) =>
      box -> (if (unspentInIndex(box)) number.toLong else -number.toLong)
    }
    listed.groupBy(entry => hashTreeTemplate(entry._1.ergoTree)).foreach { case (template, entries) =>
      val expected = entries.map(_._2)
      val indexed = history.typedExtraIndexById[IndexedContractTemplate](template).map(row => listedBoxes(history, row))
      check(indexed.contains(expected), s"contract template $template: boxes of the chain missing from its row " +
        s"${expected.diff(indexed.getOrElse(Seq.empty))}, entries of its row which are not boxes of the chain ${indexed.map(_.diff(expected))}")
    }

    txs.foreach { case (tx, _) =>
      mintedTokenOf(tx).foreach { tokenId =>
        val minted = tx.outputs.flatMap(out => tokensOf(out).filter(_._1 == tokenId).map(token => bytesToId(out.id) -> token._2))
        val expected = listed.collect { case (box, number) if tokensOf(box).exists(_._1 == tokenId) => number }
        history.typedExtraIndexById[IndexedToken](uniqueId(tokenId)) match {
          case Some(row) =>
            check(row.tokenId == tokenId && row.boxId.contains(minted.head._1) && row.amount.contains(minted.map(_._2).sum),
              s"token $tokenId is minted in box ${minted.head._1}, amount ${minted.map(_._2).sum}, by transaction ${tx.id}; " +
                s"its row says box ${row.boxId}, amount ${row.amount}")
            val indexed = listedBoxes(history, row)
            check(indexed == expected, s"token $tokenId: boxes of the chain missing from its row ${expected.diff(indexed)}, " +
              s"entries of its row which are not boxes of the chain ${indexed.diff(expected)}")
          case None =>
            diffs += s"token $tokenId minted by transaction ${tx.id} has no row"
        }
      }
    }

    val rentRows = history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue).map(_.globalIndex).sorted.toSeq
    val unspentNumbers = created.zipWithIndex.collect { case (box, number) if unspentInIndex(box) => number.toLong }
    check(rentRows == unspentNumbers,
      s"storage-rent rows without an unspent box: ${rentRows.diff(unspentNumbers)}, unspent boxes without a row: ${unspentNumbers.diff(rentRows)}")

    diffs.toSeq
  }

  /**
    * A real node view holder (history and UTXO state) with four applied blocks and, if asked for,
    * the production indexer caught up with them.
    */
  private class IndexedNode(fixture: NodeViewFixture, startIndexer: Boolean) {

    import fixture._

    /** Blocks the state has applied. */
    val chain: ArrayBuffer[ErgoFullBlock] = ArrayBuffer.empty[ErgoFullBlock]

    /** Copy of the state after the last block of `chain`, to build further blocks with. */
    var tipState: WrappedUtxoState = {
      val (genesisState, boxHolder) = createUtxoState(settings)
      val genesis = validFullBlock(None, genesisState, boxHolder)
      val afterGenesis = WrappedUtxoState(genesisState, boxHolder, settings).applyModifier(genesis)(_ => ()).get
      applyBlock(genesis) shouldBe 'success
      chain += genesis
      afterGenesis
    }
    (1 to 3).foreach(_ => extend())

    /** Number of times the indexer actor failed and was restarted by its supervisor. */
    val indexerRestarts = new AtomicInteger(0)

    private val indexerOpt: Option[ActorRef] = if (startIndexer) Some(actorSystem.actorOf(Props(
      new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true) {
        override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
          indexerRestarts.incrementAndGet()
          super.preRestart(reason, message)
        }
      }))) else None
    if (startIndexer) indexDifferences() shouldBe empty

    def history: ErgoHistory = getHistory

    def checkpoint: IndexerState = IndexerState.fromHistory(getHistory)

    def stopIndexer(): Unit = indexerOpt.foreach(actorSystem.stop)

    /** Valid blocks on top of each other, the first on top of `chain`, each with the state after it. */
    def build(count: Int): Seq[(ErgoFullBlock, WrappedUtxoState)] =
      (1 to count).scanLeft(chain.last -> tipState) { case ((parent, parentState), _) =>
        val block = validFullBlock(Some(parent), parentState)
        block -> parentState.applyModifier(block)(_ => ()).get
      }.tail

    /** Records blocks the state has applied. */
    def accept(blocks: Seq[(ErgoFullBlock, WrappedUtxoState)]): Unit = {
      chain ++= blocks.map(_._1)
      tipState = blocks.last._2
    }

    /** Hands the node a block, header first, then its sections. */
    def deliver(block: ErgoFullBlock): Unit = applyBlock(block) shouldBe 'success

    /**
      * Hands the node consecutive blocks so that history takes all of them in one step: every
      * header, then the sections of the last block first and those of the first block last.
      */
    def deliverChildFirst(blocks: Seq[ErgoFullBlock]): Unit = {
      val bestBefore = getHistory.bestFullBlockIdOpt
      blocks.foreach(block => applyHeader(block.header) shouldBe 'success)
      blocks.tail.reverse.foreach(block => applyPayload(block) shouldBe 'success)
      getHistory.bestFullBlockIdOpt shouldBe bestBefore
      applyPayload(blocks.head) shouldBe 'success
    }

    def awaitApplied(block: ErgoFullBlock): Unit = untilTimeout(settleTimeout, 20.millis) {
      getHistory.bestFullBlockIdOpt shouldBe Some(block.id)
      getCurrentState.version shouldBe idToVersion(block.id)
    }

    def awaitRejected(block: ErgoFullBlock): Unit = untilTimeout(settleTimeout, 20.millis) {
      getHistory.isSemanticallyValid(block.id) shouldBe ModifierSemanticValidity.Invalid
    }

    /** One more valid block, handed over the usual way and applied. */
    def extend(): ErgoFullBlock = {
      val next = build(1)
      deliver(next.head._1)
      awaitApplied(next.head._1)
      accept(next)
      next.head._1
    }

    /**
      * One more valid block whose last transaction mints a token, handed over the usual way
      * and applied.
      *
      * @return the minting transaction
      */
    def extendWithMint(): ErgoTransaction = {
      val txs = validFullBlock(Some(chain.last), tipState).blockTransactions.txs
      val spent = txs.last.outputs.head
      val tokens = spent.additionalTokens.toArray :+ ((spent.id.toTokenId, 1000L))
      val minting = new ErgoBoxCandidate(spent.value, spent.ergoTree, spent.creationHeight, Colls.fromArray(tokens))
      val mint = ErgoTransaction(IndexedSeq(Input(spent.id, ProverResult.empty)), IndexedSeq.empty, IndexedSeq(minting))
      val block = validFullBlock(Some(chain.last), tipState, txs :+ mint)
      deliver(block)
      awaitApplied(block)
      accept(Seq(block -> tipState.applyModifier(block)(_ => ()).get))
      mint
    }

    /**
      * Hands the node a valid block together with a child the state rejects, the child first.
      *
      * @return the valid block, which the state applies, and the rejected child
      */
    def deliverWithRejectedChild(shape: Shape): (ErgoFullBlock, ErgoFullBlock) = {
      val good = build(1)
      val bad = rejectedChild(good.head._1, good.head._2, shape)
      deliverChildFirst(Seq(good.head._1, bad))
      awaitApplied(good.head._1)
      awaitRejected(bad)
      accept(good)
      good.head._1 -> bad
    }

    /**
      * Differences between the index and `chain`, see [[auditIndex]]. Gives the indexer time to
      * reach the applied tip first; an index which does not get there is reported as a difference.
      */
    def indexDifferences(foreign: Seq[ErgoTransaction] = Seq.empty): Seq[String] = {
      Try(untilTimeout(settleTimeout, 20.millis) {
        checkpoint.indexedHeaderId shouldBe Some(chain.last.id)
      })
      auditIndex(getHistory, getCurrentState.asInstanceOf[UtxoStateReader], chain, foreign)
    }
  }

  private def withNode(startIndexer: Boolean = true)(test: (NodeViewFixture, IndexedNode) => Unit): Unit =
    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      val node = new IndexedNode(fixture, startIndexer)
      try test(fixture, node) finally {
        node.stopIndexer()
        node.tipState.closeStorage() // all copies of the state share one store
      }
    }

  /**
    * Runs `body` with a counter of the writes of buffered index rows to the database. A write
    * is not visible from outside the indexer other than by the debug message it logs.
    */
  private def countingFlushes[T](body: AtomicInteger => T): T = {
    val flushes = new AtomicInteger(0)
    val logger = LoggerFactory.getLogger(classOf[ExtraIndexer].getPackage.getName).asInstanceOf[LogbackLogger]
    val levelBefore = logger.getLevel
    val appender = new AppenderBase[ILoggingEvent] {
      override def append(event: ILoggingEvent): Unit =
        if (String.valueOf(event.getFormattedMessage).contains("inserted them to database")) flushes.incrementAndGet()
    }
    appender.setContext(logger.getLoggerContext)
    appender.start()
    logger.setLevel(Level.DEBUG)
    logger.addAppender(appender)
    try body(flushes) finally {
      logger.detachAppender(appender)
      logger.setLevel(levelBefore)
      appender.stop()
    }
  }

  /**
    * A valid block and a child the state rejects reach the node child-first, `attempts` times in
    * a row; one more valid block follows each time. The index must be the index of the applied
    * chain afterwards, without the indexer having failed on the way.
    */
  private def rejectedChildLeavesIndexIntact(shape: Shape, attempts: Int): Unit = withNode() { (_, node) =>
    (1 to attempts).foreach { _ =>
      val (_, bad) = node.deliverWithRejectedChild(shape)
      node.extend()
      node.indexDifferences(bad.blockTransactions.txs) shouldBe empty
      node.indexerRestarts.get shouldBe 0
    }
  }

  property("a block the state rejects is not indexed, also while no further block arrives") {
    withNode() { (_, node) =>
      (1 to 2).foreach { _ =>
        val (good, bad) = node.deliverWithRejectedChild(spendsSpentBox)
        // after a rejection no event reaches the indexer until the next block is applied:
        // the index has to get to the valid block without one, and to stay there
        untilTimeout(settleTimeout, 20.millis)(node.checkpoint.indexedHeaderId shouldBe Some(good.id))
        Thread.sleep(2500)
        val checkpoint = node.checkpoint
        checkpoint.indexedHeight shouldBe node.history.fullBlockHeight
        checkpoint.indexedHeaderId shouldBe Some(good.id)
        node.history.typedExtraIndexById[IndexedErgoTransaction](bad.blockTransactions.txs.last.id) shouldBe None
        node.indexDifferences(bad.blockTransactions.txs) shouldBe empty

        node.extend()
        node.indexDifferences(bad.blockTransactions.txs) shouldBe empty
      }
      node.indexerRestarts.get shouldBe 0
    }
  }

  property("a rejected block replaying the last transaction of its parent leaves the index intact") {
    rejectedChildLeavesIndexIntact(replaysParentsLastTx, attempts = 3)
  }

  property("a rejected block replaying the first transaction of its parent leaves the index intact") {
    rejectedChildLeavesIndexIntact(replaysParentsFirstTx, attempts = 3)
  }

  property("a rejected block with a transaction without inputs and a token output leaves the index intact") {
    rejectedChildLeavesIndexIntact(mintsWithoutInputs, attempts = 3)
  }

  property("a rejected block spending an unknown box leaves the index intact") {
    rejectedChildLeavesIndexIntact(spendsUnknownBox, attempts = 2)
  }

  property("a rejected block with a transaction listing the same input twice leaves the index intact") {
    rejectedChildLeavesIndexIntact(spendsSameBoxTwice, attempts = 3)
  }

  property("a rejected block minting a token of the applied chain once more leaves the index intact") {
    withNode() { (_, node) =>
      (1 to 2).foreach { _ =>
        val mint = node.extendWithMint()
        node.indexDifferences() shouldBe empty

        val (_, bad) = node.deliverWithRejectedChild(mintsAgain(mint))
        node.extend()
        node.indexDifferences(bad.blockTransactions.txs) shouldBe empty
        node.indexerRestarts.get shouldBe 0
      }
    }
  }

  property("a single rejected block on the tip, sections in the usual order, leaves the index intact") {
    withNode() { (_, node) =>
      (1 to 2).foreach { _ =>
        val tip = node.chain.last
        val bad = rejectedChild(tip, node.tipState, spendsSpentBox)
        node.deliver(bad)
        node.awaitRejected(bad)
        node.awaitApplied(tip)
        node.indexDifferences(bad.blockTransactions.txs) shouldBe empty

        node.extend()
        node.indexDifferences(bad.blockTransactions.txs) shouldBe empty
      }
      node.indexerRestarts.get shouldBe 0
    }
  }

  property("two valid blocks taken by history in one step are both indexed") {
    withNode() { (_, node) =>
      (1 to 2).foreach { _ =>
        val blocks = node.build(2)
        node.deliverChildFirst(blocks.map(_._1))
        node.awaitApplied(blocks.last._1)
        node.accept(blocks)
        node.indexDifferences() shouldBe empty
      }
    }
  }

  property("a burst of valid blocks taken by history in one step is indexed up to the chain tip") {
    withNode() { (_, node) =>
      val blocks = node.build(6)
      node.deliverChildFirst(blocks.map(_._1))
      node.awaitApplied(blocks.last._1)
      node.accept(blocks)
      node.indexDifferences() shouldBe empty
      node.checkpoint.indexedHeight shouldBe node.history.fullBlockHeight

      node.extend()
      node.indexDifferences() shouldBe empty
    }
  }

  property("a burst of blocks is written to the index in fewer writes than blocks, and completely") {
    withNode() { (_, node) =>
      // all blocks valid: the last event names the best full block of history
      val valid = node.build(12)
      val writesForValid = countingFlushes { writes =>
        node.deliverChildFirst(valid.map(_._1))
        node.awaitApplied(valid.last._1)
        node.accept(valid)
        // no further event follows
        untilTimeout(5.seconds, 20.millis)(node.checkpoint.indexedHeaderId shouldBe Some(valid.last._1.id))
        node.checkpoint.indexedHeight shouldBe node.history.fullBlockHeight
        // the checkpoint is in the store a moment before the write is logged
        untilTimeout(5.seconds, 20.millis)(writes.get should be >= 1)
        writes.get
      }
      info(s"${valid.size} valid blocks taken in one step, index writes: $writesForValid")
      writesForValid should (be >= 1 and be < valid.size)
      node.indexDifferences() shouldBe empty

      // the state rejects the last block: no event names a block history has nothing above
      val applied = node.build(10)
      val bad = rejectedChild(applied.last._1, applied.last._2, spendsSpentBox)
      val writesForApplied = countingFlushes { writes =>
        node.deliverChildFirst(applied.map(_._1) :+ bad)
        node.awaitApplied(applied.last._1)
        node.awaitRejected(bad)
        node.accept(applied)
        // no further event follows
        untilTimeout(5.seconds, 20.millis)(node.checkpoint.indexedHeaderId shouldBe Some(applied.last._1.id))
        untilTimeout(5.seconds, 20.millis)(writes.get should be >= 1)
        writes.get
      }
      info(s"${applied.size} valid blocks and a rejected one taken in one step, index writes: $writesForApplied")
      writesForApplied should (be >= 1 and be < applied.size)
      node.indexDifferences(bad.blockTransactions.txs) shouldBe empty
      node.indexerRestarts.get shouldBe 0
    }
  }

  /**
    * Two competing valid blocks `a` and `b1` on top of `node.chain`, built on the same state,
    * and the state copy after `b1` to build the rest of branch b with.
    */
  private def fork(node: IndexedNode): (ErgoFullBlock, ErgoFullBlock, WrappedUtxoState) = {
    val a = validFullBlock(Some(node.chain.last), node.tipState)
    // another timestamp, so that the block differs from `a` whatever transactions it gets
    val b1 = validFullBlock(Some(node.chain.last), node.tipState, a.header.timestamp + 1)
    (a, b1, node.tipState.applyModifier(b1)(_ => ()).get)
  }

  /** Branch b of [[fork]] with a second valid block, each block with the state after it. */
  private def twoBlockBranch(b1: ErgoFullBlock, afterB1: WrappedUtxoState): Seq[(ErgoFullBlock, WrappedUtxoState)] = {
    val b2 = validFullBlock(Some(b1), afterB1)
    Seq(b1 -> afterB1, b2 -> afterB1.applyModifier(b2)(_ => ()).get)
  }

  property("the index follows the node to a better branch") {
    withNode() { (_, node) =>
      val (a, b1, afterB1) = fork(node)
      val b = twoBlockBranch(b1, afterB1)
      node.deliver(a)
      node.awaitApplied(a)
      node.chain += a
      node.indexDifferences() shouldBe empty

      // the second block of the other branch makes it the better one
      b.foreach(block => node.deliver(block._1))
      node.awaitApplied(b.last._1)
      node.chain.remove(node.chain.length - 1)
      node.accept(b)
      node.indexDifferences(a.blockTransactions.txs) shouldBe empty

      node.extend()
      node.indexDifferences(a.blockTransactions.txs) shouldBe empty
      node.indexerRestarts.get shouldBe 0
    }
  }

  property("a better branch with a block the state rejects is indexed up to its last applied block") {
    withNode() { (_, node) =>
      val (a, b1, afterB1) = fork(node)
      val badB2 = rejectedChild(b1, afterB1, spendsSpentBox)
      node.deliver(a)
      node.awaitApplied(a)
      node.chain += a
      node.indexDifferences() shouldBe empty

      // history switches to the longer branch, the state applies b1 and rejects its child
      node.deliverChildFirst(Seq(b1, badB2))
      node.awaitRejected(badB2)
      node.awaitApplied(b1)
      node.chain.remove(node.chain.length - 1)
      node.accept(Seq(b1 -> afterB1))
      val foreign = a.blockTransactions.txs ++ badB2.blockTransactions.txs
      node.indexDifferences(foreign) shouldBe empty

      node.extend()
      node.indexDifferences(foreign) shouldBe empty
      node.indexerRestarts.get shouldBe 0
    }
  }

  property("the indexer follows the applied tip it is told about, not the best-chain marker") {
    withNode(startIndexer = false) { (fixture, node) =>
      import fixture._

      // the node ends on branch b; history keeps `a` as a block off the best chain
      val forkPoint = node.chain.last
      val (a, b1, afterB1) = fork(node)
      val b = twoBlockBranch(b1, afterB1)
      val b2 = b.last._1
      node.deliver(a)
      node.awaitApplied(a)
      b.foreach(block => node.deliver(block._1))
      node.awaitApplied(b2)
      node.accept(b)
      val history = node.history

      // Starts an indexer which is told at start that the state is at `applied`. Later block
      // events come from the test only: the node stays idle.
      def startIndexerAt(applied: ErgoFullBlock): ActorRef = {
        val viewHolder = TestProbe()
        val indexer = actorSystem.actorOf(Props(
          new ExtraIndexer(settings.cacheSettings, settings.chainSettings.addressEncoder, Some(viewHolder.ref), true)))
        val request = viewHolder.expectMsgType[GetDataFromCurrentView[_, _]].asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = createDigestState(idToVersion(applied.id), applied.header.stateRoot)
        try viewHolder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
        // the indexer answers this once it has taken the snapshot
        viewHolder.send(indexer, GetSegmentThreshold)
        viewHolder.expectMsg(512)
        indexer
      }
      def stopAndWait(indexer: ActorRef): Unit = {
        val watcher = TestProbe()
        watcher.watch(indexer)
        actorSystem.stop(indexer)
        watcher.expectTerminated(indexer)
      }
      def awaitIndexedExactly(block: ErgoFullBlock): Unit = {
        untilTimeout(settleTimeout, 20.millis)(node.checkpoint.indexedHeaderId shouldBe Some(block.id))
        Thread.sleep(1000) // nothing above it follows
        node.checkpoint.indexedHeaderId shouldBe Some(block.id)
        node.checkpoint.indexedHeight shouldBe block.header.height
      }

      // an empty index and a state two blocks behind the best full block of history: the
      // indexer catches up with the state, not with history
      val behind = startIndexerAt(forkPoint)
      awaitIndexedExactly(forkPoint)
      stopAndWait(behind)

      // the state is on the abandoned branch, at the height of b1: nothing is indexed, though
      // history has a best-chain block at that height
      val indexer = startIndexerAt(a)
      try {
        awaitIndexedExactly(forkPoint)

        indexer ! RemoteBlockApplied(b1.header, b1.blockTransactions.txs.map(_.id))
        // the node publishes the rollback of a chain switch after the blocks of the new branch;
        // b1 is still buffered then, as history holds b2 above it, and has to be written
        indexer ! Rollback(forkPoint.id)
        awaitIndexedExactly(b1)
        b2.blockTransactions.txs.foreach { tx =>
          history.typedExtraIndexById[IndexedErgoTransaction](tx.id) shouldBe None
        }

        indexer ! RemoteBlockApplied(b2.header, b2.blockTransactions.txs.map(_.id))
        awaitIndexedExactly(b2)
        node.indexDifferences(a.blockTransactions.txs) shouldBe empty
      } finally {
        stopAndWait(indexer)
      }
    }
  }

}
