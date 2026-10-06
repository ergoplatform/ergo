package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{RemoteBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.Index
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, bytesToId}
import spire.implicits.cfor

import java.util.concurrent.locks.{Condition, ReentrantLock}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag

class ExtraIndexerSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder
  val initSettings: ErgoSettings = settings
  case class CreateDB(blockCount: Int)
  case class ExtendDB(blockCount: Int)
  case class Reset()
  case class GenerateBetterChainTip()
  case class SetCaughtUp(caughtUp: Boolean)

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  val HEIGHT: Int = 50
  val BRANCHPOINT: Int = HEIGHT / 2
  implicit val segmentThreshold: Int = 8

  val system: ActorSystem = ActorSystem.create("indexer-test")
  val indexer: ActorRef = system.actorOf(Props.create(classOf[ExtraIndexerTestActor], this, Boolean.box(true)))

  var _history: ErgoHistory = _
  def history: ErgoHistoryReader = _history.getReader

  val lock: ReentrantLock = new ReentrantLock()
  val done: Condition = lock.newCondition()
  val created: Condition = lock.newCondition()

  def awaitCondition(condition: Condition): Unit = {
    lock.lock()
    try condition.await()
    finally lock.unlock()
  }

  def manualIndex(limit: Int): (ID_LL, // address -> (erg,tokenSum)
                                ID_LL, // template -> (spentBoxCount,unspentBoxCount)
                                ID_LL, // tokenId -> (boxesCount,_)
                                Int, // txs indexed
                                Int) = { // boxes indexed
    var txsIndexed = 0
    var boxesIndexed = 0
    val addresses: ID_LL = mutable.HashMap[ModifierId, (Long, Long)]()
    val templates: ID_LL = mutable.HashMap[ModifierId, (Long, Long)]()
    val indexedTokens: ID_LL = mutable.HashMap[ModifierId, (Long, Long)]()
    cfor(1)(_ <= limit, _ + 1) { i =>
      val header = history.headerIdsAtHeight(i).last
      val block = history.getFullBlock(history.typedModifierById[Header](header).get)
      block.get.transactions.foreach { tx =>
        txsIndexed += 1
        if (i != 1) {
          tx.inputs.foreach { input =>
            val iEb: IndexedErgoBox = _history.getReader.typedExtraIndexById[IndexedErgoBox](bytesToId(input.boxId)).get
            val address = hashErgoTree(ExtraIndexer.getAddress(iEb.box.ergoTree)(addressEncoder).script)
            val prevAddress = addresses(address)
            addresses.put(address, (prevAddress._1 - iEb.box.value, prevAddress._2 - iEb.box.additionalTokens.toArray.map(_._2).sum))
            val template = hashTreeTemplate(ExtraIndexer.getAddress(iEb.box.ergoTree)(addressEncoder).script)
            val prevTemplate = templates(template)
            templates.put(template, (prevTemplate._1 + 1, prevTemplate._2 - 1))
          }
        }
        tx.outputs.foreach { output =>
          boxesIndexed += 1
          val address = hashErgoTree(ExtraIndexer.getAddress(output.ergoTree)(addressEncoder).script)
          val prevAddress = addresses.getOrElse(address, (0L, 0L))
          addresses.put(address, (prevAddress._1 + output.value, prevAddress._2 + output.additionalTokens.toArray.map(_._2).sum))
          val template = hashTreeTemplate(ExtraIndexer.getAddress(output.ergoTree)(addressEncoder).script)
          val prevTemplate = templates.getOrElse(template, (0L, 0L))
          templates.put(template, (prevTemplate._1, prevTemplate._2 + 1))
          cfor(0)(_ < output.additionalTokens.length, _ + 1) { j =>
            val token = IndexedToken.fromBox(new IndexedErgoBox(i, None, None, None, output, 0), j)
            val prev2 = indexedTokens.getOrElse(token.id, (0L, 0L))
            indexedTokens.put(token.id, (prev2._1 + 1, 0))
          }
        }
      }
    }
    (addresses, templates, indexedTokens, txsIndexed, boxesIndexed)
  }

  def checkSegmentables[T <: Segment[T] : ClassTag](segmentables: ID_LL,
                                                    isChild: Boolean = false,
                                                    check: ((T, (Long, Long))) => Boolean): Int = {
    var errors: Int = 0
    segmentables.foreach { segmentable =>
      history.typedExtraIndexById[T](segmentable._1) match {
        case Some(obj: T) =>
          if (isChild) { // this is a segment
            // check tx segments
            val txSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            txSegments ++= (0 until obj.txSegmentCount).map(n => obj.factory(txSegmentId(obj.id, n)).id).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(txSegments, isChild = true, check) shouldBe 0
            // check box segments
            val boxSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            boxSegments ++= (0 until obj.boxSegmentCount).map(n => obj.factory(boxSegmentId(obj.id, n)).id).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(boxSegments, isChild = true, check) shouldBe 0
          } else { // this is the parent object
            // check properties of object
            if (!check((obj, segmentable._2)))
              errors += 1
          }
          // check boxes in memory
          obj.boxes.foreach { boxNum =>
            NumericBoxIndex.getBoxByNumber(history, boxNum) match {
              case Some(iEb) =>
                if (iEb.isSpent)
                  boxNum.toInt should be <= 0
                else
                  boxNum.toInt should be >= 0
              case None =>
                System.err.println(s"Box $boxNum not found in database")
                errors += 1
            }
          }
          // check txs in memory
          obj.txs.foreach { txNum =>
            NumericTxIndex.getTxByNumber(history, txNum) shouldNot be(empty)
          }

        case None =>
          System.err.println(s"Segmentable object ${segmentable._1} should exist, but was not found")
          errors += 1
      }
    }
    errors
  }

  def checkAddresses(addresses: ID_LL): Int =
    checkSegmentables[IndexedErgoAddress](addresses, isChild = false, seg => {
      seg._1.balanceInfo.get.nanoErgs == seg._2._1 && seg._1.balanceInfo.get.tokens.map(_._2).sum == seg._2._2
    })

  def checkTemplates(templates: ID_LL): Int =
    checkSegmentables[IndexedContractTemplate](templates, isChild = false, seg => {
      seg._1.boxCount == (seg._2._1 + seg._2._2)
    })

  def checkTokens(indexedTokens: ID_LL): Int =
    checkSegmentables[IndexedToken](indexedTokens, isChild = false, seg => {
      seg._1.boxCount == seg._2._1
    })

  /**
    * Verify the storage-rent eligibility index against the ground truth: every unspent
    * IndexedErgoBox must have exactly one rent entry with matching fields, in ascending
    * (creationHeight, globalIndex) order.
    */
  def checkRentIndex(): Unit = {
    val state = IndexerState.fromHistory(_history)
    val expected = (0L until state.globalBoxIndex).flatMap { boxNum =>
      NumericBoxIndex.getBoxByNumber(history, boxNum).filter(!_.isSpent)
    }
    val rentEntries = history.storageRentBoxesAtOrBefore(Int.MaxValue, math.max(expected.length * 2, 100))
    rentEntries.length shouldBe expected.length
    val byGlobalIndex = expected.map(iEb => iEb.globalIndex -> iEb).toMap
    rentEntries.foreach { srb =>
      byGlobalIndex.get(srb.globalIndex) match {
        case Some(iEb) =>
          srb.creationHeight shouldBe iEb.box.creationHeight
          // the resolution the claim path uses must land on the same box
          NumericBoxIndex.getBoxByNumber(history, srb.globalIndex).map(_.id) shouldBe Some(iEb.id)
        case None =>
          fail(s"Unexpected storage-rent entry for global index ${srb.globalIndex}")
      }
    }
    // ascending order by (creationHeight, globalIndex); note globalIndex alone is NOT
    // monotonic here: test chains create boxes whose R3 creation height differs from the
    // inclusion order, and the rent clock runs on R3
    rentEntries.map(e => (e.creationHeight, e.globalIndex)).toSeq shouldBe
      rentEntries.map(e => (e.creationHeight, e.globalIndex)).toSeq.sorted
  }

  /**
    * Ground truth for the rent index derived WITHOUT any indexer state: replay the best
    * chain's blocks and keep the boxes that were created and never spent, assigning the
    * global box index in output order. The genesis box is not part of the chain's blocks,
    * so the produced indices are offset relative to `NumericBoxIndex`, which counts from
    * the genesis box.
    *
    * This is deliberately independent of `IndexerState` and `NumericBoxIndex`: those are
    * maintained by the indexer under test, so a systematic miscount of the rent rows
    * could cancel out in a comparison using them on both sides.
    */
  def unspentBoxesFromChain(height: Int): Map[ModifierId, (Int, Long)] = {
    val created = mutable.HashMap.empty[ModifierId, (Int, Long)]
    val spent = mutable.HashSet.empty[ModifierId]
    var globalIndex = 0L
    var h = 1
    while (h <= height) {
      history.bestHeaderIdAtHeight(h).foreach { headerId =>
        history.typedModifierById[Header](headerId).foreach { header =>
          history.getFullBlock(header).foreach { block =>
            block.transactions.foreach { tx =>
              tx.inputs.foreach(in => spent += bytesToId(in.boxId))
              tx.outputs.foreach { out =>
                created.put(bytesToId(out.id), (out.creationHeight, globalIndex))
                globalIndex += 1
              }
            }
          }
        }
      }
      h += 1
    }
    created.filterNot { case (id, _) => spent.contains(id) }.toMap
  }

  /**
    * The rent index must hold exactly the boxes the chain has created and never spent, and
    * its rows must be ordered by (creationHeight, globalIndex) - the order the ascending
    * range scan in `HistoryStorage.storageRentBoxesAtOrBefore` relies on.
    *
    * Box ids are compared against the chain-derived truth, so this catches rent rows that
    * survive a rollback for boxes that no longer exist, and rows missing for boxes that do.
    */
  def checkRentIndexAgainstChain(height: Int): Unit = {
    val expected = unspentBoxesFromChain(height)
    val rentEntries =
      history.storageRentBoxesAtOrBefore(Int.MaxValue, math.max(expected.size * 4, 1000))

    withClue(s"rent index size at height $height: ") {
      rentEntries.length shouldBe expected.size
    }

    // rent rows carry no payload, so resolve the box id through the box-number index,
    // exactly like the claim path in CandidateGenerator does
    val resolved = rentEntries.flatMap(e => NumericBoxIndex.getBoxByNumber(history, e.globalIndex))
    withClue("every rent row must resolve to a box: ") {
      resolved.length shouldBe rentEntries.length
    }
    val actualIds = resolved.map(_.id).toSet
    withClue("rent index must cover exactly the boxes unspent on the best chain: ") {
      actualIds shouldBe expected.keySet
    }

    // creation height is part of the key, so it must match the box exactly
    rentEntries.foreach { e =>
      NumericBoxIndex.getBoxByNumber(history, e.globalIndex).foreach { iEb =>
        e.creationHeight shouldBe iEb.box.creationHeight
        iEb.isSpent shouldBe false
      }
    }

    // The global index cannot be compared against a chain replay: the indexer counts
    // from the genesis box while a chain replay only sees block outputs. What matters
    // for the range scan is that the numbering is strictly increasing in
    // (creationHeight, globalIndex) order, so the rows form one ascending, unique run.
    val keys = rentEntries.map(e => (e.creationHeight, e.globalIndex)).toSeq
    withClue("rent rows must be strictly ascending by (creationHeight, globalIndex): ") {
      keys shouldBe keys.sorted
      keys.distinct.length shouldBe keys.length
    }

  }

  // example G-30;R-20;G-35;R-30
  def rollbackWithPattern(pattern: String, checkChain: Boolean = false): Unit = {
    // when checkChain is set, the rent rows are additionally verified against the
    // chain-derived unspent set, which does not rely on indexer state
    var rolledBackTo: Int = 0
    def checkRent(): Unit = {
      checkRentIndex()
      if (checkChain) checkRentIndexAgainstChain(rolledBackTo)
    }

    def rollback(n: Int): Unit = {
      println(s"Rollback to $n")
      var state = IndexerState.fromHistory(_history)

      val txIndexBefore = state.globalTxIndex
      val boxIndexBefore = state.globalBoxIndex

      // manually count balances
      val (addresses, templates, indexedTokens, txsIndexed, boxesIndexed) = manualIndex(n)

      // perform rollback
      indexer ! Rollback(history.bestHeaderIdAtHeight(n).get)
      lock.lock()
      done.await()
      state = IndexerState.fromHistory(_history)

      // address balances
      checkAddresses(addresses) shouldBe 0

      addresses.keys.foreach { addr =>
        val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
          .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, unconfirmed = false, Set.empty)
        utxos.exists(_.isSpent) shouldBe false
      }

      checkTemplates(templates) shouldBe 0

      // token indexes
      checkTokens(indexedTokens) shouldBe 0

      // check indexnumbers
      state.globalTxIndex shouldBe txsIndexed
      state.globalBoxIndex shouldBe boxesIndexed

      // check txs
      cfor(0)(_ < txIndexBefore, _ + 1) { txNum =>
        val txOpt = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(txNum)))
        if (txNum < state.globalTxIndex)
          txOpt shouldNot be(empty)
        else
          txOpt shouldBe None
      }

      // check boxes
      cfor(0)(_ < boxIndexBefore, _ + 1) { boxNum =>
        val boxOpt = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(boxNum)))
        if (boxNum < state.globalBoxIndex)
          boxOpt shouldNot be(empty)
        else
          boxOpt shouldBe None
      }

      rolledBackTo = n
      checkRent()
    }

    def generate(n: Int): Unit = {
      println(s"Generate to $n")
      indexer ! CreateDB(n)
      indexer ! Index()
      lock.lock()
      done.await()

      val (addresses, _, _, _, _) = manualIndex(n)

      addresses.keys.foreach { addr =>
        val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
          .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, unconfirmed = false, Set.empty)
        val trees = utxos.map(_.box.ergoTree).map(hashErgoTree)
        trees.forall(_ == addr) shouldBe true
      }

      addresses.keys.foreach { addr =>
        val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
          .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, unconfirmed = false, Set.empty)
        utxos.exists(_.isSpent) shouldBe false
      }

      rolledBackTo = n
      checkRent()
    }

    pattern.split(";").map(_.split("-")).map(x => x(0) -> x(1).toInt).foreach {
      case ("G", n) => generate(n)
      case ("R", n) => rollback(n)
      case _ => System.err.println(s"Malformed rollback pattern: $pattern")
    }

    indexer ! Reset()
  }

  property("skips a duplicate applied block without blocking later blocks") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)

    indexer ! ExtendDB(HEIGHT + 2)
    awaitCondition(created)
    val firstHeader = history.typedModifierById[Header](history.bestHeaderIdAtHeight(HEIGHT + 1).get).get
    val secondHeader = history.typedModifierById[Header](history.bestHeaderIdAtHeight(HEIGHT + 2).get).get
    val blocks = (1 to HEIGHT + 2).map(height => history.bestBlockTransactionsAt(height).get)
    val expectedTxCount = blocks.map(_.txs.size.toLong).sum
    val expectedBoxCount = blocks.flatMap(_.txs).map(_.outputs.size.toLong).sum
    indexer ! RemoteBlockApplied(firstHeader, history.getFullBlock(firstHeader).get.transactions.map(_.id))
    indexer ! RemoteBlockApplied(firstHeader, history.getFullBlock(firstHeader).get.transactions.map(_.id))
    indexer ! RemoteBlockApplied(secondHeader, history.getFullBlock(secondHeader).get.transactions.map(_.id))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 2
      state.globalTxIndex shouldBe expectedTxCount
      state.globalBoxIndex shouldBe expectedBoxCount
    }
    indexer ! Reset()
  }

  property("transactions") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val state = IndexerState.fromHistory(_history)
    cfor(0)(_ < state.globalTxIndex, _ + 1) { n =>
      val id = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoTransaction](id.get.m) shouldNot be(empty)
    }
    indexer ! Reset()
  }

  property("boxes") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val state = IndexerState.fromHistory(_history)
    cfor(0)(_ < state.globalBoxIndex, _ + 1) { n =>
      val id = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoBox](id.get.m) shouldNot be(empty)
    }
    indexer ! Reset()
  }

  property("addresses") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val (addresses, _, _, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses) shouldBe 0
    indexer ! Reset()
  }

  property("templates") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val (_, templates, _, _, _) = manualIndex(HEIGHT)
    checkTemplates(templates) shouldBe 0
    indexer ! Reset()
  }

  property("tokens") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val (_, _, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens) shouldBe 0
    indexer ! Reset()
  }

  property("required schema version is gated by the storage rent collection flag") {
    // the rescan decision in ErgoHistory.readOrGenerate: rescan happens only when the
    // stored schema is older than the required version, so:
    // - flag off: schema 6 (base, no rent index) is accepted as-is and NOT bumped,
    //   so enabling the flag later still triggers the rescan
    // - flag on: schema 6 forces a rescan to 7 (rent index needs historical rows)
    ExtraIndexer.requiredSchemaVersion(storageRentCollection = false) shouldBe ExtraIndexer.BaseVersion
    ExtraIndexer.requiredSchemaVersion(storageRentCollection = true) shouldBe ExtraIndexer.NewestVersion
    ExtraIndexer.BaseVersion should be < ExtraIndexer.NewestVersion
  }

  property("storage rent eligibility index") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    checkRentIndex()
    checkRentIndexAgainstChain(HEIGHT)
    indexer ! Reset()
  }

  property("rent index entries are removable by box id") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()

    val before = history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue)
    before.length should be > 2

    // remove the first two entries by box id, as CandidateGenerator does for a rejected claim
    val toRemove = before.take(2)
      .flatMap(e => NumericBoxIndex.getBoxByNumber(history, e.globalIndex)).map(_.id)
    toRemove.length shouldBe 2
    history.removeStorageRentBoxes(toRemove)

    val after = history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue)
    after.length shouldBe before.length - 2
    after.map(_.globalIndex).toSet shouldBe before.drop(2).map(_.globalIndex).toSet

    // removing again is a no-op: the boxes are still indexed, but the entries are gone
    history.removeStorageRentBoxes(toRemove)
    history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue).length shouldBe before.length - 2

    // removing an entry of a box which is not indexed at all does not corrupt the index
    history.removeStorageRentBoxes(Seq(bytesToId(Array.fill(32)(42.toByte))))
    history.storageRentBoxesAtOrBefore(Int.MaxValue, Int.MaxValue).length shouldBe before.length - 2

    indexer ! Reset()
  }

  property("rent index rows are not written when rent collection is off") {
    val noRentIndexer = system.actorOf(Props.create(classOf[ExtraIndexerTestActor], this, Boolean.box(false)))
    noRentIndexer ! CreateDB(HEIGHT)
    noRentIndexer ! Index()
    lock.lock()
    done.await()
    // the extra index is fully built, but no storage-rent rows are written
    history.storageRentBoxesAtOrBefore(Int.MaxValue, 1000) shouldBe empty
    noRentIndexer ! Reset()
  }

  property("rent index is trimmed to the unspent set after a rollback") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    checkRentIndexAgainstChain(HEIGHT)

    // rolling back discards the blocks that created some boxes, so those rows must go
    val back = BRANCHPOINT
    indexer ! Rollback(history.bestHeaderIdAtHeight(back).get)
    lock.lock()
    done.await()

    checkRentIndexAgainstChain(back)
    indexer ! Reset()
  }

  property("rent index stays correct across repeated rollbacks") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    checkRentIndexAgainstChain(HEIGHT)

    // each rollback must re-derive the rent rows of the surviving range only; rolling
    // forward again is separate (the chain generator cannot extend a rolled-back chain,
    // so this covers successive rollbacks, as rollbackWithPattern does)
    Seq(HEIGHT - 10, BRANCHPOINT, 8, 1).foreach { back =>
      indexer ! Rollback(history.bestHeaderIdAtHeight(back).get)
      lock.lock()
      done.await()
      checkRentIndexAgainstChain(back)
    }
    indexer ! Reset()
  }

  property("rent index survives rollback interleaved with generation") {
    // same rollback/generate interleaving as rollbackWithPattern, but the rent rows are
    // checked against chain-derived truth rather than indexer state
    rollbackWithPattern("G-30;R-20;G-35;R-30", checkChain = true)
  }



  property("alternating gens and rollbacks") {
    rollbackWithPattern("G-10;R-5;G-15;R-10;G-20;R-5")
  }

  property("multiple gens before rollback") {
    rollbackWithPattern("G-5;G-10;G-15;R-10;G-20;G-25;R-15")
  }

  property("consecutive rollbacks") {
    rollbackWithPattern("G-30;R-25;R-20;R-15;R-10;R-5")
  }

  property("rollback to 1") {
    rollbackWithPattern("G-10;G-20;G-30;R-10;G-35;R-1")
  }

  property("random gens and rollbacks") {
    rollbackWithPattern("G-5;G-15;R-5;G-20;G-25;R-15;G-30;R-10;G-50;R-25")
  }

  property("indexes replacement blocks after rolling back an orphan block") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    indexer ! GenerateBetterChainTip()
    lock.lock()
    created.await()
    val newBestHeaderOpt = history.typedModifierById[Header](history.headerIdsAtHeight(history.fullBlockHeight).last)
    indexer ! RemoteBlockApplied(newBestHeaderOpt.get, Seq.empty) // will be ignored
    indexer ! CreateDB(HEIGHT + 1)
    lock.lock()
    created.await()
    indexer ! Index()
    lock.lock()
    done.await()
    indexer ! Rollback(history.bestHeaderIdAtHeight(HEIGHT).get)
    lock.lock()
    done.await()
    val (_, _, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens) shouldBe 0
    indexer ! Reset()
  }

  property("resumes catch-up from a deferred state on FullBlockApplied") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)

    indexer ! ExtendDB(HEIGHT + 1)
    awaitCondition(created)
    val nextHeader = history.typedModifierById[Header](history.bestHeaderIdAtHeight(HEIGHT + 1).get).get

    // Simulate the state after a headers-only fork briefly became the best chain
    // and then lost: caughtUp=false, but the indexed tip is still on the main chain.
    indexer ! SetCaughtUp(caughtUp = false)

    // Without the FullBlockApplied handler for !caughtUp, this event would be
    // dropped and the indexer would stay stalled.
    indexer ! RemoteBlockApplied(nextHeader, history.getFullBlock(nextHeader).get.transactions.map(_.id))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 1
      state.caughtUp shouldBe true
    }
    indexer ! Reset()
  }
}
