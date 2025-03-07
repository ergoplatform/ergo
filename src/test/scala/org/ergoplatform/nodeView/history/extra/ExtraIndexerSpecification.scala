package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.Rollback
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.Index
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
import scala.reflect.ClassTag

class ExtraIndexerSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder
  val initSettings: ErgoSettings = settings
  case class CreateDB(blockCount: Int)
  case class Reset()

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  val HEIGHT: Int = 50
  val BRANCHPOINT: Int = HEIGHT / 2
  implicit val segmentThreshold: Int = 8

  val system: ActorSystem = ActorSystem.create("indexer-test")
  val indexer: ActorRef = system.actorOf(Props.create(classOf[ExtraIndexerTestActor], this))

  var _history: ErgoHistory = _
  def history: ErgoHistoryReader = _history.getReader

  val lock: ReentrantLock = new ReentrantLock()
  val done: Condition = lock.newCondition()

  def manualIndex(limit: Int): (ID_LL, // address -> (erg,tokenSum)
                                ID_LL, // tokenId -> (boxesCount,_)
                                Int, // txs indexed
                                Int) = { // boxes indexed
    var txsIndexed = 0
    var boxesIndexed = 0
    val addresses: ID_LL = mutable.HashMap[ModifierId, (Long, Long)]()
    val indexedTokens: ID_LL = mutable.HashMap[ModifierId, (Long, Long)]()
    cfor(1)(_ <= limit, _ + 1) { i =>
      _history.getReader.bestBlockTransactionsAt(i).get.txs.foreach { tx =>
        txsIndexed += 1
        if (i != 1) {
          tx.inputs.foreach { input =>
            val iEb: IndexedErgoBox = _history.getReader.typedExtraIndexById[IndexedErgoBox](bytesToId(input.boxId)).get
            val address = hashErgoTree(ExtraIndexer.getAddress(iEb.box.ergoTree)(addressEncoder).script)
            val prev = addresses(address)
            addresses.put(address, (prev._1 - iEb.box.value, prev._2 - iEb.box.additionalTokens.toArray.map(_._2).sum))
          }
        }
        tx.outputs.foreach { output =>
          boxesIndexed += 1
          val address = hashErgoTree(addressEncoder.fromProposition(output.ergoTree).get.script)
          val prev = addresses.getOrElse(address, (0L, 0L))
          addresses.put(address, (prev._1 + output.value, prev._2 + output.additionalTokens.toArray.map(_._2).sum))
          cfor(0)(_ < output.additionalTokens.length, _ + 1) { j =>
            val token = IndexedToken.fromBox(new IndexedErgoBox(i, None, None, None, output, 0), j)
            val prev2 = indexedTokens.getOrElse(token.id, (0L, 0L))
            indexedTokens.put(token.id, (prev2._1 + 1, 0))
          }
        }
      }
    }
    (addresses, indexedTokens, txsIndexed, boxesIndexed)
  }

  def checkSegmentables[T <: Segment[T] : ClassTag](segmentables: ID_LL,
                                                    isChild: Boolean = false,
                                                    check: ((T, (Long, Long))) => Boolean,
                                                    height: Int): Int = {
    var errors: Int = 0
    segmentables.foreach { segmentable =>
      history.typedExtraIndexById[T](segmentable._1) match {
        case Some(obj: T) =>
          if (isChild) { // this is a segment
            // check tx segments
            val txSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            txSegments ++= (0 until obj.txSegmentCount).map(n => obj.factory(txSegmentId(obj.id, n)).id).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(txSegments, isChild = true, check, height) shouldBe 0
            // check box segments
            val boxSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            boxSegments ++= (0 until obj.boxSegmentCount).map(n => obj.factory(boxSegmentId(obj.id, n)).id).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(boxSegments, isChild = true, check, height) shouldBe 0
          } else { // this is the parent object
            // check properties of object
            if (!check((obj, segmentable._2)))
              errors += 1
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
          }
        case None =>
          System.err.println(s"Segmentable object ${segmentable._1} should exist, but was not found")
          errors += 1
      }
    }
    errors
  }

  def checkAddresses(addresses: ID_LL, height: Int): Int =
    checkSegmentables[IndexedErgoAddress](addresses, isChild = false, seg => {
      seg._1.balanceInfo.get.nanoErgs == seg._2._1 && seg._1.balanceInfo.get.tokens.map(_._2).sum == seg._2._2
    }, height)

  def checkTokens(indexedTokens: ID_LL, height: Int): Int =
    checkSegmentables[IndexedToken](indexedTokens, isChild = false, seg => {
      seg._1.boxCount == seg._2._1
    }, height)

  // example G-30;R-20;G-35;R-30
  def rollbackWithPattern(pattern: String): Unit = {

    def rollback(n: Int): Unit = {
      println(s"Rollback to $n")
      var state = IndexerState.fromHistory(_history)

      val txIndexBefore = state.globalTxIndex
      val boxIndexBefore = state.globalBoxIndex

      // manually count balances
      val (addresses, indexedTokens, txsIndexed, boxesIndexed) = manualIndex(n)

      // perform rollback
      indexer ! Rollback(history.bestHeaderIdAtHeight(n).get)
      lock.lock()
      done.await()
      state = IndexerState.fromHistory(_history)

      // address balances
      checkAddresses(addresses, n) shouldBe 0

      addresses.keys.foreach { addr =>
        val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
          .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, unconfirmed = false, Set.empty)
        utxos.exists(_.isSpent) shouldBe false
      }

      // token indexes
      checkTokens(indexedTokens, n) shouldBe 0

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
    }

    def generate(n: Int): Unit = {
      println(s"Generate to $n")
      indexer ! CreateDB(n)
      indexer ! Index()
      lock.lock()
      done.await()

      val (addresses, _, _, _) = manualIndex(n)

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

    }

    pattern.split(";").map(_.split("-")).map(x => x(0) -> x(1).toInt).foreach {
      case ("G", n) => generate(n)
      case ("R", n) => rollback(n)
      case _ => System.err.println(s"Malformed rollback pattern: $pattern")
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
    val (addresses, _, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses, HEIGHT) shouldBe 0
    indexer ! Reset()
  }

  property("tokens") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    lock.lock()
    done.await()
    val (_, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens, HEIGHT) shouldBe 0
    indexer ! Reset()
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
}
