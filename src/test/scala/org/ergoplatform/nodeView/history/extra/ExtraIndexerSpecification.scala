package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FullBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.Index
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, bytesToId}
import spire.implicits.cfor

import scala.collection.mutable
import scala.reflect.ClassTag

class ExtraIndexerSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder
  val initSettings: ErgoSettings = settings
  case class CreateDB()

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  val HEIGHT: Int = 50
  val BRANCHPOINT: Int = HEIGHT / 2
  implicit val segmentThreshold: Int = 8

  val system: ActorSystem = ActorSystem.create("indexer-test")
  val indexer: ActorRef = system.actorOf(Props.create(classOf[ExtraIndexerTestActor], this))

  var _history: ErgoHistory = _
  def history: ErgoHistoryReader = _history.getReader

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
            val token = IndexedToken.fromBox(new IndexedErgoBox(i, None, None, output, 0), j)
            val prev2 = indexedTokens.getOrElse(token.id, (0L, 0L))
            indexedTokens.put(token.id, (prev2._1 + 1, 0))
          }
        }
      }
    }
    (addresses, indexedTokens, txsIndexed, boxesIndexed)
  }

  def checkSegmentables[T <: Segment[_] : ClassTag](segmentables: ID_LL,
                                                    isChild: Boolean = false,
                                                    check: ((T, (Long, Long))) => Boolean
                                                   ): Int = {
    var errors: Int = 0
    segmentables.foreach { segmentable =>
      history.typedExtraIndexById[T](segmentable._1) match {
        case Some(obj: T) =>
          if (isChild) { // this is a segment
            // check tx segments
            val txSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            txSegments ++= (0 until obj.txSegmentCount).map(n => obj.idMod(txSegmentId(obj.parentId, n))).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(txSegments, isChild = true, check) shouldBe 0
            // check box segments
            val boxSegments: ID_LL = mutable.HashMap.empty[ModifierId, (Long, Long)]
            boxSegments ++= (0 until obj.boxSegmentCount).map(n => obj.idMod(boxSegmentId(obj.parentId, n))).map(Tuple2(_, (0L, 0L)))
            checkSegmentables(boxSegments, isChild = true, check) shouldBe 0
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

  def checkAddresses(addresses: ID_LL): Int =
    checkSegmentables[IndexedErgoAddress](addresses, isChild = false, seg => {
      seg._1.balanceInfo.get.nanoErgs == seg._2._1 && seg._1.balanceInfo.get.tokens.map(_._2).sum == seg._2._2
    })

  def checkTokens(indexedTokens: ID_LL): Int =
    checkSegmentables[IndexedToken](indexedTokens, isChild = false, seg => {
      seg._1.boxCount == seg._2._1
    })

  property("extra indexer transactions") {
    indexer ! CreateDB()
    indexer ! Index()
    Thread.sleep(5000)
    val state = IndexerState.fromHistory(_history)
    cfor(0)(_ < state.globalTxIndex, _ + 1) { n =>
      val id = history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoTransaction](id.get.m) shouldNot be(empty)
    }
  }

  property("extra indexer boxes") {
    indexer ! CreateDB()
    indexer ! Index()
    Thread.sleep(5000)
    val state = IndexerState.fromHistory(_history)
    cfor(0)(_ < state.globalBoxIndex, _ + 1) { n =>
      val id = history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(n)))
      id shouldNot be(empty)
      history.typedExtraIndexById[IndexedErgoBox](id.get.m) shouldNot be(empty)
    }
  }

  property("extra indexer addresses") {
    indexer ! CreateDB()
    indexer ! Index()
    Thread.sleep(5000)
    val (addresses, _, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses) shouldBe 0
  }

  property("extra indexer tokens") {
    indexer ! CreateDB()
    indexer ! Index()
    Thread.sleep(5000)
    val (_, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens) shouldBe 0
  }

  property("extra indexer rollback") {
    indexer ! CreateDB()
    indexer ! Index()
    Thread.sleep(5000)
    var state = IndexerState.fromHistory(_history)

    val txIndexBefore = state.globalTxIndex
    val boxIndexBefore = state.globalBoxIndex

    // manually count balances
    val (addresses, indexedTokens, txsIndexed, boxesIndexed) = manualIndex(BRANCHPOINT)

    // perform rollback
    indexer ! Rollback(history.bestHeaderIdAtHeight(BRANCHPOINT).get)
    Thread.sleep(5000)
    state = IndexerState.fromHistory(_history)

    // address balances
    checkAddresses(addresses) shouldBe 0

    addresses.keys.foreach { addr =>
      val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
        .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, false, Set.empty)
      utxos.exists(_.isSpent) shouldBe false
    }

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

    // -------------------------------------------------------------------
    // restart indexer to catch up
    cfor(BRANCHPOINT)(_ <= HEIGHT, _ + 1) { i =>
      indexer ! FullBlockApplied(history.bestHeaderAtHeight(i).get)
    }
    Thread.sleep(5000)
    state = IndexerState.fromHistory(_history)

    // Check addresses again
    val (addresses2, indexedTokens2, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses2) shouldBe 0
    checkTokens(indexedTokens2) shouldBe 0

    // check indexnumbers again
    state.globalTxIndex shouldBe txIndexBefore
    state.globalBoxIndex shouldBe boxIndexBefore

    // check txs after caught up
    cfor(0)(_ < txIndexBefore, _ + 1) { txNum =>
      history.typedExtraIndexById[NumericTxIndex](bytesToId(NumericTxIndex.indexToBytes(txNum))) shouldNot be(empty)
    }

    // check boxes after caught up
    cfor(0)(_ < boxIndexBefore, _ + 1) { boxNum =>
      history.typedExtraIndexById[NumericBoxIndex](bytesToId(NumericBoxIndex.indexToBytes(boxNum))) shouldNot be(empty)
    }

    addresses2.keys.foreach { addr =>
      val utxos = history.typedExtraIndexById[IndexedErgoAddress](addr).get
        .retrieveUtxos(history, ErgoMemPool.empty(settings), 0, 1000, SortDirection.ASC, false, Set.empty)
      utxos.exists(_.isSpent) shouldBe false
    }
  }
}
