package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{IndexedHeaderIdKey, IndexedHeightKey, RollbackToKey}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer
import scorex.util.bytesToId

class BlockchainExtraIndexGateSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val indexedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true))
  private val route = BlockchainApiRoute(digestReadersRef, indexedSettings, None).route
  private val txPath = "/blockchain/transaction/byId/" + ("00" * 32)
  private val boxPath = "/blockchain/box/byId/" + ("00" * 32)

  it should "withhold extra-index rows while rollback recovery is incomplete" in {
    Get(txPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    Get(boxPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    history.historyStorage.insertExtraTry(
      Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty).get
    try {
      Get("/blockchain/indexedHeight") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
      Get(txPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
      Get(boxPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
    } finally {
      history.historyStorage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty).get
    }
    Get(txPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "withhold extra-index rows when their checkpoint leaves the selected full chain" in {
    val selected = chain.last.header
    val orphan = selected.copy(timestamp = selected.timestamp + 1)
    history.historyStorage.insert(
      Array(history.validityKey(orphan.id) -> Array[Byte](1)),
      Array[org.ergoplatform.modifiers.BlockSection](orphan)
    ).get
    FullBlockProcessor.isInBestFullChain(history.historyStorage, orphan.id) shouldBe false
    history.historyStorage.insertExtraTry(
      Array(
        IndexedHeightKey -> ByteBuffer.allocate(4).putInt(selected.height).array,
        IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(orphan.id)
      ), Array.empty).get
    try {
      Get("/blockchain/indexedHeight") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
      Get(txPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
      Get(boxPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
    } finally {
      history.historyStorage.removeExtraTry(Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))).get
      history.historyStorage.remove(Array(history.validityKey(orphan.id)), Array(orphan.id)).get
    }
  }

  it should "not mix a fork header with transactions indexed for another block" in {
    val selected = chain.last
    val forkHeader = selected.header.copy(timestamp = selected.header.timestamp + 1)
    val forkTransactions = selected.blockTransactions.copy(headerId = forkHeader.id)
    forkTransactions.id shouldBe forkHeader.transactionsId
    val indexedRows = selected.blockTransactions.txs.zipWithIndex.map { case (tx, index) =>
      IndexedErgoTransaction.fromTx(
        tx, index, selected.header.height, index.toLong,
        Array.empty[Long], Array.empty[Long], selected.id
      )
    }
    history.historyStorage.insert(
      Array(history.validityKey(forkHeader.id) -> Array[Byte](1)),
      Array[org.ergoplatform.modifiers.BlockSection](forkHeader, forkTransactions)
    ).get
    history.historyStorage.insertExtraTry(
      Array(
        IndexedHeightKey -> ByteBuffer.allocate(4).putInt(selected.header.height).array,
        IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(selected.id)
      ), indexedRows.toArray[ExtraIndex]
    ).get
    try {
      Get("/blockchain/block/byHeaderId/" + selected.id) ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
      Get("/blockchain/block/byHeaderId/" + forkHeader.encodedId) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    } finally {
      history.historyStorage.removeExtraTry(
        indexedRows.map(_.id).toArray ++ Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))
      ).get
      history.historyStorage.remove(Array(history.validityKey(forkHeader.id)), Array(forkTransactions.id, forkHeader.id)).get
    }
  }

  it should "recheck rollback state when data handlers reacquire readers" in {
    val tokenPath = "/blockchain/box/unspent/byTokenId/" + ("00" * 32)
    Seq(txPath, tokenPath).foreach { path =>
      val switchingReaders = system.actorOf(Props(new Actor {
        private var readerRequests = 0

        private def recordRequest(): Unit = {
          readerRequests += 1
          if (readerRequests == 2) {
            history.historyStorage.insertExtraTry(
              Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty
            ).get
          }
        }

        override def receive: Receive = {
          case GetDataFromHistory(f) =>
            recordRequest()
            sender() ! f(history)
          case GetReaders =>
            recordRequest()
            sender() ! digestReaders
        }
      }))
      val switchingRoute = BlockchainApiRoute(switchingReaders, indexedSettings, None).route
      try {
        Get(path) ~> switchingRoute ~> check {
          status shouldBe StatusCodes.InternalServerError
        }
      } finally {
        history.historyStorage.insertExtraTry(
          Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
        ).get
        system.stop(switchingReaders)
      }
    }
  }
}
