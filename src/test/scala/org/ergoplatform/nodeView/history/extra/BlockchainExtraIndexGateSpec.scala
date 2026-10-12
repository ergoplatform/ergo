package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.core.{VersionTag, idToVersion}
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{DigestIndexProvenanceBytes, IndexModeProvenanceKey,
  IndexedHeaderIdKey, IndexedHeightKey, RollbackToKey}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, StateType}
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer
import scorex.crypto.authds.ADDigest
import scorex.util.{ModifierId, bytesToId}

class BlockchainExtraIndexGateSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val indexedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
    stateType = StateType.Digest, extraIndex = true))
  private def readersWithFrontier(frontier: Option[ModifierId],
                                  stateHeader: Header = chain.last.header,
                                  stateVersion: VersionTag = idToVersion(chain.last.id),
                                  stateRoot: ADDigest = chain.last.header.stateRoot): Readers = digestReaders.copy(s = new ErgoStateReader {
    override val rootDigest = stateRoot
    override val version = stateVersion
    override val store = digestState.store
    override protected val ergoSettings = indexedSettings
    override val fullBlockBodyFrontier = frontier
    override def stateContext: ErgoStateContext = {
      val base = digestState.stateContext
      new ErgoStateContext(Seq(stateHeader), None, base.genesisStateDigest,
        base.currentParameters, base.validationSettings, base.votingData)(indexedSettings.chainSettings)
    }
  })

  private def readersRefWithFrontier(frontier: Option[ModifierId],
                                     stateHeader: Header = chain.last.header,
                                     stateVersion: VersionTag = idToVersion(chain.last.id),
                                     stateRoot: ADDigest = chain.last.header.stateRoot): ActorRef = system.actorOf(Props(new Actor {
    override def receive: Receive = {
      case GetDataFromHistory(f) => sender() ! f(history)
      case GetReaders => sender() ! readersWithFrontier(frontier, stateHeader, stateVersion, stateRoot)
    }
  }))

  private val certifiedReadersRef = readersRefWithFrontier(Some(chain.last.id))
  private val route = {
    // Stubs builds history without extra-index initialization; model a freshly rebuilt Digest index.
    history.historyStorage.insertExtraTry(
      Array(IndexModeProvenanceKey -> DigestIndexProvenanceBytes), Array.empty).get
    BlockchainApiRoute(certifiedReadersRef, indexedSettings, None).route
  }
  private val txPath = "/blockchain/transaction/byId/" + ("00" * 32)
  private val boxPath = "/blockchain/box/byId/" + ("00" * 32)

  it should "keep disabled indexing distinct from temporary checkpoint recovery" in {
    val disabledRoute = BlockchainApiRoute(digestReadersRef,
      indexedSettings.copy(nodeSettings = indexedSettings.nodeSettings.copy(extraIndex = false)), None).route
    Get(txPath) ~> disabledRoute ~> check {
      status shouldBe StatusCodes.InternalServerError
      header("Retry-After") shouldBe None
    }
  }

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
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
      }
      Get(boxPath) ~> route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
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
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
      }
      Get(boxPath) ~> route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
      }
    } finally {
      history.historyStorage.removeExtraTry(Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))).get
      history.historyStorage.remove(Array(history.validityKey(orphan.id)), Array(orphan.id)).get
    }
  }

  it should "deny Digest rows before the indexer starts when provenance or a body certificate is absent" in {
    val selected = chain.last.header
    history.historyStorage.insertExtraTry(Array(
      IndexedHeightKey -> ByteBuffer.allocate(4).putInt(selected.height).array,
      IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(selected.id)
    ), Array.empty).get
    history.historyStorage.removeExtraTry(Array(bytesToId(IndexModeProvenanceKey))).get
    try {
      Get(txPath) ~> route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
      val utxoRoute = BlockchainApiRoute(utxoReadersRef,
        indexedSettings.copy(nodeSettings = indexedSettings.nodeSettings.copy(stateType = StateType.Utxo)), None).route
      Get(txPath) ~> utxoRoute ~> check {
        status shouldBe StatusCodes.NotFound
      }
    } finally {
      history.historyStorage.removeExtraTry(Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))).get
      history.historyStorage.insertExtraTry(
        Array(IndexModeProvenanceKey -> DigestIndexProvenanceBytes), Array.empty).get
    }
    val noCertificate = readersRefWithFrontier(None)
    try {
      Get(txPath) ~> BlockchainApiRoute(noCertificate, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
    } finally system.stop(noCertificate)
  }

  it should "reject a same-height certificate from a different full-chain fork" in {
    val selected = chain.last.header
    val other = selected.copy(timestamp = selected.timestamp + 1)
    history.historyStorage.insert(
      Array(history.validityKey(other.id) -> Array[Byte](1)),
      Array[org.ergoplatform.modifiers.BlockSection](other)
    ).get
    history.historyStorage.insertExtraTry(Array(
      IndexedHeightKey -> ByteBuffer.allocate(4).putInt(selected.height).array,
      IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(selected.id)
    ), Array.empty).get
    val otherReaders = readersRefWithFrontier(Some(other.id))
    try {
      Get(txPath) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
      Get(txPath) ~> BlockchainApiRoute(otherReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
    } finally {
      system.stop(otherReaders)
      history.historyStorage.removeExtraTry(Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))).get
      history.historyStorage.remove(Array(history.validityKey(other.id)), Array(other.id)).get
    }
  }

  it should "allow a selected state header behind the best tip with an older body frontier" in {
    val frontier = chain(3).header
    val stateHeader = chain(4).header
    history.historyStorage.insertExtraTry(Array(
      IndexedHeightKey -> ByteBuffer.allocate(4).putInt(frontier.height).array,
      IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(frontier.id)
    ), Array.empty).get
    val laggingReaders = readersRefWithFrontier(Some(frontier.id), stateHeader,
      idToVersion(stateHeader.id), stateHeader.stateRoot)
    try {
      Get(txPath) ~> BlockchainApiRoute(laggingReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    } finally {
      system.stop(laggingReaders)
      history.historyStorage.removeExtraTry(Array(bytesToId(IndexedHeightKey), bytesToId(IndexedHeaderIdKey))).get
    }
  }

  it should "withhold rows for a stale context despite a state version at the selected tip" in {
    val staleHeader = chain(4).header
    val staleReaders = readersRefWithFrontier(Some(staleHeader.id), staleHeader,
      idToVersion(chain.last.id), chain.last.header.stateRoot)
    try {
      Get(txPath) ~> BlockchainApiRoute(staleReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
    } finally system.stop(staleReaders)
  }

  it should "withhold rows when the state root disagrees with its context header" in {
    val contextHeader = chain.last.header
    val wrongRoot = contextHeader.stateRoot.clone()
    wrongRoot(0) = (wrongRoot(0) ^ 1).toByte
    val rootMismatchReaders = readersRefWithFrontier(Some(chain(4).id), contextHeader,
      idToVersion(contextHeader.id), ADDigest @@ wrongRoot)
    try {
      Get(txPath) ~> BlockchainApiRoute(rootMismatchReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
    } finally system.stop(rootMismatchReaders)
  }

  it should "withhold rows when the body frontier is beyond the state context" in {
    val contextHeader = chain(4).header
    val wrongFrontierReaders = readersRefWithFrontier(Some(chain.last.id), contextHeader,
      idToVersion(contextHeader.id), contextHeader.stateRoot)
    try {
      Get(txPath) ~> BlockchainApiRoute(wrongFrontierReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
      }
    } finally system.stop(wrongFrontierReaders)
  }

  it should "withhold Digest rows when transaction verification is disabled" in {
    val verificationOffSettings = indexedSettings.copy(nodeSettings =
      indexedSettings.nodeSettings.copy(verifyTransactions = false))
    Get(txPath) ~> BlockchainApiRoute(certifiedReadersRef, verificationOffSettings, None).route ~> check {
      status shouldBe StatusCodes.ServiceUnavailable
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
            sender() ! readersWithFrontier(Some(chain.last.id))
        }
      }))
      val switchingRoute = BlockchainApiRoute(switchingReaders, indexedSettings, None).route
      try {
        Get(path) ~> switchingRoute ~> check {
          status shouldBe StatusCodes.ServiceUnavailable
          header("Retry-After").map(_.value()) shouldBe Some("1")
        }
      } finally {
        history.historyStorage.insertExtraTry(
          Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
        ).get
        system.stop(switchingReaders)
      }
    }
  }

  it should "recheck the Digest certificate when a data handler reacquires readers" in {
    val switchingReaders = system.actorOf(Props(new Actor {
      private var readerRequests = 0
      override def receive: Receive = {
        case GetReaders =>
          readerRequests += 1
          sender() ! readersWithFrontier(if (readerRequests == 1) Some(chain.last.id) else None)
      }
    }))
    try {
      Get(txPath) ~> BlockchainApiRoute(switchingReaders, indexedSettings, None).route ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
      }
    } finally system.stop(switchingReaders)
  }
}
