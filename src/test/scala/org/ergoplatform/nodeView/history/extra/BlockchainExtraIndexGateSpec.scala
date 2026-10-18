package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.{VersionTag, idToVersion}
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.{BlockSection, NonHeaderBlockSection}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{DigestIndexProvenanceBytes, IndexModeProvenanceKey,
  IndexedHeaderIdKey, IndexedHeightKey, RollbackToKey}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.GetSegmentThreshold
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, ErgoStateReader, StateType}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADDigest
import scorex.util.{ModifierId, bytesToId}

import scala.util.Try

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

  it should "withhold Digest genesis reads until a checkpoint is persisted" in {
    val genesisSettings = indexedSettings.copy(directory = createTempDir.getAbsolutePath)
    genesisSettings.nodeSettings.verifyTransactions shouldBe true
    val genesisHistory = ErgoHistory.readOrGenerate(genesisSettings)(null)
    val genesisState = ErgoState.generateGenesisDigestState(createTempDir, genesisSettings)
    val readers = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetReaders => sender() ! Readers(genesisHistory, genesisState, memPool, wallet)
        case GetDataFromHistory(f) => sender() ! f(genesisHistory)
      }
    }))
    try {
      genesisHistory.bestHeaderOpt shouldBe None
      genesisState.version shouldBe ErgoState.genesisStateVersion
      genesisState.stateContext.lastHeaderOpt shouldBe None
      genesisState.fullBlockBodyFrontier shouldBe None
      ExtraIndexer.digestIndexHasProvenance(genesisHistory) shouldBe true
      ExtraIndexer.checkpointOnSelectedFullChain(genesisHistory) shouldBe true
      ExtraIndexer.checkpointOnCertifiedDigestFullChain(genesisHistory, genesisState,
        verifyTransactions = true) shouldBe false

      val genesisRoute = akka.http.scaladsl.server.Route.seal(
        BlockchainApiRoute(readers, genesisSettings, None).route)
      Get(boxPath) ~> genesisRoute ~> check {
        status shouldBe StatusCodes.ServiceUnavailable
        header("Retry-After").map(_.value()) shouldBe Some("1")
      }
    } finally {
      system.stop(readers)
      genesisState.close()
      genesisHistory.closeStorage()
    }
  }

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

  private def checkBoxRowAtRead(startRollbackOnRowRead: Boolean,
                                expectedStatus: StatusCode,
                                expectedRetryAfter: Option[String],
                                byIndex: Boolean = false): Unit = {
    val box = chain.last.blockTransactions.txs.head.outputs.head
    val indexedBox = new IndexedErgoBox(chain.last.header.height, None, None, None, box, 1L)
    val numericBoxIndex = NumericBoxIndex(1L, indexedBox.id)
    val originalStorage = history.historyStorage
    ExtraIndexer.getIndex(IndexedHeightKey, history).getInt shouldBe 0
    ExtraIndexer.getIndex(RollbackToKey, history).getInt shouldBe 0

    val rowReads = new AtomicInteger(0)
    val numericReads = new AtomicInteger(0)
    val controlledHistory = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage =
        new HistoryStorage(null, null, null, null) {
          override def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
            originalStorage.modifierBytesById(id)

          override def getExtraIndex(id: ModifierId): Option[ExtraIndex] = {
            if (byIndex && id == numericBoxIndex.id) {
              numericReads.incrementAndGet()
              Some(numericBoxIndex)
            } else if (id == indexedBox.id) {
              rowReads.incrementAndGet()
              if (startRollbackOnRowRead) {
                originalStorage.insertExtraTry(
                  Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty
                ).get
              }
              Some(indexedBox)
            } else {
              None
            }
          }
        }
      override protected val settings: ErgoSettings = indexedSettings
      override protected def requireProofs: Boolean = false
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
        throw new UnsupportedOperationException("Read-only route fixture")
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
        throw new UnsupportedOperationException("Read-only route fixture")
      override val powScheme: AutolykosPowScheme = indexedSettings.chainSettings.powScheme
    }
    val controlledReaders = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetDataFromHistory(f) => sender() ! f(controlledHistory)
      }
    }))
    val controlledRoute = BlockchainApiRoute(controlledReaders,
      indexedSettings.copy(nodeSettings = indexedSettings.nodeSettings.copy(stateType = StateType.Utxo)), None).route

    try {
      val path = if (byIndex) "/blockchain/box/byIndex/1" else "/blockchain/box/byId/" + indexedBox.id
      Get(path) ~> controlledRoute ~> check {
        numericReads.get() shouldBe (if (byIndex) 1 else 0)
        rowReads.get() shouldBe 1
        ExtraIndexer.getIndex(RollbackToKey, history).getInt shouldBe (if (startRollbackOnRowRead) 1 else 0)
        status shouldBe expectedStatus
        header("Retry-After").map(_.value()) shouldBe expectedRetryAfter
        if (!startRollbackOnRowRead) {
          response.entity.asInstanceOf[akka.http.scaladsl.model.HttpEntity.Strict]
            .data.utf8String should include("\"globalIndex\"")
        }
      }
    } finally {
      originalStorage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
      ).get
      system.stop(controlledReaders)
    }
  }

  it should "serve an indexed box row when rollback does not start after the history check" in {
    checkBoxRowAtRead(false, StatusCodes.OK, None)
  }

  it should "withhold an indexed box row if rollback starts after the history check" in {
    checkBoxRowAtRead(true, StatusCodes.ServiceUnavailable, Some("1"))
  }

  it should "serve an indexed box by index when rollback does not start after the history check" in {
    checkBoxRowAtRead(false, StatusCodes.OK, None, byIndex = true)
  }

  it should "withhold an indexed box by index if rollback starts on its box row" in {
    checkBoxRowAtRead(true, StatusCodes.ServiceUnavailable, Some("1"), byIndex = true)
  }

  private def checkTokenRowAtRead(startRollbackOnRowRead: Boolean,
                                  expectedStatus: StatusCode,
                                  expectedRetryAfter: Option[String]): Unit = {
    val box = chain.last.blockTransactions.txs.head.outputs.head
    val tokenId = bytesToId(chain.last.blockTransactions.txs.head.inputs.head.boxId)
    val indexedToken = IndexedToken(tokenId, Some(bytesToId(box.id)), Some(1L),
      Some("gate-token"), Some("row-read"), Some(0))
    val originalStorage = history.historyStorage
    ExtraIndexer.getIndex(IndexedHeightKey, history).getInt shouldBe 0
    ExtraIndexer.getIndex(RollbackToKey, history).getInt shouldBe 0

    val rowReads = new AtomicInteger(0)
    val controlledHistory = new ErgoHistoryReader {
      override protected[history] val historyStorage: HistoryStorage =
        new HistoryStorage(null, null, null, null) {
          override def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
            originalStorage.modifierBytesById(id)

          override def getExtraIndex(id: ModifierId): Option[ExtraIndex] = {
            if (id == indexedToken.id) {
              rowReads.incrementAndGet()
              if (startRollbackOnRowRead) {
                originalStorage.insertExtraTry(
                  Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty
                ).get
              }
              Some(indexedToken)
            } else {
              None
            }
          }
        }
      override protected val settings: ErgoSettings = indexedSettings
      override protected def requireProofs: Boolean = false
      override protected def process(m: NonHeaderBlockSection): Try[ProgressInfo[BlockSection]] =
        throw new UnsupportedOperationException("Read-only route fixture")
      override protected def validate(m: NonHeaderBlockSection): Try[Unit] =
        throw new UnsupportedOperationException("Read-only route fixture")
      override val powScheme: AutolykosPowScheme = indexedSettings.chainSettings.powScheme
    }
    val controlledReaders = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetDataFromHistory(f) => sender() ! f(controlledHistory)
      }
    }))
    val controlledRoute = BlockchainApiRoute(controlledReaders,
      indexedSettings.copy(nodeSettings = indexedSettings.nodeSettings.copy(stateType = StateType.Utxo)), None).route

    try {
      Get("/blockchain/token/byId/" + tokenId) ~> controlledRoute ~> check {
        rowReads.get() shouldBe 1
        ExtraIndexer.getIndex(RollbackToKey, history).getInt shouldBe (if (startRollbackOnRowRead) 1 else 0)
        status shouldBe expectedStatus
        header("Retry-After").map(_.value()) shouldBe expectedRetryAfter
        if (!startRollbackOnRowRead) {
          val body = response.entity.asInstanceOf[akka.http.scaladsl.model.HttpEntity.Strict]
            .data.utf8String
          body should include(tokenId)
          body should include("gate-token")
        }
      }
    } finally {
      originalStorage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
      ).get
      system.stop(controlledReaders)
    }
  }

  it should "serve an indexed token row when rollback does not start after the history check" in {
    checkTokenRowAtRead(false, StatusCodes.OK, None)
  }

  it should "withhold an indexed token row if rollback starts on its row" in {
    checkTokenRowAtRead(true, StatusCodes.ServiceUnavailable, Some("1"))
  }

  it should "serve an empty unspent token page with a certified Digest reader" in {
    Get("/blockchain/box/unspent/byTokenId/" + ("00" * 32)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      header("Retry-After") shouldBe None
      response.entity.asInstanceOf[akka.http.scaladsl.model.HttpEntity.Strict]
        .data.utf8String.replaceAll("\\s", "") shouldBe "[]"
    }
  }

  it should "resolve the segment threshold before holding the extra-index read lock" in {
    val storage = history.historyStorage
    val enteredThreshold = new CountDownLatch(1)
    val finishedWrite = new CountDownLatch(1)
    val failure = new AtomicReference[Throwable]()
    storage.insertExtraTry(
      Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
    ).get
    val thresholdActor = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetSegmentThreshold =>
          enteredThreshold.countDown()
          if (!finishedWrite.await(2, TimeUnit.SECONDS))
            throw new IllegalStateException("Extra-index write waited for the segment threshold")
          sender() ! 1
      }
    }))
    val writer = new Thread(new Runnable {
      override def run(): Unit = try {
        if (!enteredThreshold.await(2, TimeUnit.SECONDS))
          throw new IllegalStateException("Route did not request the segment threshold")
        storage.insertExtraTry(
          Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
        ).get
      } catch {
        case error: Throwable => failure.compareAndSet(null, error); ()
      } finally finishedWrite.countDown()
    })
    writer.setDaemon(true)
    writer.start()
    try {
      Get("/blockchain/box/byTokenId/" + ("00" * 32)) ~>
        BlockchainApiRoute(certifiedReadersRef, indexedSettings, Some(thresholdActor)).route ~> check {
          status shouldBe StatusCodes.OK
          header("Retry-After") shouldBe None
        }
      enteredThreshold.getCount shouldBe 0L
      finishedWrite.getCount shouldBe 0L
      failure.get() shouldBe null
    } finally {
      writer.join(10000)
      system.stop(thresholdActor)
      if (!writer.isAlive) storage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
      ).get
    }
    writer.isAlive shouldBe false
  }

  it should "let an extra-index write finish while an unspent request reads the mempool" in {
    val storage = history.historyStorage
    val transaction = chain.last.blockTransactions.txs.head
    val box = transaction.outputs.head
    val templateHash = IndexedContractTemplateSerializer.hashTreeTemplate(box.ergoTree)
    val testPool = ErgoMemPool.empty(settings).put(UnconfirmedTransaction(transaction, None))
    val enteredMempool = new CountDownLatch(1)
    val finishedWrite = new CountDownLatch(1)
    val failure = new AtomicReference[Throwable]()
    storage.insertExtraTry(
      Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
    ).get
    val blockingMempool = new ErgoMemPoolReader {
      override def contains(id: ModifierId): Boolean = testPool.contains(id)
      override def modifierById(id: ModifierId): Option[ErgoTransaction] = testPool.modifierById(id)
      override def unconfirmedById(id: ModifierId): Option[UnconfirmedTransaction] = testPool.unconfirmedById(id)
      override def getAll(ids: Seq[ModifierId]): Seq[UnconfirmedTransaction] = testPool.getAll(ids)
      override def getAll: Seq[UnconfirmedTransaction] = testPool.getAll
      override def getAllPrioritized: Seq[UnconfirmedTransaction] = testPool.getAllPrioritized
      override def size: Int = testPool.size
      override def take(limit: Int): Iterable[UnconfirmedTransaction] = testPool.take(limit)
      override def random(limit: Int): Iterable[UnconfirmedTransaction] = testPool.random(limit)
      override def weightedTransactionIds(limit: Int): Seq[WeightedTxId] = testPool.weightedTransactionIds(limit)
      override def getExpectedWaitTime(fee: Long, size: Int): Long = testPool.getExpectedWaitTime(fee, size)
      override def getRecommendedFee(minutes: Int, size: Int): Long = testPool.getRecommendedFee(minutes, size)
      override def spentInputs: Iterator[BoxId] = {
        enteredMempool.countDown()
        if (!finishedWrite.await(10, TimeUnit.SECONDS))
          throw new IllegalStateException("Extra-index write waited for the mempool read")
        testPool.spentInputs
      }
    }
    val controlledReaders = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetReaders => sender() ! readersWithFrontier(Some(chain.last.id)).copy(m = blockingMempool)
      }
    }))
    val controlledRoute = BlockchainApiRoute(controlledReaders, indexedSettings, None).route
    val writer = new Thread(new Runnable {
      override def run(): Unit = try {
        if (!enteredMempool.await(10, TimeUnit.SECONDS))
          throw new IllegalStateException("Unspent request did not read the mempool")
        storage.insertExtraTry(
          Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
        ).get
      } catch {
        case error: Throwable => failure.compareAndSet(null, error); ()
      } finally finishedWrite.countDown()
    })
    writer.setDaemon(true)
    writer.start()
    try {
      Get("/blockchain/box/unspent/byTemplateHash/" + templateHash +
        "?includeUnconfirmed=true&excludeMempoolSpent=true") ~>
        controlledRoute ~> check {
          status shouldBe StatusCodes.OK
          response.entity.asInstanceOf[akka.http.scaladsl.model.HttpEntity.Strict]
            .data.utf8String should include(bytesToId(box.id))
        }
      enteredMempool.getCount shouldBe 0L
      finishedWrite.getCount shouldBe 0L
      failure.get() shouldBe null
    } finally {
      writer.join(10000)
      system.stop(controlledReaders)
      if (!writer.isAlive) storage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
      ).get
    }
    writer.isAlive shouldBe false
  }

  it should "hold a same-storage rollback marker write until an extra-index read finishes" in {
    val storage = history.historyStorage
    val enteredRead = new CountDownLatch(1)
    val releaseRead = new CountDownLatch(1)
    val startedWrite = new CountDownLatch(1)
    val finishedWrite = new CountDownLatch(1)
    val failure = new AtomicReference[Throwable]()
    storage.insertExtraTry(
      Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
    ).get

    val reader = new Thread(new Runnable {
      override def run(): Unit = try {
        storage.withExtraIndexReadLock {
          enteredRead.countDown()
          if (!releaseRead.await(10, TimeUnit.SECONDS))
            throw new IllegalStateException("Timed out holding the extra-index read lock")
        }
      } catch {
        case error: Throwable => failure.compareAndSet(null, error); ()
      }
    })
    val writer = new Thread(new Runnable {
      override def run(): Unit = try {
        if (!enteredRead.await(10, TimeUnit.SECONDS))
          throw new IllegalStateException("Timed out waiting for the extra-index read")
        startedWrite.countDown()
        storage.insertExtraTry(
          Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty
        ).get
      } catch {
        case error: Throwable => failure.compareAndSet(null, error); ()
      } finally finishedWrite.countDown()
    })
    reader.setDaemon(true)
    writer.setDaemon(true)
    reader.start()
    try {
      enteredRead.await(10, TimeUnit.SECONDS) shouldBe true
      writer.start()
      startedWrite.await(10, TimeUnit.SECONDS) shouldBe true
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while (writer.isAlive && writer.getState != Thread.State.WAITING && System.nanoTime() < deadline)
        Thread.sleep(1)
      writer.getState shouldBe Thread.State.WAITING
      finishedWrite.getCount shouldBe 1L
      ExtraIndexer.getIndex(RollbackToKey, storage).getInt shouldBe 0
    } finally {
      releaseRead.countDown()
      reader.join(10000)
      if (writer.getState != Thread.State.NEW) writer.join(10000)
      if (!reader.isAlive && !writer.isAlive) storage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty
      ).get
    }
    reader.isAlive shouldBe false
    writer.isAlive shouldBe false
    failure.get() shouldBe null
    finishedWrite.getCount shouldBe 0L
  }
}
