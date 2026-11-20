package org.ergoplatform.mining

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.ByteBuffer

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, ErgoStateContext, UtxoState, VotingData}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.crypto.authds.ADKey
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigmastate.helpers.TestingHelpers._

import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
import scala.concurrent.duration._

/** A rejected batch claim must not erase eligibility of independent unspent boxes. */
class CandidateRentRejectIndexSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  property("actor rotates retained rent rows across forced candidate regeneration") {
    new TestKit(ActorSystem()) {
      val height = 3 * Constants.StoragePeriod
      val threshold = height - Constants.StoragePeriod
      val eligibleBoxes = (0 to StorageRentClaimBuilder.MaxClaims).map { index =>
        testBox(10000000000L + index, Constants.TrueTree, threshold, Seq.empty, Map.empty)
      }
      val independentBox = testBox(12000000000L, Constants.TrueTree, height, Seq.empty, Map.empty)
      val fixtureSettings = baseSettings.copy(
        directory = createTempDir.getAbsolutePath,
        nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
      val firstGlobalIndex = ErgoState.genesisBoxes(fixtureSettings.chainSettings).size.toLong + 1L
      val baseState = UtxoState.fromBoxHolder(BoxHolder(eligibleBoxes :+ independentBox), None,
        createTempDir, fixtureSettings, parameters)
      val block = invalidErgoFullBlockGen.sample.get
      val parentHeader = block.header.copy(height = height - 1)
      val parentBlock = block.copy(parentHeader)
      val agedContext = new ErgoStateContext(Seq(parentHeader), None,
        genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
      val state = new UtxoState(baseState.persistentProver, baseState.version,
        baseState.store, fixtureSettings) {
        override def stateContext: ErgoStateContext = agedContext
        override def emissionBoxOpt: Option[ErgoBox] = None
      }
      val storage = HistoryStorage(fixtureSettings)
      eligibleBoxes.zipWithIndex.foreach { case (box, index) =>
        val globalIndex = firstGlobalIndex + index
        val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
        storage.insertExtra(Array.empty[(Array[Byte], Array[Byte])],
          Array[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed)))
      }
      storage.insertExtra(Array(
        ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
        ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + eligibleBoxes.size).array
      ), Array.empty[ExtraIndex])
      val history = Proxy.newProxyInstance(
        classOf[ErgoHistoryReader].getClassLoader,
        Array[Class[_]](classOf[ErgoHistoryReader]),
        new InvocationHandler {
          override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
            method.getName match {
              case "lastHeaders$default$2" => Int.box(0)
              case "lastHeaders" => HeaderChain.empty
              case "bestFullBlockOpt" => Some(parentBlock)
              case "fullBlockHeight" => Int.box(parentHeader.height)
              case "historyStorage" => storage
              case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
              case "storageRentScanPage" =>
                storage.storageRentScanPage(args(0).asInstanceOf[Int], args(1).asInstanceOf[Long],
                  args(2).asInstanceOf[Option[Array[Byte]]], args(3).asInstanceOf[Int])
              case "storageRentBoxesAtOrBefore" =>
                storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int],
                  args(1).asInstanceOf[Int], args(2).asInstanceOf[Option[StorageRentBox]])
              case "removeStorageRentBoxes" => fail("candidate generation must retain unspent rent rows")
              case "typedModifierById" => None
              case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
              case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
            }
        }
      ).asInstanceOf[ErgoHistoryReader]

      try {
        val readersProbe = TestProbe()
        val viewHolderProbe = TestProbe()
        val replyProbe = TestProbe()
        val wallet = new ErgoWalletReader {
          val walletActor: ActorRef = system.deadLetters
        }
        val generator = CandidateGenerator(defaultMinerPk, readersProbe.ref, viewHolderProbe.ref, fixtureSettings)
        readersProbe.expectMsg(GetReaders)
        // The actor's automatic empty request cannot scan: there is no mempool or emission transaction.
        readersProbe.reply(Readers(history, state, ErgoMemPool.empty(fixtureSettings), wallet))
        val conflictingSpend = ErgoTransaction(
          IndexedSeq(Input(eligibleBoxes.head.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(eligibleBoxes.head.value, Constants.TrueTree, height)))
        val independentSpend = ErgoTransaction(
          IndexedSeq(Input(independentBox.id, emptyProverResult)), IndexedSeq.empty,
          IndexedSeq(new ErgoBoxCandidate(independentBox.value, Constants.TrueTree, height)))
        val firstClaim = StorageRentClaimBuilder.buildClaim(eligibleBoxes.take(StorageRentClaimBuilder.MaxClaims).iterator,
          height, parameters, defaultMinerPk, None, Set.empty[ModifierId]).get
        val laterClaim = StorageRentClaimBuilder.buildClaim(Iterator(eligibleBoxes.last),
          height, parameters, defaultMinerPk, None, Set.empty[ModifierId]).get
        def nextCandidate(prioritized: ErgoTransaction): Candidate = {
          generator.tell(GenerateCandidate(Seq(prioritized), reply = true, forced = true), replyProbe.ref)
          replyProbe.expectMsgPF(10.seconds) {
            case StatusReply.Success(candidate: Candidate) => candidate
          }
        }
        val first = nextCandidate(conflictingSpend)
        first.candidateBlock.transactions.map(_.id) should contain(conflictingSpend.id)
        first.candidateBlock.transactions.map(_.id) should not contain firstClaim.id
        first.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))) should not contain
          bytesToId(eligibleBoxes.last.id)
        viewHolderProbe.expectMsgType[EliminateTransactions](10.seconds).ids should contain(firstClaim.id)
        val second = nextCandidate(independentSpend)
        second.candidateBlock.transactions.map(_.id) should contain(laterClaim.id)
        val third = nextCandidate(independentSpend)
        third.candidateBlock.transactions.map(_.id) should contain(firstClaim.id)
        third.candidateBlock.transactions.map(_.id) should not contain laterClaim.id
        val fourth = nextCandidate(independentSpend)
        fourth.candidateBlock.transactions.map(_.id) should contain(laterClaim.id)
        // No candidate was applied: all 101 persistent rows and UTXOs remain available.
        storage.storageRentBoxesAtOrBefore(threshold, 4 * StorageRentClaimBuilder.MaxClaims)
          .map(_.globalIndex) shouldBe (firstGlobalIndex to firstGlobalIndex + StorageRentClaimBuilder.MaxClaims)
        eligibleBoxes.foreach(box => state.boxById(box.id) shouldBe Some(box))
      } finally {
        TestKit.shutdownActorSystem(system)
        storage.close()
        state.closeStorage()
      }
    }
  }

  property("prioritized owner spend does not drop other boxes in a rejected rent claim") {
    val height = 3 * Constants.StoragePeriod
    val spentByOwner = testBox(10000000000L, Constants.TrueTree,
      height - Constants.StoragePeriod, Seq.empty, Map.empty)
    val stillUnspent = testBox(11000000000L, Constants.TrueTree,
      height - Constants.StoragePeriod, Seq.empty, Map.empty)
    val freshBox = testBox(12000000000L, Constants.TrueTree,
      height - 1, Seq.empty, Map.empty)
    val fixtureSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val firstGlobalIndex = ErgoState.genesisBoxes(fixtureSettings.chainSettings).size.toLong + 1L

    val baseState = UtxoState.fromBoxHolder(BoxHolder(Seq(spentByOwner, stillUnspent, freshBox)), None,
      createTempDir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    val parentHeader = block.header.copy(height = height - 1)
    val parentBlock = block.copy(parentHeader)
    val agedContext = new ErgoStateContext(Seq(parentHeader), None,
      genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext = agedContext
      override def emissionBoxOpt: Option[ErgoBox] = None
    }

    val storage = HistoryStorage(fixtureSettings)
    Seq(spentByOwner, stillUnspent).zipWithIndex.foreach { case (box, index) =>
      val globalIndex = firstGlobalIndex + index
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
      storage.insertExtra(Array.empty[(Array[Byte], Array[Byte])],
        Array[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed)))
    }

    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + 2L).array
    ), Array.empty[ExtraIndex])

    val history = Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "bestFullBlockOpt" => Some(parentBlock)
            case "fullBlockHeight" => Int.box(parentHeader.height)
            case "historyStorage" => storage
            case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "storageRentScanPage" =>
              storage.storageRentScanPage(args(0).asInstanceOf[Int], args(1).asInstanceOf[Long],
                args(2).asInstanceOf[Option[Array[Byte]]], args(3).asInstanceOf[Int])
            case "storageRentBoxesAtOrBefore" =>
              storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int], args(1).asInstanceOf[Int],
                if (args.length > 2) args(2).asInstanceOf[Option[StorageRentBox]] else None)
            case "removeStorageRentBoxes" =>
              storage.removeStorageRentBoxes(args(0).asInstanceOf[Seq[ModifierId]])
              ().asInstanceOf[AnyRef]
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      val threshold = height - Constants.StoragePeriod
      val before = history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
      before.map(_.globalIndex).toSet shouldBe Set(firstGlobalIndex, firstGlobalIndex + 1L)
      val batch = StorageRentClaimBuilder.buildClaim(Iterator(spentByOwner, stillUnspent),
        height, parameters, defaultMinerPk, None, Set.empty[ModifierId]).get
      batch.inputs.map(in => bytesToId(in.boxId)).toSet shouldBe
        Set(bytesToId(spentByOwner.id), bytesToId(stillUnspent.id))

      val ownerSpend = ErgoTransaction(
        IndexedSeq(Input(spentByOwner.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(spentByOwner.value, Constants.TrueTree, height)))
      val result = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(ownerSpend), None, fixtureSettings)
      result should not be None
      result.get.isSuccess shouldBe true
      val (candidate, rejected) = result.get.get
      candidate.candidateBlock.transactions.map(_.id) should contain(ownerSpend.id)
      candidate.candidateBlock.transactions.map(_.id) should not contain batch.id
      rejected.ids should contain(batch.id)

      // A was spent only in this candidate; B was never spent at all. It must remain
      // available for the next candidate even when the combined claim was rejected.
      state.boxById(ADKey @@ idToBytes(bytesToId(stillUnspent.id))) shouldBe Some(stillUnspent)
      history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex).toSet shouldBe Set(firstGlobalIndex, firstGlobalIndex + 1L)

      // Neither candidate has been applied. With the conflict gone, the ordinary next
      // generation must rediscover and include the claim without repairing the index.
      // An independent young-box spend satisfies the existing nonempty-candidate gate.
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, height)))
      val nextResult = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend), None, fixtureSettings)
      val (nextCandidate, nextRejected) = nextResult.get.get
      nextCandidate.candidateBlock.transactions.map(_.id) should contain(batch.id)
      nextRejected.ids should not contain batch.id
      state.boxById(spentByOwner.id) shouldBe Some(spentByOwner)
      state.boxById(stillUnspent.id) shouldBe Some(stillUnspent)
      history.storageRentBoxesAtOrBefore(threshold, StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex).toSet shouldBe Set(firstGlobalIndex, firstGlobalIndex + 1L)
    } finally {
      storage.close()
      state.closeStorage()
    }
  }

  property("advances beyond a full window of unresolved rent rows without deleting them") {
    val height = 3 * Constants.StoragePeriod
    val threshold = height - Constants.StoragePeriod
    val scanLimit = 4 * StorageRentClaimBuilder.MaxClaims
    val unresolvedCount = scanLimit + 5
    val laterBox = testBox(11000000000L, Constants.TrueTree, threshold, Seq.empty, Map.empty)
    val freshBox = testBox(12000000000L, Constants.TrueTree, height - 1, Seq.empty, Map.empty)
    val restoredBox = testBox(13000000000L, Constants.TrueTree, threshold - 1, Seq.empty, Map.empty)
    val fixtureSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val firstGlobalIndex = ErgoState.genesisBoxes(fixtureSettings.chainSettings).size.toLong + 1L
    val baseState = UtxoState.fromBoxHolder(BoxHolder(Seq(laterBox, freshBox, restoredBox)), None,
      createTempDir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    val parentHeader = block.header.copy(height = height - 1)
    val parentBlock = block.copy(parentHeader)
    val agedContext = new ErgoStateContext(Seq(parentHeader), None,
      genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext = agedContext
      override def emissionBoxOpt: Option[ErgoBox] = None
    }
    val storage = HistoryStorage(fixtureSettings)
    val unresolvedRows = (0 until unresolvedCount).flatMap { index =>
      // The index is structurally complete, but these boxes are absent from the UTXO.
      val box = testBox(10000000000L + index, Constants.TrueTree, threshold, Seq.empty, Map.empty)
      val row = new IndexedErgoBox(threshold, None, None, None, box, firstGlobalIndex + index)
      Seq[ExtraIndex](row, NumericBoxIndex(row.globalIndex, row.id), StorageRentBox(row))
    }
    val indexed = new IndexedErgoBox(threshold, None, None, None, laterBox, firstGlobalIndex + unresolvedCount)
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8)
        .putLong(firstGlobalIndex + unresolvedCount + 1L).array
    ), (unresolvedRows ++ Seq[ExtraIndex](indexed,
      NumericBoxIndex(indexed.globalIndex, indexed.id), StorageRentBox(indexed))).toArray)
    val scanRowCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
    val history = Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "bestFullBlockOpt" => Some(parentBlock)
            case "fullBlockHeight" => Int.box(parentHeader.height)
            case "historyStorage" => storage
            case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "storageRentScanPage" =>
              val page = storage.storageRentScanPage(args(0).asInstanceOf[Int],
                args(1).asInstanceOf[Long], args(2).asInstanceOf[Option[Array[Byte]]],
                args(3).asInstanceOf[Int])
              scanRowCounts += page.entries.length
              page
            case "storageRentBoxesAtOrBefore" =>
              storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int],
                args(1).asInstanceOf[Int], args(2).asInstanceOf[Option[StorageRentBox]])
            case "removeStorageRentBoxes" => fail("unresolved rent rows must not be evicted")
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      val before = storage.storageRentBoxesAtOrBefore(threshold, unresolvedCount + 1).map(_.globalIndex).toSeq
      before shouldBe (firstGlobalIndex to firstGlobalIndex + unresolvedCount)
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, height)))
      val scan = new CandidateGenerator.StorageRentScan
      def candidateInputs(): Set[ModifierId] = {
        val result = CandidateGenerator.generateCandidate(history, state,
          ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend), None, fixtureSettings, scan)
        result should not be None
        withClue(result.toString) { result.get.isSuccess shouldBe true }
        result.get.get._1.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))).toSet
      }

      candidateInputs() shouldBe Set(bytesToId(freshBox.id))
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + scanLimit - 1L)
      candidateInputs() should contain(bytesToId(laterBox.id))
      scanRowCounts.toSeq shouldBe Seq(scanLimit, unresolvedCount + 1 - scanLimit)
      scan.after shouldBe None
      candidateInputs() shouldBe Set(bytesToId(freshBox.id))
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + scanLimit - 1L)
      storage.storageRentBoxesAtOrBefore(threshold, unresolvedCount + 1).map(_.globalIndex).toSeq shouldBe before
      state.boxById(laterBox.id) shouldBe Some(laterBox)

      // Simulate selected-chain indexation removing the later claim, then rollback
      // restoring an older key behind the saved cursor. This is manual index mutation,
      // not an execution of ExtraIndexer rollback or a state rollback interleaving.
      storage.removeExtra(Array(StorageRentBox(indexed).id))
      val restored = new IndexedErgoBox(restoredBox.creationHeight, None, None, None,
        restoredBox, firstGlobalIndex + unresolvedCount + 1L)
      storage.insertExtra(Array(ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8)
        .putLong(restored.globalIndex + 1).array), Array[ExtraIndex](restored,
        NumericBoxIndex(restored.globalIndex, restored.id), StorageRentBox(restored)))
      candidateInputs() shouldBe Set(bytesToId(freshBox.id))
      scan.after shouldBe None
      candidateInputs() should contain(bytesToId(restoredBox.id))
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + scanLimit - 2L)
    } finally {
      storage.close()
      state.closeStorage()
    }
  }

  property("advances past a repeatedly rejected first rent batch to offer later eligible boxes") {
    val height = 3 * Constants.StoragePeriod
    val eligibleBoxes = (0 to StorageRentClaimBuilder.MaxClaims).map { index =>
      testBox(10000000000L + index, Constants.TrueTree,
        height - Constants.StoragePeriod, Seq.empty, Map.empty)
    }
    val fixtureSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val firstGlobalIndex = ErgoState.genesisBoxes(fixtureSettings.chainSettings).size.toLong + 1L

    val baseState = UtxoState.fromBoxHolder(BoxHolder(eligibleBoxes), None,
      createTempDir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    val parentHeader = block.header.copy(height = height - 1)
    val parentBlock = block.copy(parentHeader)
    val agedContext = new ErgoStateContext(Seq(parentHeader), None,
      genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext = agedContext
      override def emissionBoxOpt: Option[ErgoBox] = None
    }

    val storage = HistoryStorage(fixtureSettings)
    eligibleBoxes.zipWithIndex.foreach { case (box, index) =>
      val globalIndex = firstGlobalIndex + index
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
      storage.insertExtra(Array.empty[(Array[Byte], Array[Byte])],
        Array[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed)))
    }
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + eligibleBoxes.size).array
    ), Array.empty[ExtraIndex])

    val history = Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "bestFullBlockOpt" => Some(parentBlock)
            case "fullBlockHeight" => Int.box(parentHeader.height)
            case "historyStorage" => storage
            case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "storageRentScanPage" =>
              storage.storageRentScanPage(args(0).asInstanceOf[Int], args(1).asInstanceOf[Long],
                args(2).asInstanceOf[Option[Array[Byte]]], args(3).asInstanceOf[Int])
            case "storageRentBoxesAtOrBefore" =>
              storage.storageRentBoxesAtOrBefore(args(0).asInstanceOf[Int], args(1).asInstanceOf[Int],
                if (args.length > 2) args(2).asInstanceOf[Option[StorageRentBox]] else None)
            case "removeStorageRentBoxes" =>
              storage.removeStorageRentBoxes(args(0).asInstanceOf[Seq[ModifierId]])
              ().asInstanceOf[AnyRef]
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      val threshold = height - Constants.StoragePeriod
      val indexedBefore = history.storageRentBoxesAtOrBefore(threshold, 4 * StorageRentClaimBuilder.MaxClaims)
      indexedBefore.map(_.globalIndex) shouldBe (firstGlobalIndex to firstGlobalIndex + StorageRentClaimBuilder.MaxClaims)

      val conflictingSpend = ErgoTransaction(
        IndexedSeq(Input(eligibleBoxes.head.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(eligibleBoxes.head.value, Constants.TrueTree, height)))

      val scan = new CandidateGenerator.StorageRentScan
      def candidateInputs(): Set[ModifierId] = {
        val result = CandidateGenerator.generateCandidate(history, state,
          ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(conflictingSpend), None, fixtureSettings, scan)
        result should not be None
        result.get.isSuccess shouldBe true
        val (candidate, _) = result.get.get
        candidate.candidateBlock.transactions.map(_.id) should contain(conflictingSpend.id)
        candidate.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))).toSet
      }

      val laterBoxId = bytesToId(eligibleBoxes.last.id)
      candidateInputs() should not contain laterBoxId
      // Advancing only to the last examined row must leave row 101 for this attempt,
      // even though it was already fetched in the first 400-row scan window.
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + StorageRentClaimBuilder.MaxClaims - 1L)
      candidateInputs() should contain(laterBoxId)
      history.storageRentBoxesAtOrBefore(threshold, 4 * StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex) shouldBe (firstGlobalIndex to firstGlobalIndex + StorageRentClaimBuilder.MaxClaims)

      // The accepted but unmined claim does not pin the cursor: the next
      // regeneration revisits the first batch, then rotates back to the later row.
      scan.after shouldBe None
      candidateInputs() should not contain laterBoxId
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + StorageRentClaimBuilder.MaxClaims - 1L)
      candidateInputs() should contain(laterBoxId)
      scan.after shouldBe None
      history.storageRentBoxesAtOrBefore(threshold, 4 * StorageRentClaimBuilder.MaxClaims)
        .map(_.globalIndex) shouldBe (firstGlobalIndex to firstGlobalIndex + StorageRentClaimBuilder.MaxClaims)
    } finally {
      storage.close()
      state.closeStorage()
    }
  }
}
