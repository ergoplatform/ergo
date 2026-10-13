package org.ergoplatform.mining

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.ByteBuffer

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexer, IndexedErgoBox, NumericBoxIndex, StorageRentBox}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoStateContext, UtxoState, VotingData}
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.{ModifierId, bytesToId}
import sigma.Colls
import sigma.ast.ByteArrayConstant
import sigmastate.helpers.TestingHelpers._

/** Candidate scans must advance past builder skips without deleting unspent eligibility. */
class CandidateRentSkippedPageSpec extends ErgoCorePropertyTest with TestFileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}

  property("revisits an old rent row while every new height adds a full eligible page") {
    val firstHeight = 3 * Constants.StoragePeriod
    val firstCutoff = firstHeight - Constants.StoragePeriod
    val scanLimit = 4 * StorageRentClaimBuilder.MaxClaims
    val highMinValuePerByte = parameters.minValuePerByte + Parameters.MinValueStep
    val highParameters = new Parameters(parameters.height,
      parameters.parametersTable.updated(Parameters.MinValuePerByteIncrease, highMinValuePerByte),
      parameters.proposedUpdate)

    def floorBox(index: Int, creationHeight: Int, minValuePerByte: Int,
                 padding: Int = 0): ErgoBox = {
      val txId = bytesToId(ByteBuffer.allocate(32).putInt(index).array)
      val registers: ErgoBox.AdditionalRegisters =
        if (padding == 0) Map.empty
        else Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(padding)(index.toByte)))
      def at(value: Long): ErgoBox =
        new ErgoBoxCandidate(value, Constants.TrueTree, creationHeight,
          Colls.emptyColl, registers).toBox(txId, 0)
      var box = at(10000000000L)
      var floor = minValuePerByte.toLong * box.bytes.length
      while (box.value != floor) {
        box = at(floor)
        floor = minValuePerByte.toLong * box.bytes.length
      }
      box
    }

    val oldBox = floorBox(0, firstCutoff, highMinValuePerByte, padding = 64)
    val firstPage = (1 until scanLimit).map(i =>
      floorBox(i, firstCutoff, parameters.minValuePerByte))
    val laterPages = (1 to 2).flatMap { round =>
      (0 until scanLimit).map(i =>
        floorBox(round * scanLimit + i, firstCutoff + round, parameters.minValuePerByte))
    }
    val rentBoxes = (oldBox +: firstPage) ++ laterPages
    val freshBox = testBox(12000000000L, Constants.TrueTree, firstHeight - 1,
      Seq.empty, Map.empty)
    StorageRentClaimBuilder.buildClaim(Iterator(oldBox), firstHeight, highParameters,
      defaultMinerPk, None, Set.empty[ModifierId]) shouldBe None
    StorageRentClaimBuilder.buildClaim(Iterator(oldBox), firstHeight + 1, parameters,
      defaultMinerPk, None, Set.empty[ModifierId]).isDefined shouldBe true

    val fixtureSettings = baseSettings.copy(directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val firstGlobalIndex = 1L
    val baseState = UtxoState.fromBoxHolder(BoxHolder(rentBoxes :+ freshBox), None,
      createTempDir, fixtureSettings, parameters)
    val block = invalidErgoFullBlockGen.sample.get
    var activeHeight = firstHeight
    var activeParameters = highParameters
    var forkOffset = 0L
    var priorParentSelected = true
    var selectedChainChecks = 0
    def activeHeader = block.header.copy(height = activeHeight - 1,
      timestamp = block.header.timestamp + forkOffset)
    val state = new UtxoState(baseState.persistentProver, baseState.version,
      baseState.store, fixtureSettings) {
      override def stateContext: ErgoStateContext =
        new ErgoStateContext(Seq(activeHeader), None, genesisStateDigest, activeParameters,
          validationSettingsNoIl, VotingData.empty)(fixtureSettings.chainSettings)
      override def emissionBoxOpt: Option[ErgoBox] = None
    }
    val storage = HistoryStorage(fixtureSettings)
    val rows = rentBoxes.zipWithIndex.flatMap { case (box, index) =>
      val globalIndex = firstGlobalIndex + index
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
      Seq[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed))
    }
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(firstHeight - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + rentBoxes.size).array
    ), rows.toArray)
    val scanSizes = scala.collection.mutable.ArrayBuffer.empty[Int]
    val history = Proxy.newProxyInstance(classOf[ErgoHistoryReader].getClassLoader,
      Array[Class[_]](classOf[ErgoHistoryReader]), new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "bestFullBlockOpt" => Some(block.copy(activeHeader))
            case "fullBlockHeight" => Int.box(activeHeight - 1)
            case "historyStorage" => storage
            case "typedExtraIndexById" => storage.getExtraIndex(args(0).asInstanceOf[ModifierId])
            case "storageRentScanPage" =>
              val page = storage.storageRentScanPage(args(0).asInstanceOf[Int],
                args(1).asInstanceOf[Long], args(2).asInstanceOf[Option[Array[Byte]]],
                args(3).asInstanceOf[Int])
              scanSizes += page.entries.length
              page
            case "isInSelectedFullChain" =>
              selectedChainChecks += 1
              java.lang.Boolean.valueOf(priorParentSelected)
            case "removeStorageRentBoxes" => fail("candidate must retain skipped rent rows")
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => activeHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }).asInstanceOf[ErgoHistoryReader]
    val scan = new CandidateGenerator.StorageRentScan
    val oldId = bytesToId(oldBox.id)
    val freshId = bytesToId(freshBox.id)

    def candidateInputs(): Set[ModifierId] = {
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, activeHeight)))
      val result = CandidateGenerator.generateCandidate(history, state,
        ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend),
        None, fixtureSettings, scan)
      result should not be None
      withClue(result.toString) { result.get.isSuccess shouldBe true }
      result.get.get._1.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))).toSet
    }

    try {
      candidateInputs() shouldBe Set(freshId)
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + scanLimit - 1)
      activeParameters = parameters
      val revisits = (1 to 2).map { round =>
        activeHeight = firstHeight + round
        storage.insertExtra(Array(ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4)
          .putInt(activeHeight - 1).array), Array.empty[ExtraIndex])
        val inputs = candidateInputs()
        inputs should contain(freshId)
        inputs.contains(oldId)
      }
      scanSizes.toSeq shouldBe Seq(scanLimit, 0, scanLimit)
      state.boxById(oldBox.id) shouldBe Some(oldBox)
      storage.storageRentBoxesAtOrBefore(Int.MaxValue, rentBoxes.size).exists(_.id == StorageRentBox(
        new IndexedErgoBox(oldBox.creationHeight, None, None, None, oldBox, firstGlobalIndex)).id) shouldBe true
      withClue(s"old rent box stayed eligible; cursor=${scan.after.map(_.id)}: ") {
        revisits should contain(true)
      }
      scan.after should not be None
      // A competing selected parent can reuse global box numbers after rollback.
      // Its first candidate must restart at the old row, not inherit this cursor.
      val oldParentHeight = scan.progress.parentHeight
      activeHeight = oldParentHeight + 2
      forkOffset = 1L
      priorParentSelected = false
      val checksBeforeReorg = selectedChainChecks
      activeHeight - 1 should be > oldParentHeight
      candidateInputs() should contain(oldId)
      selectedChainChecks should be > checksBeforeReorg
    } finally {
      storage.close()
      state.closeStorage()
    }
  }

  property("retains a full page of resolved builder-skipped rows while claiming the next page") {
    val height = 3 * Constants.StoragePeriod
    val threshold = height - Constants.StoragePeriod
    val scanLimit = 4 * StorageRentClaimBuilder.MaxClaims
    val skippedBoxes = (0 until scanLimit).map { index =>
      // Consensus accepts equality at the dust floor; the rent builder requires >.
      // Iterate because changing the value can change its serialized VLQ length.
      val txId = bytesToId(ByteBuffer.allocate(32).putInt(index).array)
      def boxAtValue(value: Long): ErgoBox =
        new ErgoBoxCandidate(value, Constants.TrueTree, threshold).toBox(txId, 0)
      var box = boxAtValue(10000000000L)
      var floor = parameters.minValuePerByte.toLong * box.bytes.length
      while (box.value != floor) {
        box = boxAtValue(floor)
        floor = parameters.minValuePerByte.toLong * box.bytes.length
      }
      box
    }
    skippedBoxes.map(box => bytesToId(box.id)).distinct.size shouldBe scanLimit
    skippedBoxes.foreach { box =>
      box.value shouldBe parameters.minValuePerByte.toLong * box.bytes.length
    }
    StorageRentClaimBuilder.buildClaim(skippedBoxes.iterator, height, parameters,
      defaultMinerPk, None, Set.empty[ModifierId]) shouldBe None
    val laterBox = testBox(11000000000L, Constants.TrueTree, threshold, Seq.empty, Map.empty)
    val freshBox = testBox(12000000000L, Constants.TrueTree, height - 1, Seq.empty, Map.empty)
    val rentBoxes = skippedBoxes :+ laterBox
    val fixtureSettings = baseSettings.copy(
      directory = createTempDir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val baseState = UtxoState.fromBoxHolder(BoxHolder(rentBoxes :+ freshBox), None,
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
    val firstGlobalIndex = 1L
    val rows = rentBoxes.zipWithIndex.flatMap { case (box, index) =>
      val globalIndex = firstGlobalIndex + index
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
      Seq[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed))
    }
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + rentBoxes.size).array
    ), rows.toArray)
    val scanRowCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
    val removedRowCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
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
            case "removeStorageRentBoxes" =>
              val ids = args(0).asInstanceOf[Seq[ModifierId]]
              removedRowCounts += ids.size
              storage.removeStorageRentBoxes(ids)
              ().asInstanceOf[AnyRef]
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      val expectedIndices = firstGlobalIndex to (firstGlobalIndex + scanLimit)
      def assertUnspentRowsRetained(): Unit = {
        withClue(s"scan pages=$scanRowCounts; removed rows=$removedRowCounts: ") {
          storage.storageRentBoxesAtOrBefore(threshold, scanLimit + 1)
            .map(_.globalIndex).toSeq shouldBe expectedIndices
        }
        rentBoxes.foreach(box => state.boxById(box.id) shouldBe Some(box))
      }
      assertUnspentRowsRetained()
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, height)))
      val scan = new CandidateGenerator.StorageRentScan
      def candidateInputs(): Set[ModifierId] = {
        val result = CandidateGenerator.generateCandidate(history, state,
          ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend), None, fixtureSettings, scan)
        result should not be None
        withClue(result.toString) { result.get.isSuccess shouldBe true }
        val (candidate, _) = result.get.get
        candidate.candidateBlock.transactions.map(_.id) should contain(independentSpend.id)
        candidate.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))).toSet
      }

      candidateInputs() shouldBe Set(bytesToId(freshBox.id))
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + scanLimit - 1)
      // A successful ordinary candidate must preserve every unspent skipped row.
      assertUnspentRowsRetained()
      candidateInputs() shouldBe Set(bytesToId(freshBox.id), bytesToId(laterBox.id))
      scanRowCounts.toSeq shouldBe Seq(scanLimit, 1)
      removedRowCounts shouldBe empty
      // Neither generated candidate is applied to the persistent state or index.
      assertUnspentRowsRetained()
      state.boxById(freshBox.id) shouldBe Some(freshBox)
    } finally {
      storage.close()
      state.closeStorage()
    }
  }

  property("advances past a full claim of positive-wrapped fees dropped below the output minimum") {
    val height = 3 * Constants.StoragePeriod
    val threshold = height - Constants.StoragePeriod
    val size = 3436
    val value = parameters.minValuePerByte.toLong * size + 1
    val wrappedFee = parameters.storageFeeFactor * size
    parameters.storageFeeFactor.toLong * size shouldBe 4295000000L
    wrappedFee shouldBe 32704

    def boxAt(padding: Int, index: Int): ErgoBox = {
      val txId = bytesToId(ByteBuffer.allocate(32).putInt(index).array)
      val registers: ErgoBox.AdditionalRegisters =
        Map(ErgoBox.R4 -> ByteArrayConstant(Array.fill(padding)(index.toByte)))
      new ErgoBoxCandidate(value, Constants.TrueTree, threshold, Colls.emptyColl, registers)
        .toBox(txId, 0)
    }
    val padding = (3300 to size).find(n => boxAt(n, 0).bytes.length == size)
      .getOrElse(fail("no 3436-byte box with a single byte-array register"))
    val overflowBoxes = (0 until StorageRentClaimBuilder.MaxClaims).map(i => boxAt(padding, i))
    overflowBoxes.map(b => bytesToId(b.id)).distinct.size shouldBe StorageRentClaimBuilder.MaxClaims
    overflowBoxes.foreach { box =>
      box.bytes.length shouldBe size
      box.value shouldBe parameters.minValuePerByte.toLong * box.bytes.length + 1
      height - box.creationHeight shouldBe Constants.StoragePeriod
      val recreation = new ErgoBoxCandidate(box.value - 1, box.ergoTree, height,
        box.additionalTokens, box.additionalRegisters)
      recreation.toBox(bytesToId(box.id), 0).bytes.length shouldBe size
    }
    val builderSkips = scala.collection.mutable.ArrayBuffer.empty[ErgoBox]
    StorageRentClaimBuilder.buildClaim(overflowBoxes.iterator, height, parameters,
      defaultMinerPk, None, Set.empty[ModifierId], b => builderSkips += b) shouldBe None
    builderSkips shouldBe empty

    val laterBox = testBox(11000000000L, Constants.TrueTree, threshold, Seq.empty, Map.empty)
    StorageRentClaimBuilder.buildClaim(Iterator(laterBox), height, parameters,
      defaultMinerPk, None, Set.empty[ModifierId]).isDefined shouldBe true
    val freshBox = testBox(12000000000L, Constants.TrueTree, height - 1, Seq.empty, Map.empty)
    val rentBoxes = overflowBoxes :+ laterBox
    val dir = createTempDir
    val fixtureSettings = baseSettings.copy(
      directory = dir.getAbsolutePath,
      nodeSettings = baseSettings.nodeSettings.copy(extraIndex = true, storageRentCollection = true))
    val baseState = UtxoState.fromBoxHolder(BoxHolder(rentBoxes :+ freshBox), None,
      dir, fixtureSettings, parameters)
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
    val firstGlobalIndex = 1L
    val rows = rentBoxes.zipWithIndex.flatMap { case (box, index) =>
      val globalIndex = firstGlobalIndex + index
      val indexed = new IndexedErgoBox(box.creationHeight, None, None, None, box, globalIndex)
      Seq[ExtraIndex](indexed, NumericBoxIndex(globalIndex, indexed.id), StorageRentBox(indexed))
    }
    storage.insertExtra(Array(
      ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(height - 1).array,
      ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(firstGlobalIndex + rentBoxes.size).array
    ), rows.toArray)
    val scanRowCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
    val removedRowCounts = scala.collection.mutable.ArrayBuffer.empty[Int]
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
            case "removeStorageRentBoxes" =>
              val ids = args(0).asInstanceOf[Seq[ModifierId]]
              removedRowCounts += ids.size
              storage.removeStorageRentBoxes(ids)
              ().asInstanceOf[AnyRef]
            case "typedModifierById" => None
            case "requiredDifficultyAfter" => parentHeader.requiredDifficulty
            case other => throw new UnsupportedOperationException(s"unexpected history read: $other")
          }
      }
    ).asInstanceOf[ErgoHistoryReader]

    try {
      def assertUnspentRowsRetained(): Unit = {
        storage.storageRentBoxesAtOrBefore(threshold, rentBoxes.size)
          .map(_.globalIndex).toSeq shouldBe (firstGlobalIndex until firstGlobalIndex + rentBoxes.size)
        rentBoxes.foreach(box => state.boxById(box.id) shouldBe Some(box))
      }
      assertUnspentRowsRetained()
      val independentSpend = ErgoTransaction(
        IndexedSeq(Input(freshBox.id, emptyProverResult)), IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(freshBox.value, Constants.TrueTree, height)))
      val scan = new CandidateGenerator.StorageRentScan
      def candidateInputs(): Set[ModifierId] = {
        val result = CandidateGenerator.generateCandidate(history, state,
          ErgoMemPool.empty(fixtureSettings), defaultMinerPk, Seq(independentSpend), None, fixtureSettings, scan)
        result should not be None
        withClue(result.toString) { result.get.isSuccess shouldBe true }
        val (candidate, _) = result.get.get
        candidate.candidateBlock.transactions.map(_.id) should contain(independentSpend.id)
        candidate.candidateBlock.transactions.flatMap(_.inputs.map(in => bytesToId(in.boxId))).toSet
      }

      candidateInputs() shouldBe Set(bytesToId(freshBox.id))
      scan.after.map(_.globalIndex) shouldBe Some(firstGlobalIndex + StorageRentClaimBuilder.MaxClaims - 1)
      assertUnspentRowsRetained()
      candidateInputs() shouldBe Set(bytesToId(freshBox.id), bytesToId(laterBox.id))
      scanRowCounts.toSeq shouldBe Seq(rentBoxes.size, 1)
      removedRowCounts shouldBe empty
      assertUnspentRowsRetained()
    } finally {
      storage.close()
      state.closeStorage()
    }
  }
}
