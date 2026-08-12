package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSystem, Identify, Props}
import akka.testkit.TestProbe
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.http.api.SortDirection
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{RemoteBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.Index
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.SegmentSerializer.{boxSegmentId, txSegmentId}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.{ErgoSettings, NetworkType}
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, bytesToId}
import spire.implicits.cfor

import java.util.concurrent.locks.{Condition, ReentrantLock}
import java.nio.ByteBuffer
import java.nio.file.Files
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
  case class CacheBlockTransactions(height: Int, transactions: BlockTransactions)
  case class DeferNextHeaderOnce(height: Int)
  case class DeferBlockTransactionsOnce(height: Int)
  case class Reload()
  case class ForceRollback(height: Int)
  case class GetLoadedState()
  case class FailNextRollbackRemoval(probe: ActorRef)
  case class PauseBufferedCatchUpAt(height: Int, saveLimit: Int, probe: ActorRef)

  type ID_LL = mutable.HashMap[ModifierId,(Long,Long)]

  val HEIGHT: Int = 50
  val BRANCHPOINT: Int = HEIGHT / 2
  implicit val segmentThreshold: Int = 8

  val system: ActorSystem = ActorSystem.create("indexer-test")
  val indexer: ActorRef = system.actorOf(Props.create(classOf[ExtraIndexerTestActor], this))

  var _history: ErgoHistory = _
  def history: ErgoHistoryReader = _history.getReader

  def fullChainHeaderAt(height: Int): Header = {
    val bestFullBlock = history.bestFullBlockOpt.get
    history.headerChainBack(bestFullBlock.height - height + 1, bestFullBlock.header, _.height == height)
      .headers
      .find(_.height == height)
      .get
  }

  def fullChainTransactionsAt(height: Int): BlockTransactions = {
    val header = fullChainHeaderAt(height)
    history.typedModifierById[BlockTransactions](header.transactionsId).get
  }

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
      val header = fullChainHeaderAt(i)
      val block = history.getFullBlock(header)
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

  // example G-30;R-20;G-35;R-30
  def rollbackWithPattern(pattern: String): Unit = {

    def rollback(n: Int): Unit = {
      println(s"Rollback to $n")
      var state = IndexerState.fromHistory(_history)

      val txIndexBefore = state.globalTxIndex
      val boxIndexBefore = state.globalBoxIndex

      // manually count balances
      val (addresses, templates, indexedTokens, txsIndexed, boxesIndexed) = manualIndex(n)

      // perform rollback
      indexer ! ForceRollback(n)
      awaitCondition(done)
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
    }

    def generate(n: Int): Unit = {
      println(s"Generate to $n")
      indexer ! CreateDB(n)
      indexer ! Index()
      awaitCondition(done)

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

  property("catches up past a direct block event when history is already ahead") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)

    indexer ! ExtendDB(HEIGHT + 2)
    awaitCondition(created)
    val firstHeader = fullChainHeaderAt(HEIGHT + 1)
    indexer ! RemoteBlockApplied(firstHeader, history.getFullBlock(firstHeader).get.transactions.map(_.id))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 2
      state.indexedHeaderId shouldBe Some(fullChainHeaderAt(HEIGHT + 2).id)
    }
    indexer ! Reset()
  }

  property("restores the exact persisted indexed header after the best header changes") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)

    val persistedState = IndexerState.fromHistory(_history)
    persistedState.indexedHeaderId shouldBe history.bestHeaderIdAtHeight(HEIGHT)

    indexer ! GenerateBetterChainTip()
    awaitCondition(created)
    indexer ! CreateDB(HEIGHT + 1)
    awaitCondition(created)
    history.bestHeaderIdAtHeight(HEIGHT) should not be persistedState.indexedHeaderId

    IndexerState.fromHistory(_history).indexedHeaderId shouldBe persistedState.indexedHeaderId
    indexer ! Reset()
  }

  property("rebuilds legacy, malformed, and interrupted checkpoints") {
    def intBytes(value: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(value).array
    def longBytes(value: Long): Array[Byte] = ByteBuffer.allocate(8).putLong(value).array
    val schema = ExtraIndexer.SchemaVersionKey -> intBytes(ExtraIndexer.NewestVersion)
    val emptyMetadata = Array(
      ExtraIndexer.IndexedHeightKey -> intBytes(0),
      ExtraIndexer.GlobalTxIndexKey -> longBytes(0),
      ExtraIndexer.GlobalBoxIndexKey -> longBytes(0),
      ExtraIndexer.RollbackToKey -> intBytes(0)
    )
    val invalidCheckpoints = Seq[(String, Array[(Array[Byte], Array[Byte])])](
      "legacy non-empty" -> Array(
        ExtraIndexer.SchemaVersionKey -> intBytes(6),
        ExtraIndexer.IndexedHeightKey -> intBytes(1),
        ExtraIndexer.GlobalTxIndexKey -> longBytes(0),
        ExtraIndexer.GlobalBoxIndexKey -> longBytes(0),
        ExtraIndexer.RollbackToKey -> intBytes(0)
      ),
      "legacy height zero with stale counters" -> Array(
        ExtraIndexer.SchemaVersionKey -> intBytes(6),
        ExtraIndexer.IndexedHeightKey -> intBytes(0),
        ExtraIndexer.GlobalTxIndexKey -> longBytes(7),
        ExtraIndexer.GlobalBoxIndexKey -> longBytes(9),
        ExtraIndexer.RollbackToKey -> intBytes(0)
      ),
      "missing header id" -> (Array(schema) ++ emptyMetadata.updated(0, ExtraIndexer.IndexedHeightKey -> intBytes(1))),
      "short header id" -> (Array(schema) ++ emptyMetadata.updated(0, ExtraIndexer.IndexedHeightKey -> intBytes(1)) ++
        Array(ExtraIndexer.IndexedHeaderIdKey -> Array[Byte](1, 2, 3))),
      "unknown header id" -> (Array(schema) ++ emptyMetadata.updated(0, ExtraIndexer.IndexedHeightKey -> intBytes(1)) ++
        Array(ExtraIndexer.IndexedHeaderIdKey -> Array.fill[Byte](32)(1))),
      "interrupted rollback" -> (Array(schema) ++ emptyMetadata.updated(3, ExtraIndexer.RollbackToKey -> intBytes(1))),
      "short indexed height" -> (Array(schema) ++ emptyMetadata.updated(0, ExtraIndexer.IndexedHeightKey -> Array[Byte](1))),
      "long transaction index" -> (Array(schema) ++ emptyMetadata.updated(1, ExtraIndexer.GlobalTxIndexKey -> Array.fill[Byte](9)(1))),
      "short box index" -> (Array(schema) ++ emptyMetadata.updated(2, ExtraIndexer.GlobalBoxIndexKey -> Array[Byte](1))),
      "short rollback target" -> (Array(schema) ++ emptyMetadata.updated(3, ExtraIndexer.RollbackToKey -> Array[Byte](1))),
      "current schema height zero with stale transaction counter" ->
        (Array(schema) ++ emptyMetadata.updated(1, ExtraIndexer.GlobalTxIndexKey -> longBytes(1))),
      "current schema height zero with stale box counter" ->
        (Array(schema) ++ emptyMetadata.updated(2, ExtraIndexer.GlobalBoxIndexKey -> longBytes(1))),
      "negative box index" -> (Array(schema) ++ emptyMetadata.updated(2, ExtraIndexer.GlobalBoxIndexKey -> longBytes(-1)))
    )

    invalidCheckpoints.foreach { case (name, entries) =>
      val dbDir = Files.createTempDirectory("extra-indexer-checkpoint").toFile
      val dbSettings = initSettings.copy(
        directory = dbDir.getAbsolutePath,
        nodeSettings = initSettings.nodeSettings.copy(extraIndex = true)
      )
      val db = HistoryStorage(dbSettings)
      db.insertExtraTry(entries, Array.empty).get
      db.close()

      val probe = TestProbe()(system)
      system.actorOf(Props(new Actor {
        override def preStart(): Unit = {
          val reloaded = ErgoHistory.readOrGenerate(dbSettings)(context)
          probe.ref ! ((
            ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, reloaded).getInt,
            ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, reloaded).getLong,
            ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, reloaded).getLong,
            ExtraIndexer.getIndex(ExtraIndexer.RollbackToKey, reloaded).getInt,
            reloaded.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
          ))
          reloaded.closeStorage()
          context.stop(self)
        }

        override def receive: Receive = Actor.emptyBehavior
      }))
      withClue(name) {
        probe.expectMsg((0, 0L, 0L, 0, None))
      }
    }
  }

  property("preserves a valid non-empty checkpoint across storage reopen") {
    val dbDir = Files.createTempDirectory("extra-indexer-valid-checkpoint").toFile
    val dbSettings = initSettings.copy(
      directory = dbDir.getAbsolutePath,
      networkType = NetworkType.TestNet,
      nodeSettings = initSettings.nodeSettings.copy(extraIndex = true, headerChainDiff = 5000)
    )
    val generationSettings = dbSettings.copy(
      nodeSettings = dbSettings.nodeSettings.copy(extraIndex = false)
    )
    val probe = TestProbe()(system)
    system.actorOf(Props(new Actor {
      override def preStart(): Unit = {
        val generatedHistory = ErgoHistory.readOrGenerate(generationSettings)(context)
        ChainGenerator.generate(1, dbDir, generatedHistory, None)
        generatedHistory.closeStorage()

        val indexedHistory = ErgoHistory.readOrGenerate(dbSettings)(context)
        val header = indexedHistory.bestFullBlockOpt.get.header
        val blockTransactions = indexedHistory.typedModifierById[BlockTransactions](header.transactionsId).get
        val txCount = blockTransactions.txs.size.toLong
        val boxCount = blockTransactions.txs.map(_.outputs.size.toLong).sum
        val lastTx = blockTransactions.txs.last
        val lastTxIndex = txCount - 1
        val lastBoxIndex = boxCount - 1
        val outputIndexes = Array.tabulate(lastTx.outputs.size) { i =>
          boxCount - lastTx.outputs.size.toLong + i.toLong
        }
        val indexedTx = IndexedErgoTransaction.fromTx(
          lastTx,
          blockTransactions.txs.size - 1,
          header.height,
          lastTxIndex,
          Array.fill(lastTx.inputs.size)(0L),
          outputIndexes
        )
        val indexedBoxes = lastTx.outputs.zip(outputIndexes).map { case (output, outputIndex) =>
          new IndexedErgoBox(
            header.height,
            None,
            None,
            None,
            output,
            outputIndex
          )
        }.toArray
        val numericBoxes = indexedBoxes.map(box => NumericBoxIndex(box.globalIndex, box.id))
        val lastBox = indexedBoxes.last
        val numericTx = NumericTxIndex(lastTxIndex, lastTx.id)
        val numericBox = numericBoxes.last
        val checkpointMetadata = Array(
          ExtraIndexer.SchemaVersionKey -> ByteBuffer.allocate(4).putInt(ExtraIndexer.NewestVersion).array,
          ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(header.height).array,
          ExtraIndexer.GlobalTxIndexKey -> ByteBuffer.allocate(8).putLong(txCount).array,
          ExtraIndexer.GlobalBoxIndexKey -> ByteBuffer.allocate(8).putLong(boxCount).array,
          ExtraIndexer.RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array,
          ExtraIndexer.IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(header.id)
        )
        val checkpointObjects = Array[ExtraIndex](numericTx, indexedTx) ++ numericBoxes ++ indexedBoxes
        indexedHistory.historyStorage.insertExtraTry(
          checkpointMetadata,
          checkpointObjects
        ).get
        indexedHistory.closeStorage()

        val reloaded = ErgoHistory.readOrGenerate(dbSettings)(context)
        val state = IndexerState.fromHistory(reloaded)
        val terminalRowsPreserved =
          reloaded.typedExtraIndexById[NumericTxIndex](numericTx.id).contains(numericTx) &&
            reloaded.typedExtraIndexById[IndexedErgoTransaction](indexedTx.id).exists { tx =>
              tx.txid == indexedTx.txid && tx.globalIndex == indexedTx.globalIndex &&
                tx.height == indexedTx.height && tx.outputNums.sameElements(indexedTx.outputNums)
            } &&
            reloaded.typedExtraIndexById[NumericBoxIndex](numericBox.id).contains(numericBox) &&
            reloaded.typedExtraIndexById[IndexedErgoBox](lastBox.id).exists(_.globalIndex == lastBoxIndex)
        probe.ref ! state
        probe.ref ! terminalRowsPreserved
        reloaded.closeStorage()

        val interrupted = HistoryStorage(dbSettings)
        interrupted.insertExtraTry(
          Array(ExtraIndexer.RollbackToKey -> ByteBuffer.allocate(4).putInt(header.height).array),
          Array.empty
        ).get
        interrupted.close()

        val rebuiltAfterInterruptedRollback = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterInterruptedRollback).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterInterruptedRollback).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterInterruptedRollback).getLong,
          rebuiltAfterInterruptedRollback.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterInterruptedRollback.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        rebuiltAfterInterruptedRollback.closeStorage()

        val wrongSchema = HistoryStorage(dbSettings)
        wrongSchema.insertExtraTry(
          Array(ExtraIndexer.SchemaVersionKey -> ByteBuffer.allocate(4).putInt(ExtraIndexer.NewestVersion - 1).array),
          Array.empty
        ).get
        wrongSchema.close()

        val rebuiltAfterSchemaMismatch = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterSchemaMismatch).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterSchemaMismatch).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterSchemaMismatch).getLong,
          rebuiltAfterSchemaMismatch.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterSchemaMismatch.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        rebuiltAfterSchemaMismatch.closeStorage()

        val malformed = HistoryStorage(dbSettings)
        val malformedTx = indexedTx.copy(outputNums = indexedTx.outputNums ++ Array(lastBoxIndex))
        malformed.insertExtraTry(Array.empty, Array(malformedTx)).get
        malformed.close()

        val rebuiltAfterMalformedTx = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterMalformedTx).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterMalformedTx).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterMalformedTx).getLong,
          rebuiltAfterMalformedTx.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterMalformedTx.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        rebuiltAfterMalformedTx.closeStorage()

        val malformedInputs = HistoryStorage(dbSettings)
        val malformedInputTx = indexedTx.copy(inputNums = indexedTx.inputNums.map(_ + 1L))
        malformedInputs.insertExtraTry(Array.empty, Array(malformedInputTx)).get
        malformedInputs.close()

        val rebuiltAfterMalformedInputs = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterMalformedInputs).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterMalformedInputs).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterMalformedInputs).getLong,
          rebuiltAfterMalformedInputs.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterMalformedInputs.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        rebuiltAfterMalformedInputs.closeStorage()

        val malformedSpentOutput = HistoryStorage(dbSettings)
        val spentTerminalBox = new IndexedErgoBox(
          header.height,
          Some(lastTx.id),
          Some(header.height + 1),
          None,
          lastTx.outputs.last,
          lastBoxIndex
        )
        malformedSpentOutput.insertExtraTry(Array.empty, Array(spentTerminalBox)).get
        malformedSpentOutput.close()

        val rebuiltAfterSpentOutput = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterSpentOutput).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterSpentOutput).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterSpentOutput).getLong,
          rebuiltAfterSpentOutput.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterSpentOutput.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        rebuiltAfterSpentOutput.closeStorage()

        val corrupted = HistoryStorage(dbSettings)
        corrupted.removeExtraTry(Array(numericBox.id)).get
        corrupted.close()

        val rebuilt = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuilt).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuilt).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuilt).getLong,
          rebuilt.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuilt.closeStorage()

        val invalidatedHeaderHistory = ErgoHistory.readOrGenerate(dbSettings)(context)
        invalidatedHeaderHistory.historyStorage.insertExtraTry(checkpointMetadata, checkpointObjects).get
        invalidatedHeaderHistory.historyStorage.insert(
          Array(invalidatedHeaderHistory.validityKey(header.id) -> Array(0.toByte)),
          org.ergoplatform.modifiers.BlockSection.emptyArray
        ).get
        invalidatedHeaderHistory.closeStorage()

        val rebuiltAfterInvalidatedHeader = ErgoHistory.readOrGenerate(dbSettings)(context)
        probe.ref ! ((
          ExtraIndexer.getIndex(ExtraIndexer.IndexedHeightKey, rebuiltAfterInvalidatedHeader).getInt,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalTxIndexKey, rebuiltAfterInvalidatedHeader).getLong,
          ExtraIndexer.getIndex(ExtraIndexer.GlobalBoxIndexKey, rebuiltAfterInvalidatedHeader).getLong,
          rebuiltAfterInvalidatedHeader.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexedHeaderIdKey))
        ))
        rebuiltAfterInvalidatedHeader.closeStorage()
        context.stop(self)
      }

      override def receive: Receive = Actor.emptyBehavior
    }))
    val preserved = probe.expectMsgType[IndexerState]
    preserved.indexedHeight shouldBe 1
    preserved.indexedHeaderId should not be empty
    preserved.globalTxIndex should be > 0L
    preserved.globalBoxIndex should be > 0L
    probe.expectMsg(true)
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
    probe.expectMsg((0, 0L, 0L, None))
  }

  property("binds cached transactions to the selected header") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val originalTransactions = history.bestBlockTransactionsAt(HEIGHT).get
    val staleTransactions = originalTransactions.copy(
      txs = history.bestBlockTransactionsAt(HEIGHT - 1).get.txs
    )

    indexer ! ForceRollback(HEIGHT - 1)
    awaitCondition(done)
    val branchState = IndexerState.fromHistory(_history)

    indexer ! GenerateBetterChainTip()
    awaitCondition(created)
    indexer ! CreateDB(HEIGHT + 1)
    awaitCondition(created)
    val selectedTransactions = history.bestBlockTransactionsAt(HEIGHT).get
    selectedTransactions.headerId should not be originalTransactions.headerId
    selectedTransactions.txs.head.id should not be staleTransactions.txs.head.id

    indexer ! CacheBlockTransactions(HEIGHT, staleTransactions)
    awaitCondition(created)
    indexer ! Index()
    awaitCondition(done)

    NumericTxIndex.getTxByNumber(history, branchState.globalTxIndex).map(_.id) shouldBe
      Some(selectedTransactions.txs.head.id)
    indexer ! Reset()
  }

  property("retries catch-up after a transient non-extending header") {
    indexer ! CreateDB(HEIGHT)
    awaitCondition(created)
    indexer ! DeferNextHeaderOnce(2)
    awaitCondition(created)
    indexer ! Index()

    org.ergoplatform.utils.untilTimeout(3.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT
      state.indexedHeaderId shouldBe history.bestHeaderIdAtHeight(HEIGHT)
    }
    indexer ! Reset()
  }

  property("retries catch-up when the selected block transactions are temporarily unavailable") {
    indexer ! CreateDB(HEIGHT)
    awaitCondition(created)
    indexer ! DeferBlockTransactionsOnce(2)
    awaitCondition(created)
    indexer ! Index()

    org.ergoplatform.utils.untilTimeout(3.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT
      state.indexedHeaderId shouldBe Some(fullChainHeaderAt(HEIGHT).id)
    }
    indexer ! Reset()
  }

  property("the production actor starts catch-up from the event stream") {
    val dbDir = Files.createTempDirectory("extra-indexer-production-start").toFile
    val dbSettings = initSettings.copy(
      directory = dbDir.getAbsolutePath,
      networkType = NetworkType.TestNet,
      nodeSettings = initSettings.nodeSettings.copy(extraIndex = true, headerChainDiff = 5000)
    )
    val setupProbe = TestProbe()(system)
    system.actorOf(Props(new Actor {
      override def preStart(): Unit = {
        val generatedHistory = ErgoHistory.readOrGenerate(dbSettings)(context)
        ChainGenerator.generate(5, dbDir, generatedHistory, None)
        setupProbe.ref ! generatedHistory
        context.stop(self)
      }

      override def receive: Receive = Actor.emptyBehavior
    }))
    val history = setupProbe.expectMsgType[ErgoHistory](30.seconds)
    IndexerState.fromHistory(history).indexedHeight shouldBe 0

    val productionIndexer = system.actorOf(Props(new ExtraIndexer(
      dbSettings.cacheSettings,
      dbSettings.chainSettings.addressEncoder
    )))
    val probe = TestProbe()(system)
    probe.send(productionIndexer, Identify("started"))
    probe.expectMsg(ActorIdentity("started", Some(productionIndexer)))
    system.eventStream.publish(ExtraIndexer.ReceivableMessages.StartExtraIndexer(history))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(history)
      state.indexedHeight shouldBe history.fullBlockHeight
      state.indexedHeaderId shouldBe Some(history.bestFullBlockOpt.get.header.id)
    }

    probe.watch(productionIndexer)
    system.stop(productionIndexer)
    probe.expectTerminated(productionIndexer)
    history.closeStorage()
  }

  property("transactions") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
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
    awaitCondition(done)
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
    awaitCondition(done)
    val (addresses, _, _, _, _) = manualIndex(HEIGHT)
    checkAddresses(addresses) shouldBe 0
    indexer ! Reset()
  }

  property("templates") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val (_, templates, _, _, _) = manualIndex(HEIGHT)
    checkTemplates(templates) shouldBe 0
    indexer ! Reset()
  }

  property("tokens") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val (_, _, indexedTokens, _, _) = manualIndex(HEIGHT)
    checkTokens(indexedTokens) shouldBe 0
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

  property("uses the production rollback point when the indexed tip becomes invalid") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val originalTipId = IndexerState.fromHistory(_history).indexedHeaderId.get

    indexer ! GenerateBetterChainTip()
    awaitCondition(created)
    indexer ! ExtendDB(HEIGHT + 1)
    awaitCondition(created)
    val branchPoint = fullChainHeaderAt(HEIGHT - 1)
    val replacementHeader = fullChainHeaderAt(HEIGHT)
    val replacementTip = fullChainHeaderAt(HEIGHT + 1)

    _history.historyStorage.insert(
      Array(_history.validityKey(originalTipId) -> Array(0.toByte)),
      org.ergoplatform.modifiers.BlockSection.emptyArray
    ).get
    val eventProbe = TestProbe()(system)
    eventProbe.send(indexer, RemoteBlockApplied(
      replacementHeader,
      history.getFullBlock(replacementHeader).get.transactions.map(_.id)
    ))
    eventProbe.send(indexer, RemoteBlockApplied(
      replacementTip,
      history.getFullBlock(replacementTip).get.transactions.map(_.id)
    ))
    eventProbe.send(indexer, Rollback(branchPoint.id))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 1
      state.indexedHeaderId shouldBe Some(replacementTip.id)
    }
    indexer ! Reset()
  }

  property("persists buffered catch-up rows before processing a reorg") {
    indexer ! CreateDB(HEIGHT)
    awaitCondition(created)
    val pauseProbe = TestProbe()(system)
    pauseProbe.send(indexer, PauseBufferedCatchUpAt(HEIGHT, Int.MaxValue, pauseProbe.ref))
    pauseProbe.expectMsg("configured")
    indexer ! Index()

    val bufferedState = pauseProbe.expectMsgType[IndexerState](10.seconds)
    bufferedState.indexedHeight shouldBe HEIGHT
    IndexerState.fromHistory(_history).indexedHeight shouldBe 0
    val originalTipId = bufferedState.indexedHeaderId.get

    indexer ! GenerateBetterChainTip()
    awaitCondition(created)
    indexer ! ExtendDB(HEIGHT + 1)
    awaitCondition(created)
    val branchPoint = fullChainHeaderAt(HEIGHT - 1)
    val replacementHeader = fullChainHeaderAt(HEIGHT)
    val replacementTip = fullChainHeaderAt(HEIGHT + 1)
    replacementHeader.id should not be originalTipId

    val eventProbe = TestProbe()(system)
    eventProbe.send(indexer, RemoteBlockApplied(
      replacementHeader,
      history.getFullBlock(replacementHeader).get.transactions.map(_.id)
    ))
    eventProbe.send(indexer, RemoteBlockApplied(
      replacementTip,
      history.getFullBlock(replacementTip).get.transactions.map(_.id)
    ))
    eventProbe.send(indexer, Rollback(branchPoint.id))

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 1
      state.indexedHeaderId shouldBe Some(replacementTip.id)
    }

    val expectedTransactions = (1 to HEIGHT + 1).flatMap(fullChainTransactionsAt(_).txs)
    val expectedBoxes = expectedTransactions.flatMap(_.outputs)
    val state = IndexerState.fromHistory(_history)
    state.globalTxIndex shouldBe expectedTransactions.size
    state.globalBoxIndex shouldBe expectedBoxes.size
    expectedTransactions.zipWithIndex.foreach { case (tx, index) =>
      NumericTxIndex.getTxByNumber(history, index).map(_.id) shouldBe Some(tx.id)
    }
    expectedBoxes.zipWithIndex.foreach { case (box, index) =>
      NumericBoxIndex.getBoxByNumber(history, index).map(_.id) shouldBe Some(bytesToId(box.id))
    }
    val (addresses, templates, indexedTokens, _, _) = manualIndex(HEIGHT + 1)
    checkAddresses(addresses) shouldBe 0
    checkTemplates(templates) shouldBe 0
    checkTokens(indexedTokens) shouldBe 0
    indexer ! Reset()
  }

  property("requests shutdown when final removal fails after partial rollback writes") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val spentInputId = bytesToId(fullChainTransactionsAt(HEIGHT).txs.flatMap(_.inputs).head.boxId)
    history.typedExtraIndexById[IndexedErgoBox](spentInputId).exists(_.isSpent) shouldBe true

    val probe = TestProbe()(system)
    indexer ! FailNextRollbackRemoval(probe.ref)
    awaitCondition(created)
    indexer ! ForceRollback(HEIGHT - 1)

    probe.expectMsg("shutdown-requested")
    ExtraIndexer.getIndex(ExtraIndexer.RollbackToKey, _history).getInt shouldBe HEIGHT - 1
    _history.historyStorage.invalidateExtraCache(Seq(spentInputId))
    history.typedExtraIndexById[IndexedErgoBox](spentInputId).exists(_.isSpent) shouldBe false
    indexer ! Reset()
  }

  property("recovers a reloaded checkpoint without waiting for block or rollback events") {
    indexer ! CreateDB(HEIGHT)
    indexer ! Index()
    awaitCondition(done)
    val originalTipId = IndexerState.fromHistory(_history).indexedHeaderId
    val branchPointId = fullChainHeaderAt(HEIGHT - 1).id

    indexer ! GenerateBetterChainTip()
    awaitCondition(created)
    indexer ! ExtendDB(HEIGHT + 1)
    awaitCondition(created)

    val replacementHeader = fullChainHeaderAt(HEIGHT)
    val replacementChild = fullChainHeaderAt(HEIGHT + 1)
    replacementHeader.id should not be originalTipId.get

    indexer ! Reload()
    awaitCondition(created)

    org.ergoplatform.utils.untilTimeout(10.seconds, 50.millis) {
      val state = IndexerState.fromHistory(_history)
      state.indexedHeight shouldBe HEIGHT + 1
      state.indexedHeaderId shouldBe Some(replacementChild.id)
    }

    val expectedTransactions = (1 to HEIGHT + 1).flatMap(fullChainTransactionsAt(_).txs)
    val expectedBoxes = expectedTransactions.flatMap(_.outputs)
    val state = IndexerState.fromHistory(_history)
    state.globalTxIndex shouldBe expectedTransactions.size
    state.globalBoxIndex shouldBe expectedBoxes.size
    expectedTransactions.zipWithIndex.foreach { case (tx, index) =>
      NumericTxIndex.getTxByNumber(history, index).map(_.id) shouldBe Some(tx.id)
    }
    expectedBoxes.zipWithIndex.foreach { case (box, index) =>
      NumericBoxIndex.getBoxByNumber(history, index).map(_.id) shouldBe Some(bytesToId(box.id))
    }

    val (addresses, templates, indexedTokens, _, _) = manualIndex(HEIGHT + 1)
    checkAddresses(addresses) shouldBe 0
    checkTemplates(templates) shouldBe 0
    checkTokens(indexedTokens) shouldBe 0

    val probe = TestProbe()(system)
    probe.send(indexer, Rollback(branchPointId))
    probe.send(indexer, GetLoadedState())
    val stateAfterLateRollback = probe.expectMsgType[IndexerState]
    stateAfterLateRollback.indexedHeight shouldBe HEIGHT + 1
    stateAfterLateRollback.indexedHeaderId shouldBe Some(replacementChild.id)
    IndexerState.fromHistory(_history) shouldBe stateAfterLateRollback
    indexer ! Reset()
  }
}
