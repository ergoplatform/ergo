package org.ergoplatform.tools

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{BlockTransactions, HistoryModifierSerializer}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{DataInput, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import scorex.db.{LDBKVStore, LDBFactory}
import scorex.util.{bytesToId, idToBytes}
import sigma.ast.{Constant, EvaluatedValue, SByte, SCollection, SType, SUnit}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.{SigmaByteWriter, SigmaSerializer, TypeSerializer}

class DuplicateDataInputsCheckerSpec extends ErgoCorePropertyTest with TestFileUtils {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  // same as HeadersProcessor.heightIdsKey and DuplicateDataInputsChecker.heightIdsKey
  private def heightIdsKey(height: Int): Array[Byte] = Algos.hash(Ints.toByteArray(height))

  private def mkTx(dataInputIds: Seq[Array[Byte]]): ErgoTransaction = {
    val input = Input(ADKey @@ scorex.util.Random.randomBytes(), emptyProverResult)
    val out = new ErgoBoxCandidate(10000000L, TrueTree, 0)
    val dataInputs = dataInputIds.map(id => DataInput(ADKey @@ id)).toIndexedSeq
    ErgoTransaction(IndexedSeq(input), dataInputs, IndexedSeq(out))
  }

  private def deepType(depth: Int): SCollection[SType] =
    SCollection[SType](if (depth <= 1) SByte else deepType(depth - 1))

  // builds an ErgoBoxCandidate with one register, writing the register value bytes directly
  private def boxWithRegister(writeValue: SigmaByteWriter => Unit): ErgoBoxCandidate = {
    val w = SigmaSerializer.startWriter()
    w.putULong(10000000L)        // value
    w.putBytes(TrueTree.bytes)   // ergoTree
    w.putUInt(0)                 // creationHeight
    w.putUByte(0)                // no tokens
    w.putUByte(1)                // one register
    writeValue(w)
    ErgoBoxCandidate.serializer.parse(SigmaSerializer.startReader(w.toBytes))
  }

  private def mkTxWithBox(box: ErgoBoxCandidate, ext: ContextExtension): ErgoTransaction = {
    val input = Input(ADKey @@ scorex.util.Random.randomBytes(), ProverResult(Array.emptyByteArray, ext))
    ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(box))
  }

  // stores modifiers in the same way HistoryStorage does
  private def storeBlock(objectsStore: LDBKVStore,
                         indexStore: LDBKVStore,
                         height: Int,
                         tx: ErgoTransaction): Unit = {
    val txs = Seq(tx)
    val header = defaultHeaderGen.sample.get.copy(
      version = Header.InitialVersion,
      height = height,
      transactionsRoot = BlockTransactions.transactionsRoot(txs, Header.InitialVersion))
    val bt = BlockTransactions(header.id, Header.InitialVersion, txs)
    objectsStore.insert(header.serializedId, HistoryModifierSerializer.toBytes(header))
    objectsStore.insert(bt.serializedId, HistoryModifierSerializer.toBytes(bt))
    indexStore.insert(heightIdsKey(height), idToBytes(header.id))
  }

  property("checker finds over-duplicated data inputs in synthetic history") {
    val dir = createTempDir
    val indexStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/objects")

    val boxId1 = scorex.util.Random.randomBytes(32)
    val boxId2 = scorex.util.Random.randomBytes(32)

    try {
      // valid block at height 1 - one pair of data inputs with the same box id (allowed)
      val validTx = mkTx(Seq(boxId1, boxId1))
      storeBlock(objectsStore, indexStore, 1, validTx)
      // invalid block at height 2 - the same box referred by three data inputs
      val invalidTx1 = mkTx(Seq(boxId1, boxId1, boxId1))
      storeBlock(objectsStore, indexStore, 2, invalidTx1)
      // invalid block at height 3 - two pairs of data inputs with the same box ids
      val invalidTx2 = mkTx(Seq(boxId1, boxId1, boxId2, boxId2))
      storeBlock(objectsStore, indexStore, 3, invalidTx2)
    } finally {
      indexStore.close()
      objectsStore.close()
    }

    val report = DuplicateDataInputsChecker.check(dir.getAbsolutePath, progressEvery = 0)

    report.chainHeight shouldBe 3
    report.blocksChecked shouldBe 3
    report.forkBlocksChecked shouldBe 0
    report.missingSections shouldBe 0
    report.unparsedBlocks shouldBe 0
    report.txsChecked shouldBe 3
    report.txsWithDataInputs shouldBe 3
    report.violations.size shouldBe 2
    report.bestChainViolations.size shouldBe 2
    report.typeViolations shouldBe empty

    val violation2 = report.violations.find(_.height == 2).get
    violation2.inBestChain shouldBe true
    violation2.overReferencedBoxes shouldBe Seq(bytesToId(boxId1))

    val violation3 = report.violations.find(_.height == 3).get
    violation3.inBestChain shouldBe true
    violation3.overReferencedBoxes.toSet shouldBe Set(bytesToId(boxId1), bytesToId(boxId2))
  }

  property("checker reports no violations for clean synthetic history") {
    val dir = createTempDir
    val indexStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/objects")

    try {
      val tx = mkTx(Seq(scorex.util.Random.randomBytes(32)))
      storeBlock(objectsStore, indexStore, 1, tx)
    } finally {
      indexStore.close()
      objectsStore.close()
    }

    val report = DuplicateDataInputsChecker.check(dir.getAbsolutePath, progressEvery = 0)

    report.chainHeight shouldBe 1
    report.blocksChecked shouldBe 1
    report.txsChecked shouldBe 1
    report.violations shouldBe empty
    report.typeViolations shouldBe empty
  }

  property("checker finds tightened sigma rule violations in synthetic history") {
    val dir = createTempDir
    val indexStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/objects")

    try {
      // block at height 1: SUnit in a register and in a context extension value
      val unitConstant: EvaluatedValue[SType] = Constant[SUnit.type]((), SUnit)
      val unitBox = boxWithRegister(w => TypeSerializer.serialize(SUnit, w))
      val tx1 = mkTxWithBox(unitBox, ContextExtension(Map(1.toByte -> unitConstant)))
      storeBlock(objectsStore, indexStore, 1, tx1)

      // block at height 2: zero-width collection Coll[Unit] in a register
      // (two elements declared, but zero-width elements occupy no data bytes)
      val collUnitBox = boxWithRegister { w =>
        TypeSerializer.serialize(SCollection(SUnit), w)
        w.putUShort(2)
      }
      val tx2 = mkTxWithBox(collUnitBox, ContextExtension.empty)
      storeBlock(objectsStore, indexStore, 2, tx2)

      // block at height 3: type descriptor nested deeper than MaxTypeDepth in a register
      // (canonical encoding of Coll^N[Byte] reaches deserializer depth N - 2)
      val depth = DuplicateDataInputsChecker.MaxTypeDepth + 3
      val deepBox = boxWithRegister { w =>
        TypeSerializer.serialize(deepType(depth), w)
        w.putUShort(0) // empty collection, no data bytes
      }
      val tx3 = mkTxWithBox(deepBox, ContextExtension.empty)
      storeBlock(objectsStore, indexStore, 3, tx3)
    } finally {
      indexStore.close()
      objectsStore.close()
    }

    val report = DuplicateDataInputsChecker.check(dir.getAbsolutePath, progressEvery = 0)

    report.chainHeight shouldBe 3
    report.blocksChecked shouldBe 3
    report.unparsedBlocks shouldBe 0
    report.missingSections shouldBe 0
    report.violations shouldBe empty

    report.typeViolations.forall(_.inBestChain) shouldBe true

    val byHeight = report.typeViolations.groupBy(_.height)

    byHeight(1).map(_.location).toSet shouldBe
      Set("input #0 context extension var 1", "output #0 register R4")
    byHeight(1).forall(_.rule.contains("SUnit")) shouldBe true

    byHeight(2).map(_.rule).toSet shouldBe Set(
      "type contains SUnit (tightened rule #1019 CheckV6Type)",
      "collection with zero-width element type (new rule #1020 CheckZeroWidthCollection)")

    byHeight(3).size shouldBe 1
    byHeight(3).head.rule should include("MaxTypeDepth")
    byHeight(3).head.location shouldBe "output #0 register R4"
  }

}
