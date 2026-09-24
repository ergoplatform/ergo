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
import scorex.db.LDBFactory
import scorex.util.{bytesToId, idToBytes}

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

  property("checker finds duplicated data inputs in synthetic history") {
    val dir = createTempDir
    val indexStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/objects")

    val dupBoxId = scorex.util.Random.randomBytes(32)

    try {
      // valid block at height 1 - two unique data inputs
      val validTx = mkTx(Seq(scorex.util.Random.randomBytes(32), scorex.util.Random.randomBytes(32)))
      // invalid block at height 2 - same data input twice
      val invalidTx = mkTx(Seq(dupBoxId, dupBoxId))

      Seq(validTx, invalidTx).zipWithIndex.foreach { case (tx, idx) =>
        val height = idx + 1
        val txs = Seq(tx)
        val header = defaultHeaderGen.sample.get.copy(
          version = Header.InitialVersion,
          height = height,
          transactionsRoot = BlockTransactions.transactionsRoot(txs, Header.InitialVersion))
        val bt = BlockTransactions(header.id, Header.InitialVersion, txs)
        // storing modifiers in the same way HistoryStorage does
        objectsStore.insert(header.serializedId, HistoryModifierSerializer.toBytes(header))
        objectsStore.insert(bt.serializedId, HistoryModifierSerializer.toBytes(bt))
        indexStore.insert(heightIdsKey(height), idToBytes(header.id))
      }
    } finally {
      indexStore.close()
      objectsStore.close()
    }

    val report = DuplicateDataInputsChecker.check(dir.getAbsolutePath, progressEvery = 0)

    report.chainHeight shouldBe 2
    report.blocksChecked shouldBe 2
    report.forkBlocksChecked shouldBe 0
    report.missingSections shouldBe 0
    report.unparsedBlocks shouldBe 0
    report.txsChecked shouldBe 2
    report.txsWithDataInputs shouldBe 2
    report.violations.size shouldBe 1
    report.bestChainViolations.size shouldBe 1

    val violation = report.violations.head
    violation.height shouldBe 2
    violation.inBestChain shouldBe true
    violation.duplicatedDataInputs shouldBe Seq(bytesToId(dupBoxId))
  }

  property("checker reports no violations for clean synthetic history") {
    val dir = createTempDir
    val indexStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${dir.getAbsolutePath}/history/objects")

    try {
      val tx = mkTx(Seq(scorex.util.Random.randomBytes(32)))
      val txs = Seq(tx)
      val header = defaultHeaderGen.sample.get.copy(
        version = Header.InitialVersion,
        height = 1,
        transactionsRoot = BlockTransactions.transactionsRoot(txs, Header.InitialVersion))
      val bt = BlockTransactions(header.id, Header.InitialVersion, txs)
      objectsStore.insert(header.serializedId, HistoryModifierSerializer.toBytes(header))
      objectsStore.insert(bt.serializedId, HistoryModifierSerializer.toBytes(bt))
      indexStore.insert(heightIdsKey(1), idToBytes(header.id))
    } finally {
      indexStore.close()
      objectsStore.close()
    }

    val report = DuplicateDataInputsChecker.check(dir.getAbsolutePath, progressEvery = 0)

    report.chainHeight shouldBe 1
    report.blocksChecked shouldBe 1
    report.txsChecked shouldBe 1
    report.violations shouldBe empty
  }

}
