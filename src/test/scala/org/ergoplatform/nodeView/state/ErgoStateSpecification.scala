package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.{Args, ErgoSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, RandomWrapper}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalacheck.Gen
import scorex.core.bytesToVersion
import scorex.core.validation.ValidationResult.Valid
import scorex.db.ByteArrayWrapper

import scala.collection.mutable
import scala.util.{Failure, Try}

class ErgoStateSpecification extends ErgoPropertyTest {

  property("applyModifier() - double spending") {
    forAll(boxesHolderGen, Gen.choose(1: Byte, 2: Byte)) { case (bh, version) =>
      val us = createUtxoState(bh, parameters)
      val ds = createDigestState(bytesToVersion(Array.fill(32)(100: Byte)), us.rootHash, parameters)

      val validBlock = validFullBlock(None, us, bh)
      val dsTxs = validBlock.transactions ++ validBlock.transactions
      ErgoState.stateChanges(dsTxs) shouldBe 'failure

      val dsRoot = BlockTransactions.transactionsRoot(dsTxs, version)
      val dsHeader = validBlock.header.copy(transactionsRoot = dsRoot)
      val bt = BlockTransactions(dsHeader.id, version, dsTxs)
      val doubleSpendBlock = ErgoFullBlock(dsHeader, bt, validBlock.extension, validBlock.adProofs)

      us.applyModifier(doubleSpendBlock, None)(_ => ()) shouldBe 'failure
      us.applyModifier(validBlock, None)(_ => ()) shouldBe 'success

      ds.applyModifier(doubleSpendBlock, None)(_ => ()) shouldBe 'failure
      ds.applyModifier(validBlock, None)(_ => ()) shouldBe 'success
    }
  }

  property("stateContext should be the same for Utxo and Digest states") {
    def requireEqualStateContexts(s1: ErgoStateContext, s2: ErgoStateContext, lastHeaders: Seq[Header]): Unit = {
      s1.currentHeight shouldBe lastHeaders.headOption.map(_.height).getOrElse(0)
      s1.currentHeight shouldBe s2.currentHeight
      s1.previousStateDigest shouldEqual s2.previousStateDigest
      s1.lastHeaders shouldEqual s2.lastHeaders
      s1.lastHeaders shouldEqual lastHeaders
      s1.currentParameters shouldEqual s2.currentParameters
      s1.votingData shouldEqual s2.votingData
      s1.genesisStateDigest shouldBe s2.genesisStateDigest
    }

    var (us, bh) = createUtxoState(parameters)
    var ds = createDigestState(us.version, us.rootHash, parameters)
    var lastBlocks: Seq[ErgoFullBlock] = Seq()
    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(lastBlocks.headOption, us, bh, new RandomWrapper(Some(seed)))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block, None)(_ => ()).get
      us = us.applyModifier(block, None)(_ => ()).get
      lastBlocks = block +: lastBlocks
      requireEqualStateContexts(us.stateContext, ds.stateContext, lastBlocks.map(_.header))
    }
  }

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(Args.empty)
    val dir = createTempDir
    val rootHash = createUtxoState(parameters)._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings).rootHash
    rootHash shouldBe expectedRootHash
  }

  property("ErgoState.boxChanges() should generate operations in the same order") {
    var (us, bh) = createUtxoState(parameters)
    var parentOpt: Option[ErgoFullBlock] = None

    forAll { seed: Int =>
      val blBh = validFullBlockWithBoxHolder(parentOpt, us, bh, new RandomWrapper(Some(seed)))
      val block = blBh._1
      parentOpt = Some(block)
      bh = blBh._2
      us = us.applyModifier(block, None)(_ => ()).get

      val changes1 = ErgoState.boxChanges(block.transactions).get
      val changes2 = ErgoState.boxChanges(block.transactions).get
      changes1._1 shouldBe changes2._1
      changes1._2 shouldBe changes2._2
    }
  }

  property("ErgoState.boxChanges() double spend attempt") {
    val (_, bh) = createUtxoState(parameters)
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new RandomWrapper(Some(seed)))._1
      whenever(txs.lengthCompare(2) > 0) {
        // valid transaction should spend the only existing genesis box
        ErgoState.boxChanges(txs).get._1.length shouldBe 1
        ErgoState.boxChanges(txs).get._1.head._2.key shouldBe emissionBox.id

        // second transaction input should be an input created by the first transaction
        val inputToDoubleSpend = txs(1).inputs.head
        txs.head.outputs.find(b => java.util.Arrays.equals(b.id, inputToDoubleSpend.boxId)) should not be None
        val doubleSpendTx = txs.last.copy(inputs = inputToDoubleSpend +: txs.last.inputs.tail)
        val invalidTxs = txs.dropRight(1) :+ doubleSpendTx
        invalidTxs.length shouldBe txs.length
        invalidTxs.count(_.inputs.contains(inputToDoubleSpend)) shouldBe 2

        ErgoState.boxChanges(invalidTxs).get._1.length shouldBe 2
      }
    }
  }

  property("ErgoState.stateChanges()") {
    val bh = createUtxoState(parameters)._2
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new RandomWrapper(Some(seed)))._1
      whenever(txs.lengthCompare(1) > 0) {
        val changes = ErgoState.stateChanges(txs).get
        val removals = changes.toRemove
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head._2.key shouldEqual emissionBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.toAppend
        // sum of coins in outputs should equal to genesis value
        insertions.map(_._2.value).map(ErgoBoxSerializer.parseBytes).map(_.value).sum shouldBe emissionBox.value

        // if output was spend and then created - it is in both toInsert and toRemove
        val changesRev = ErgoState.stateChanges(txs.reverse).get
        val removalsRev = changesRev.toRemove
        val insertionsRev = changesRev.toAppend
        removalsRev.length should be > removals.length
        insertionsRev.length should be > insertions.length
      }
    }
  }

  property("ErgoState.execTransactions()") {
    val bh = BoxHolder(genesisBoxes)
    def generateTxs =
      (1 to 15).foldLeft(mutable.WrappedArray.newBuilder[ErgoTransaction]) { case (txAcc, _) =>
        val (transactions, _) = validTransactionsFromBoxes(10000, bh.boxes.values.toSeq, new RandomWrapper())
        val allBoxIds = bh.boxes.keys.toSet
        val txsFromBoxesOnly = transactions.filter { tx =>
          tx.inputs.map(i => ByteArrayWrapper(i.boxId)).forall(allBoxIds.contains) &&
            tx.dataInputs.map(i => ByteArrayWrapper(i.boxId)).forall(allBoxIds.contains)
        }
        txAcc ++= txsFromBoxesOnly
      }.result()

    val txs = generateTxs
    val boxes = bh.boxes
    val stateContext = emptyStateContext
    val expectedCost = 535995

    // successful validation
    ErgoState.execTransactions(txs, stateContext)((_, id) => Try(boxes(ByteArrayWrapper(id)))) shouldBe Valid(expectedCost)

    // cost limit exception expected when crossing MaxBlockCost
    val tooManyTxs = txs ++ generateTxs
    assert(
      ErgoState.execTransactions(tooManyTxs, stateContext)((_, id) => Try(boxes(ByteArrayWrapper(id)))).errors.head.message.contains(
        "Estimated execution cost 23533 exceeds the limit 23009"
      )
    )

    // missing box in state
    ErgoState.execTransactions(txs, stateContext)((_, _) => Failure(new RuntimeException)).errors.head.message shouldBe
      "Every input of the transaction should be in UTXO. null"

    // tx validation should kick in and detect block height violation
    val invalidTx = invalidErgoTransactionGen.sample.get
    assert(
      ErgoState.execTransactions(txs :+ invalidTx, stateContext)((_, id) => Try(boxes.getOrElse(ByteArrayWrapper(id), invalidTx.outputs.head)))
        .errors.head.message.startsWith("Transaction outputs should have creationHeight not exceeding block height.")
    )

    // no transactions are valid
    assert(ErgoState.execTransactions(Seq.empty, stateContext)((_, id) => Try(boxes(ByteArrayWrapper(id)))).isValid)
  }
}
