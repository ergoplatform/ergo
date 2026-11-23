package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.{Algos, Args, ErgoSettingsReader}
import org.ergoplatform.utils.{ErgoCompilerHelpers, ErgoCorePropertyTest, ErgoStateContextHelpers, RandomWrapper}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalacheck.Gen
import org.ergoplatform.core.{bytesToId, bytesToVersion}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.{boxesHolderGen, validTransactionFromBoxes}
import org.ergoplatform.validation.ValidationResult.{Invalid, Valid}
import scorex.db.ByteArrayWrapper
import scorex.crypto.authds.avltree.batch.{Lookup, Remove}

import scala.collection.mutable
import scala.util.{Failure, Try}

class ErgoStateSpecification extends ErgoCorePropertyTest with ErgoCompilerHelpers with ErgoStateContextHelpers {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  property("applyModifier() - double spending") {
    forAll(boxesHolderGen, Gen.choose(1: Byte, 2: Byte)) { case (bh, version) =>
      val us = createUtxoState(bh, parameters)
      val ds = createDigestState(bytesToVersion(Array.fill(32)(100: Byte)), us.rootDigest)

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

    var (us, bh) = createUtxoState(settings)
    var ds = createDigestState(us.version, us.rootDigest)
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
    val settings = ErgoSettingsReader.read(Args.empty)
    val dir = createTempDir
    val rootHash = createUtxoState(settings)._1.rootDigest
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings).rootDigest
    rootHash shouldBe expectedRootHash
  }

  property("ErgoState.boxChanges() should generate operations in the same order") {
    var (us, bh) = createUtxoState(settings)
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
    val (_, bh) = createUtxoState(settings)
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new RandomWrapper(Some(seed)))._1
      whenever(txs.lengthCompare(2) > 0) {
        // valid transaction should spend the only existing genesis box
        ErgoState.boxChanges(txs).get._1.length shouldBe 1
        ErgoState.boxChanges(txs).get._1.head.key shouldBe emissionBox.id

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

  property("applyModifier() - output created in a block and spent twice in it") {
    forAll(boxesHolderGen) { bh =>
      val us = createUtxoState(bh, parameters)
      val ds = createDigestState(bytesToVersion(Array.fill(32)(100: Byte)), us.rootDigest)
      try {
        val validBlock = validFullBlock(None, us, bh)
        val txs = validBlock.transactions
        val spentInBlock = txs.zipWithIndex.flatMap { case (tx, i) =>
          tx.outputs.filter(o => txs.drop(i + 1).exists(_.inputs.exists(in => java.util.Arrays.equals(in.boxId, o.id))))
        }
        whenever(spentInBlock.nonEmpty) {
          val box = spentInBlock.head
          val secondSpend = validTransactionFromBoxes(IndexedSeq(box), issueNew = false)
          val invalidTxs = txs :+ secondSpend
          secondSpend.statelessValidity() shouldBe 'success

          // boxChanges() does not throw here: the first spend cancels the insertion,
          // so the second spend becomes a removal of a box the UTXO set does not hold
          val changes = ErgoState.stateChanges(invalidTxs).get
          changes.toRemove.map(r => ByteArrayWrapper(r.key)) should contain(ByteArrayWrapper(box.id))
          us.boxById(box.id) shouldBe None

          // proof with the non-membership path for the box in place of its removal
          val ops = changes.operations.map {
            case Remove(key) if java.util.Arrays.equals(key, box.id) => Lookup(key)
            case op => op
          }
          val (proofBytes, stateRoot) = us.persistentProver.avlProver.generateProofForOperations(ops).get
          val header = validBlock.header.copy(
            transactionsRoot = BlockTransactions.transactionsRoot(invalidTxs, validBlock.header.version),
            stateRoot = stateRoot,
            ADProofsRoot = ADProofs.proofDigest(proofBytes))
          val invalidBlock = ErgoFullBlock(
            header,
            BlockTransactions(header.id, validBlock.header.version, invalidTxs),
            validBlock.extension.copy(headerId = header.id),
            Some(ADProofs(header.id, proofBytes)))

          // UtxoState: the removal fails in the AVL+ tree (fbOperationFailed);
          // DigestState: it fails in ADProofs.verify, reported as a missing input (txBoxesToSpend)
          val usRoot = us.rootDigest
          val usVersion = us.store.lastVersionID.map(_.toSeq)
          val usResult = us.applyModifier(invalidBlock, None)(_ => ())
          usResult shouldBe 'failure
          val usError = usResult.failed.get
          usError.getMessage should include(s"Key ${Algos.encode(box.id)} does not exist")
          us.rootDigest shouldBe usRoot
          us.store.lastVersionID.map(_.toSeq) shouldBe usVersion
          us.applyModifier(validBlock, None)(_ => ()) shouldBe 'success

          val dsVersion = ds.store.lastVersionID.map(_.toSeq)
          val dsResult = ds.applyModifier(invalidBlock, None)(_ => ())
          dsResult shouldBe 'failure
          val dsError = dsResult.failed.get
          dsError.getMessage should include(s"Key ${Algos.encode(box.id)} does not exist")
          ds.store.lastVersionID.map(_.toSeq) shouldBe dsVersion
          ds.applyModifier(validBlock, None)(_ => ()) shouldBe 'success
        }
      } finally {
        us.closeStorage()
        ds.close()
      }
    }
  }

  property("ErgoState.stateChanges()") {
    val bh = createUtxoState(settings)._2
    val emissionBox = genesisBoxes.head

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new RandomWrapper(Some(seed)))._1
      whenever(txs.lengthCompare(1) > 0) {
        val changes = ErgoState.stateChanges(txs).get
        val removals = changes.toRemove
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head.key shouldEqual emissionBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.toAppend
        // sum of coins in outputs should equal to genesis value
        insertions.map(_.value).map(ErgoBoxSerializer.parseBytes).map(_.value).sum shouldBe emissionBox.value

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
    val expectedCost = 185160

    // successful validation
    ErgoState.execTransactions(txs, stateContext, settings.nodeSettings)(id => Try(boxes(ByteArrayWrapper(id)))) shouldBe Valid(expectedCost)

    // cost limit exception expected when crossing MaxBlockCost
    val tooManyTxs = (1 to 10).flatMap(_ => generateTxs)
    assert(
      ErgoState.execTransactions(tooManyTxs, stateContext, settings.nodeSettings)(id => Try(boxes(ByteArrayWrapper(id)))).errors.head.message.contains(
        "Accumulated cost of block transactions should not exceed <maxBlockCost>"
      )
    )

    // missing box in state
    ErgoState.execTransactions(txs, stateContext, settings.nodeSettings)(_ => Failure(new RuntimeException)).errors.head.message shouldBe
      "Every input of the transaction should be in UTXO. null"

    // tx validation should kick in and detect block height violation
    val invalidTx = invalidErgoTransactionGen.sample.get
    assert(
      ErgoState.execTransactions(txs :+ invalidTx, stateContext, settings.nodeSettings)(id => Try(boxes.getOrElse(ByteArrayWrapper(id), invalidTx.outputs.head)))
        .errors.head.message.startsWith("Transaction outputs should have creationHeight not exceeding block height.")
    )

    // no transactions are valid
    assert(ErgoState.execTransactions(Seq.empty, stateContext, settings.nodeSettings)(id => Try(boxes(ByteArrayWrapper(id)))).isValid)
  }

  property("ErgoState.execTransactions() - invalid 6.0 spending after 6.0 activation") {
    val sc = stateContext(1, 4, settings)

    val bx = new ErgoBoxCandidate(1000000000L, TrueTree, 0).toBox(bytesToId(Array.fill(32)(0.toByte)), 0)
    val tx1 = validTransactionFromBoxes(IndexedSeq(bx), outputsProposition = TrueTree)

    val ergoTree = compileSourceV6("sigmaProp(Global.serialize(2).size <= 0)", treeVersion = 3)

    val tx2 = validTransactionFromBoxes(tx1.outputs, outputsProposition = ergoTree)

    val tx3 = validTransactionFromBoxes(tx2.outputs, outputsProposition = TrueTree)

    val txs = IndexedSeq(tx1, tx2, tx3)

    val boxes = (IndexedSeq(bx) ++ txs.flatMap(_.outputs)).map(o => ByteArrayWrapper(o.id) -> o).toMap

    val execRes = ErgoState.execTransactions(txs, sc, settings.nodeSettings)(id => Try(boxes(ByteArrayWrapper(id))))
    execRes.isInstanceOf[Invalid] shouldBe true
    execRes.asInstanceOf[Invalid].errors.head.message.startsWith("Scripts of all transaction inputs should pass verification") shouldBe true

    val ergoTree2 = compileSourceV6("sigmaProp(Global.serialize(2).size > 0)", treeVersion = 3)

    val tx22 = validTransactionFromBoxes(tx1.outputs, outputsProposition = ergoTree2)

    val tx32 = validTransactionFromBoxes(tx22.outputs, outputsProposition = TrueTree)

    val txs2 = IndexedSeq(tx1, tx22, tx32)

    val boxes2 = (IndexedSeq(bx) ++ txs2.flatMap(_.outputs)).map(o => ByteArrayWrapper(o.id) -> o).toMap

    val execRes2 = ErgoState.execTransactions(txs2, sc, settings.nodeSettings)(id => Try(boxes2(ByteArrayWrapper(id))))
    execRes2.isInstanceOf[Valid[_]] shouldBe true
  }

}
