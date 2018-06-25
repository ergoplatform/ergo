package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest

import scala.util.Random

class ErgoStateSpecification extends ErgoPropertyTest {

  property("stateContext should be the same for Utxo and Diget states") {
    var (us, bh) = createUtxoState()
    var ds = createDigestState(us.version, us.rootHash)
    var lastBlockOpt: Option[Header] = None
    requireEqualStateContexts(us.stateContext, ds.stateContext, 0)
    forAll { seed: Int =>
      val blBh = validFullBlockWithBlockHolder(lastBlockOpt, us, bh, new Random(seed))
      val block = blBh._1
      bh = blBh._2
      ds = ds.applyModifier(block).get
      us = us.applyModifier(block).get
      lastBlockOpt = Some(block.header)
      requireEqualStateContexts(us.stateContext, ds.stateContext, block.header.height + 1)
    }
  }

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(None)
    val dir = createTempDir
    val rootHash = createUtxoState()._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings).rootHash
    rootHash shouldBe expectedRootHash
  }

  property("ErgoState.boxChanges() should generate operations in the same order") {
    var (us, bh) = createUtxoState()

    forAll { seed: Int =>
      val blBh = validFullBlockWithBlockHolder(None, us, bh, new Random(seed))
      val block = blBh._1
      bh = blBh._2
      us = us.applyModifier(block).get

      val changes1 = ErgoState.boxChanges(block.transactions)
      val changes2 = ErgoState.boxChanges(block.transactions)
      changes1._1 shouldBe changes2._1
      changes1._2 shouldBe changes2._2
    }
  }

  property("ErgoState.boxChanges() double spend attempt") {
    var (us: UtxoState, bh) = createUtxoState()
    bh.boxes.size shouldBe 1
    val genesisBox = bh.boxes.head._2

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new Random(seed))._1
      whenever(txs.lengthCompare(2) > 0) {
        // valid transaction should spend the only existing genesis box
        ErgoState.boxChanges(txs)._1.length shouldBe 1
        ErgoState.boxChanges(txs)._1.head shouldBe genesisBox.id

        // second transaction input should be be an input, created by the first transaction
        val inputToDoubleSpend = txs(1).inputs.head
        txs.head.outputs.find(_.id sameElements inputToDoubleSpend.boxId) should not be None
        val doubleSpendTx = txs.last.copy(inputs = inputToDoubleSpend +: txs.last.inputs.tail)
        val invalidTxs = txs.dropRight(1) :+ doubleSpendTx
        invalidTxs.length shouldBe txs.length
        invalidTxs.count(_.inputs.contains(inputToDoubleSpend)) shouldBe 2

        ErgoState.boxChanges(invalidTxs)._1.length shouldBe 2
      }
    }
  }


  property("ErgoState.stateChanges()") {
    var (us: UtxoState, bh) = createUtxoState()
    bh.boxes.size shouldBe 1
    val genesisBox = bh.boxes.head._2

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new Random(seed))._1
      whenever(txs.lengthCompare(1) > 0) {
        val changes = ErgoState.stateChanges(txs)
        val removals = changes.toRemove
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head.boxId shouldEqual genesisBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.toAppend
        // sum of coins in outputs should equal to genesis value
        insertions.map(_.box.value).sum shouldBe genesisBox.value

        // if output was spend and then created - it is in both toInsert and toRemove
        val changesRev = ErgoState.stateChanges(txs.reverse)
        val removalsRev = changesRev.toRemove
        val insertionsRev = changesRev.toAppend
        removalsRev.length should be > removals.length
        insertionsRev.length should be > insertions.length
      }
    }
  }

  def requireEqualStateContexts(s1: ErgoStateContext, s2: ErgoStateContext, expectedHeight: Int): Unit = {
    s1.height shouldBe expectedHeight
    s1.height shouldBe s2.height
    s1.digest shouldEqual s2.digest
  }

}
