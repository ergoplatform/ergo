package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.state.{Insertion, Removal}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest

import scala.util.Random


class ErgoStateSpecification extends ErgoPropertyTest {

  property("generateGenesisUtxoState & generateGenesisDigestState are compliant") {
    val settings = ErgoSettings.read(None)
    val dir = createTempDir
    val rootHash = createUtxoState()._1.rootHash
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings).rootHash
    rootHash shouldBe expectedRootHash
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
        val removals = changes.operations.filter(_.isInstanceOf[Removal]).map(_.asInstanceOf[Removal])
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head.boxId shouldEqual genesisBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.operations.filter(_.isInstanceOf[Insertion]).map(_.asInstanceOf[Insertion])
        // sum of coins in outputs should equal to genesis value
        insertions.map(_.box.value).sum shouldBe genesisBox.value

        val changesRev = ErgoState.stateChanges(txs.reverse)
        val removalsRev = changesRev.operations.filter(_.isInstanceOf[Removal]).map(_.asInstanceOf[Removal])
        val insertionsRev = changesRev.operations.filter(_.isInstanceOf[Insertion]).map(_.asInstanceOf[Insertion])
        removalsRev shouldEqual removals
        insertionsRev.sortBy(_.toString) shouldEqual insertions.sortBy(_.toString)
      }
    }

  }
}
