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
    val expectedRootHash = ErgoState.generateGenesisDigestState(dir, settings.nodeSettings).rootHash
    rootHash shouldBe expectedRootHash
  }

  property("ErgoState.boxChanges()") {
    var (us: UtxoState, bh) = createUtxoState()
    bh.boxes.size shouldBe 1
    val genesisBox = bh.boxes.head._2

    forAll { seed: Int =>
      val txs = validTransactionsFromBoxHolder(bh, new Random(seed))._1
      whenever(txs.lengthCompare(1) > 0) {
        val changes = ErgoState.boxChanges(txs)
        val removals = changes.operations.filter(_.isInstanceOf[Removal]).map(_.asInstanceOf[Removal])
        // should remove the only genesis box from the state
        removals.length shouldBe 1
        removals.head.boxId shouldEqual genesisBox.id
        // number of inputs should be more than 1 - we create boxes and spend them in the same block
        txs.flatMap(_.inputs).length should be > 1

        val insertions = changes.operations.filter(_.isInstanceOf[Insertion]).map(_.asInstanceOf[Insertion])
        // sum of coins in outputs should equal to genesis value
        insertions.map(_.box.value).sum shouldBe genesisBox.value

        val changesRev = ErgoState.boxChanges(txs.reverse)
        val removalsRev = changesRev.operations.filter(_.isInstanceOf[Removal]).map(_.asInstanceOf[Removal])
        val insertionsRev = changesRev.operations.filter(_.isInstanceOf[Insertion]).map(_.asInstanceOf[Insertion])
        removalsRev shouldEqual removals
        insertionsRev.sortBy(_.toString) shouldEqual insertions.sortBy(_.toString)
      }
    }

  }
}
