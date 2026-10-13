package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoCorePropertyTest

class UtxoSetStatsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  property("utxoSetStats reports correct box counts and conserves store bytes") {
    val boxCount = 64
    val bh = boxesHolderGenOfSize(boxCount).sample.get
    val us = createUtxoState(bh, parameters)

    val stats = us.utxoSetStats().get

    // live UTXO set: inserted boxes plus the special (infinity) sentinel leaf
    stats.liveBoxes shouldBe (bh.size + 1).toLong
    stats.liveInternalNodes should be > 0L
    stats.treeHeight should be > 0

    // physical store agrees with the live tree (single committed version, no stale nodes)
    stats.leafRecords shouldBe stats.liveBoxes
    stats.internalRecords shouldBe stats.liveInternalNodes
    stats.leafValueBytes shouldBe stats.liveBoxValueBytes

    // box payloads account for every inserted box (sentinel leaf may add a little)
    val insertedBytes = bh.sortedBoxes.toSeq.map(_.bytes.length.toLong).sum
    stats.liveBoxValueBytes should be >= insertedBytes

    // classification partitions every record and conserves both counts and bytes
    stats.totalRecords shouldBe (stats.leafRecords + stats.internalRecords + stats.otherRecords)
    stats.totalValueBytes shouldBe
      (stats.leafRecordBytes + stats.internalRecordBytes + stats.otherRecordBytes)

    // metadata / index records (top-node keys, block-id index, state context, ...) are present
    // and not miscounted as tree nodes
    stats.otherRecords should be >= 2L
  }
}
