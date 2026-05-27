package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.util.Extensions._
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * Version-agnostic wallet progress for scanning a bootstrapped UTXO snapshot.
  */
final case class UtxoSnapshotScanStatus(snapshotHeight: Int,
                                        snapshotBlockId: ModifierId,
                                        manifestDepth: Int,
                                        nextSubtreeIndex: Int,
                                        totalSubtrees: Int,
                                        completed: Boolean)

object UtxoSnapshotScanStatusSerializer extends ErgoSerializer[UtxoSnapshotScanStatus] {

  override def serialize(obj: UtxoSnapshotScanStatus, w: Writer): Unit = {
    w.putUInt(obj.snapshotHeight)
    w.putBytes(idToBytes(obj.snapshotBlockId))
    w.putUInt(obj.manifestDepth)
    w.putUInt(obj.nextSubtreeIndex)
    w.putUInt(obj.totalSubtrees)
    w.putBoolean(obj.completed)
  }

  override def parse(r: Reader): UtxoSnapshotScanStatus = {
    UtxoSnapshotScanStatus(
      snapshotHeight = r.getUInt().toIntExact,
      snapshotBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize)),
      manifestDepth = r.getUInt().toIntExact,
      nextSubtreeIndex = r.getUInt().toIntExact,
      totalSubtrees = r.getUInt().toIntExact,
      completed = r.getByte() != 0
    )
  }
}
