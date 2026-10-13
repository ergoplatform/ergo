package org.ergoplatform.nodeView.state

import org.ergoplatform.serialization.ManifestSerializer
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverManifest
import scorex.crypto.hash.Digest32

object SnapshotTestAccess {
  def dumpManifest(state: UtxoState,
                   height: Int,
                   manifestDepth: Byte): BatchAVLProverManifest[Digest32] = {
    state.dumpSnapshot(height, state.rootDigest.dropRight(1), manifestDepth).get
    val manifestId = state.snapshotsDb.readSnapshotsInfo.availableManifests(height)
    val manifestBytes = state.snapshotsDb.readManifestBytes(manifestId).get
    new ManifestSerializer(manifestDepth).parseBytes(manifestBytes)
  }
}
