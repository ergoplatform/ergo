package org.ergoplatform.settings

case class PendingAnnouncementsSettings(maxEntries: Int,
                                        maxBytes: Long,
                                        perPeer: Int,
                                        replayPerParent: Int,
                                        ttlMs: Long) {
  require(ttlMs > 0, "ergo.node.matrix.pendingAnnouncements.ttlMs must be positive")
  require(maxEntries > 0, "ergo.node.matrix.pendingAnnouncements.maxEntries must be positive")
  require(maxBytes > 0, "ergo.node.matrix.pendingAnnouncements.maxBytes must be positive")
  require(replayPerParent > 0, "ergo.node.matrix.pendingAnnouncements.replayPerParent must be positive")
  require(perPeer > 0, "ergo.node.matrix.pendingAnnouncements.perPeer must be positive")
}

case class MatrixSettings(pendingAnnouncements: PendingAnnouncementsSettings)
