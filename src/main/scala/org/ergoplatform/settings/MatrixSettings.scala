package org.ergoplatform.settings

case class PendingAnnouncementsSettings(maxEntries: Int = 256,
                                        maxBytes: Long = 4194304L,
                                        perPeer: Int = 32,
                                        replayPerParent: Int = 64,
                                        ttlMs: Long = 120000L) {
  require(ttlMs > 0, "matrix.pendingAnnouncements.ttlMs must be positive")
  require(maxEntries > 0, "matrix.pendingAnnouncements.maxEntries must be positive")
  require(maxBytes > 0, "matrix.pendingAnnouncements.maxBytes must be positive")
  require(replayPerParent > 0, "matrix.pendingAnnouncements.replayPerParent must be positive")
  require(perPeer > 0, "matrix.pendingAnnouncements.perPeer must be positive")
}

case class MatrixSettings(pendingAnnouncements: PendingAnnouncementsSettings =
                          PendingAnnouncementsSettings())
