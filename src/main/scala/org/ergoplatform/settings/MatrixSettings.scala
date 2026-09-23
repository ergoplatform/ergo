package org.ergoplatform.settings

case class PendingAnnouncementsSettings(maxEntries: Int = 256,
                                        maxBytes: Long = 4194304L,
                                        perPeer: Int = PendingAnnouncementsSettings.DefaultPerPeer,
                                        replayPerParent: Int = 64,
                                        ttlMs: Long = 120000L) {
  require(ttlMs > 0, "matrix.pendingAnnouncements.ttlMs must be positive")
  require(maxEntries > 0, "matrix.pendingAnnouncements.maxEntries must be positive")
  require(maxBytes > 0, "matrix.pendingAnnouncements.maxBytes must be positive")
  require(replayPerParent > 0, "matrix.pendingAnnouncements.replayPerParent must be positive")
  require(perPeer > 0, "matrix.pendingAnnouncements.perPeer must be positive")
}

object PendingAnnouncementsSettings {
  // Allow one ordering block's roots plus headroom for announcement timing.
  val DefaultPerPeer: Int = 2 * Parameters.SubsPerBlockDefault
}

case class MatrixSettings(pendingAnnouncements: PendingAnnouncementsSettings =
                          PendingAnnouncementsSettings())
