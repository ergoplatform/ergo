package org.ergoplatform.settings

case class PendingAnnouncementsSettings(maxEntries: Int = 256,
                                        maxBytes: Long = 4194304L,
                                        perPeer: Int = 32) {
  require(maxEntries > 0, "matrix.pendingAnnouncements.maxEntries must be positive")
  require(maxBytes > 0, "matrix.pendingAnnouncements.maxBytes must be positive")
  require(perPeer > 0, "matrix.pendingAnnouncements.perPeer must be positive")
}

case class MatrixSettings(pendingAnnouncements: PendingAnnouncementsSettings =
                          PendingAnnouncementsSettings())
