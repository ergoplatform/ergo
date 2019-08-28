package org.ergoplatform.settings

final case class PoPowSettings(enabled: Boolean,
                               minProofsToCheck: Int,
                               m: Int,
                               k: Int,
                               k1: Int,
                               d: Double)