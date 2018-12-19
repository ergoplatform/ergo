package org.ergoplatform.settings

case class NiPoPowSettings(enabled: Boolean,
                           minProofsToCheck: Int,
                           m: Int,
                           k: Int,
                           k1: Int,
                           d: Double)
