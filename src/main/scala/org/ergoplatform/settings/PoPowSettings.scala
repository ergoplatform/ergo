package org.ergoplatform.settings

final case class PoPowSettings(enabled: Boolean,
                               minProofsToCheck: Int,
                               params: PoPowParams)