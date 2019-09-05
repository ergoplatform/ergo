package org.ergoplatform.settings

final case class PoPowSettings(prove: Boolean,
                               bootstrap: Boolean,
                               minProofsToCheck: Int,
                               params: PoPowParams)