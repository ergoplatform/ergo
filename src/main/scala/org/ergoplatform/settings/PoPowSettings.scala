package org.ergoplatform.settings

import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationState}

final case class PoPowSettings(
  prove: Boolean,
  bootstrap: Boolean,
  minProofsToCheck: Int,
  params: PoPowParams
)