package org.ergoplatform.settings

import scorex.core.validation.ValidationState

trait ValidatedConfig {
  val validate: ValidationState[Unit]
}
