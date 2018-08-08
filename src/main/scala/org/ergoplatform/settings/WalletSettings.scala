package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

case class WalletSettings(seed: String, scanningInterval: FiniteDuration)