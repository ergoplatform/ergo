package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

case class BootstrapSettings(resourceUri: String, pollDelay: FiniteDuration)
