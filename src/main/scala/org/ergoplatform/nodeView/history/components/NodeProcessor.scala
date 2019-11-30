package org.ergoplatform.nodeView.history.components

import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.NetworkTimeProvider

/**
  * A component proving an access to a node configuration.
  */
trait NodeProcessor {

  protected val timeProvider: NetworkTimeProvider

  protected val settings: ErgoSettings

}
