package org.ergoplatform.settings

import scorex.core.NodeViewModifier.ModifierId
import scorex.core.settings.Settings

trait ErgoSettings extends Settings {
  val dataDir: String = dataDirOpt.getOrElse("/tmp/ergo")
  val maxRollback: Int = 1000
  val genesisId: ModifierId = ???

}
