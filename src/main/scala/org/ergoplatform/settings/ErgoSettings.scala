package org.ergoplatform.settings

import scorex.core.settings.Settings

trait ErgoSettings extends Settings {
  val dataDir: String = dataDirOpt.getOrElse("/tmp/ergo")
  //todo read all from settings file
  val ADState: Boolean = false
  val verifyTransactions: Boolean = false
  val poPoWBootstrap: Boolean = false
  val blocksToKeep: Int = 10
  val minimalSuffix: Int = 10
}
