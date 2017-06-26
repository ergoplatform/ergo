package org.ergoplatform.settings

import org.ergoplatform.modifiers.block.{ErgoFullBlock, ErgoHeader}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.settings.Settings

trait ErgoSettings extends Settings {
  val dataDir: String = dataDirOpt.getOrElse("/tmp/ergo")
  val maxRollback: Int = 1000

  //TODO ???
  lazy val genesisBlock: ErgoFullBlock = ErgoFullBlock(ErgoHeader(0.toByte, Array.fill(32)(0.toByte),
    Seq[Array[Byte]](), Array.fill(32)(0.toByte), ErgoFullBlock.calcTransactionsRootHash(Seq()), 0, 0), Seq())
  lazy val genesisId: ModifierId = genesisBlock.id

}
