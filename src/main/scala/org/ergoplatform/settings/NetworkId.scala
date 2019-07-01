package org.ergoplatform.settings

sealed trait NetworkId { val verboseName: String }

object NetworkId {

  def all: Seq[NetworkId] = Seq(MainNet, TestNet, DevNet)

  case object MainNet extends NetworkId { val verboseName: String = "mainnet" }
  case object TestNet extends NetworkId { val verboseName: String = "testnet" }
  case object DevNet extends NetworkId { val verboseName: String = "devnet" }

}
