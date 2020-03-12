package org.ergoplatform.settings

sealed trait NetworkType {
  val verboseName: String
  def isMainNet: Boolean = false
}

object NetworkType {

  def all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet)

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  case object MainNet extends NetworkType {
    val verboseName: String = "mainnet"
    override def isMainNet: Boolean = true
  }

  case object TestNet extends NetworkType {
    val verboseName: String = "testnet"
  }

  case object DevNet extends NetworkType {
    val verboseName: String = "devnet"
  }

}
