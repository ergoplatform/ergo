package org.ergoplatform.settings

import org.ergoplatform.ErgoAddressEncoder

sealed trait NetworkType {
  def verboseName: String
  def isMainNet: Boolean
  def isTestNet: Boolean
  def addressPrefix: Byte
}

object NetworkType {

  def all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet)

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  case object MainNet extends NetworkType {
    override val verboseName: String = "mainnet"
    override val isMainNet: Boolean = true
    override val isTestNet: Boolean = false
    override val addressPrefix: Byte = ErgoAddressEncoder.MainnetNetworkPrefix
  }

  case object TestNet extends NetworkType {
    override val verboseName: String = "testnet"
    override val isMainNet: Boolean = false
    override val isTestNet: Boolean = true
    override val addressPrefix: Byte = ErgoAddressEncoder.TestnetNetworkPrefix
  }


  // devnet which is starting from 5.0 activated since genesis block
  case object DevNet extends NetworkType {
    override val verboseName: String = "devnet"
    override val isMainNet: Boolean = false
    override val isTestNet: Boolean = false
    override val addressPrefix: Byte = 32
  }

  // used in tests only curently, devnet which is starting from 6.0 activated since genesis block
  case object DevNet60 extends NetworkType {
    override val verboseName: String = "devnet60"
    override val isMainNet: Boolean = false
    override val isTestNet: Boolean = false
    override val addressPrefix: Byte = 32
  }

}
