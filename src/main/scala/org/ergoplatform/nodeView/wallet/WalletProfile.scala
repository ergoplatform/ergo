package org.ergoplatform.nodeView.wallet

import scorex.util.ScorexLogging

/**
  * Hierarchy of possible wallet profile options. Wallet profile indicates intended use case for the node wallet
  */
sealed trait WalletProfile {

  /**
    * @return name of the wallet profile
    */
  def label: String

  /**
    * @return size of wallet scripts (P2PK and mining) Bloom filter. If wallet is going to use more scripts, efficiency
    *         of the filter sharply decreasing
    */
  def scriptsFilterSize: Int

  /**
    * @return size of Bloom filter on top of unspent wallet outputs. With outputs above the limit, efficiency
    *         of the filter sharply decreasing
    */
  def outputsFilterSize: Int
}

object WalletProfile extends ScorexLogging {

  /**
    * Wallet profile for ordinary single-user use case, consumes minimum RAM
    */
  case object User extends WalletProfile {
    override val label = "user"
    override def scriptsFilterSize: Int = 1000
    override def outputsFilterSize: Int = 10000
  }

  /**
    * Wallet profile for exchange with a lot of users, consumes ~20 MB RAM for Bloom filters only
    */
  case object Exchange extends WalletProfile {
    override val label = "exchange"
    override def scriptsFilterSize: Int = 1000000
    override def outputsFilterSize: Int = 10000000
  }

  /**
    * Wallet profile for serving applications, caches and filers sizes are between User and Exchange profiles values
    */
  case object AppServer extends WalletProfile {
    override val label = "appServer"
    override def scriptsFilterSize: Int = 50000
    override def outputsFilterSize: Int = 2000000
  }

  def fromLabel(label: String): WalletProfile = {
    label match {
      case s: String if s == User.label => User
      case s: String if s == Exchange.label => Exchange
      case s: String if s == AppServer.label => AppServer
      case a =>
        log.error(s"For wallet profiles, only ${User.label}, ${Exchange.label}, ${AppServer.label} are expected, $a given")
        ???
    }
  }

}
