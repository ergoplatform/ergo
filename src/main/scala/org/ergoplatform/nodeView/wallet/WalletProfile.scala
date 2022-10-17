package org.ergoplatform.nodeView.wallet

import scorex.util.ScorexLogging


sealed trait WalletProfile {
  def label: String

  def scriptsFilterSize: Int

  def outputsFilterSize: Int
}

object WalletProfile extends ScorexLogging {

  case object User extends WalletProfile {
    override val label = "user"
    override def scriptsFilterSize: Int = 1000
    override def outputsFilterSize: Int = 10000
  }

  case object Exchange extends WalletProfile {
    override val label = "exchange"
    override def scriptsFilterSize: Int = 1000000
    override def outputsFilterSize: Int = 10000000
  }

  case object AppServer extends WalletProfile {
    override val label = "appServer"
    override def scriptsFilterSize: Int = 200000
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


