package org.ergoplatform.nodeView.state

import akka.actor.ActorRef
import org.ergoplatform.settings.{ErgoSettings, VotingSettings}
import scorex.crypto.authds.ADDigest

/**
  * Constants that do not change with state version changes
  *
  * @param nodeViewHolderRef - actor ref of node view holder
  * @param settings          - node settings
  */
case class StateConstants(nodeViewHolderRef: Option[ActorRef], settings: ErgoSettings) {

  lazy val keepVersions: Int = settings.nodeSettings.keepVersions
  lazy val votingSettings: VotingSettings = settings.chainSettings.voting

  lazy val genesisStateDigest: ADDigest = settings.chainSettings.genesisStateDigest
}
