package org.ergoplatform.nodeView.state

import org.ergoplatform.settings.{ErgoSettings, VotingSettings}
import scorex.crypto.authds.ADDigest

/**
  * Constants that do not change when state version changes
  *
  * @param settings          - node settings
  */
case class StateConstants(settings: ErgoSettings) {

  lazy val keepVersions: Int = settings.nodeSettings.keepVersions
  lazy val votingSettings: VotingSettings = settings.chainSettings.voting

  lazy val genesisStateDigest: ADDigest = settings.chainSettings.genesisStateDigest
}
