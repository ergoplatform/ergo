package org.ergoplatform.nodeView.state

import akka.actor.ActorRef
import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.ErgoSettings

/**
  * Constants that do not change with state version changes
  *
  * @param nodeViewHolderRef - actor ref of node view holder
  * @param settings          - node settings
  */
case class StateConstants(nodeViewHolderRef: Option[ActorRef], settings: ErgoSettings) {
  lazy val emission = settings.emission
  lazy val genesisEmissionBox: ErgoBox = ErgoState.genesisEmissionBox(emission)
  lazy val keepVersions: Int = settings.nodeSettings.keepVersions
}
