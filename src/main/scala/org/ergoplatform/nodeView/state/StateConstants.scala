package org.ergoplatform.nodeView.state

import akka.actor.ActorRef
import org.ergoplatform.ErgoBox

/**
  * Constants, that do not change with state version changes
  *
  * @param nodeViewHolderRef - actor ref of node view holder
  * @param genesisEmissionBox - genesis emission box
  */
case class StateConstants(nodeViewHolderRef: Option[ActorRef], genesisEmissionBox: ErgoBox)
