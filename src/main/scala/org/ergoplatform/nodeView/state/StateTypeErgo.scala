package org.ergoplatform.nodeView.state

import org.ergoplatform.nodeView.state.StateType.{Digest, Utxo}

object StateTypeErgo {
  type UtxoType = Utxo.type
  type DigestType = Digest.type

  val values: Seq[StateType] = Seq(Utxo, Digest)

  /** This class allows to check the correspondence between concrete instances of [[StateType]] and [[ErgoState]]
   */
  sealed trait Evidence[ST <: StateType, S <: ErgoState[S]]

  implicit final object UtxoEvidence extends Evidence[UtxoType, UtxoState]

  implicit final object DigestEvidence extends Evidence[DigestType, DigestState]

}
