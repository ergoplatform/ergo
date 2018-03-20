package org.ergoplatform.nodeView.state

import io.circe.Encoder

sealed trait StateType {
  def stateTypeName: String
  override def toString: String = stateTypeName
}

object StateType {

  case object Utxo extends StateType {
    def stateTypeName: String = "utxo"
  }

  case object Digest extends StateType {
    def stateTypeName: String = "digest"
  }

  type UtxoType = Utxo.type
  type DigestType = Digest.type

  val values: Seq[StateType] = Seq(Utxo, Digest)

  /** This class allows to check the correspondence between concrete instances of [[StateType]] and [[ErgoState]]
    */
  sealed trait Evidence[ST <: StateType, S <: ErgoState[S]]

  implicit object UtxoEvidence extends Evidence[UtxoType, UtxoState]
  implicit object DigestEvidence extends Evidence[DigestType, DigestState]

  /** This is for json representation of [[StateType]] instances
    */
  implicit val jsonEncoder: Encoder[StateType] =
    Encoder.encodeString.contramap[StateType](_.stateTypeName)

}
