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

  val values: Seq[StateType] = Seq(Utxo, Digest)

  implicit val stateTypeEncoder: Encoder[StateType] =
    Encoder.encodeString.contramap[StateType](_.stateTypeName)
}


