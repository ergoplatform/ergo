package org.ergoplatform.nodeView.state

import io.circe.Encoder

import scala.reflect.ClassTag

sealed trait StateType {
  def stateTypeName: String
  def requireProofs: Boolean
  override def toString: String = stateTypeName
}

object StateType {

  case object Utxo extends StateType {
    def stateTypeName: String = "utxo"
    val requireProofs: Boolean = false
  }

  case object Digest extends StateType {
    def stateTypeName: String = "digest"
    val requireProofs: Boolean = true
  }

  type UtxoType = Utxo.type
  type DigestType = Digest.type

  val values: Seq[StateType] = Seq(Utxo, Digest)

  /** This class allows to check the correspondence between concrete instances of [[StateType]] and [[ErgoState]]
    */
  sealed trait Evidence[ST <: StateType, S <: ErgoState[S]]

  implicit final object UtxoEvidence extends Evidence[UtxoType, UtxoState]
  implicit final object DigestEvidence extends Evidence[DigestType, DigestState]

  def forState[T <: ErgoState[T]](implicit tag: ClassTag[T]): StateType = {
    if (classOf[DigestState].isAssignableFrom(tag.runtimeClass)) Digest else Utxo
  }

  /** This is for json representation of [[StateType]] instances
    */
  implicit val jsonEncoder: Encoder[StateType] =
    Encoder.encodeString.contramap[StateType](_.stateTypeName)

}
