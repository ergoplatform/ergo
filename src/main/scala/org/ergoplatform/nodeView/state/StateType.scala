package org.ergoplatform.nodeView.state

import io.circe.Encoder
import org.ergoplatform.nodeView.state.StateType.StateTypeCode

import scala.reflect.ClassTag

sealed trait StateType {
  def stateTypeCode: StateTypeCode

  def allUtxos: Boolean
  def stateTypeName: String
  def requireProofs: Boolean
  override def toString: String = stateTypeName
}

object StateType {
  type StateTypeCode = Byte

  case object Utxo extends StateType {
    override val allUtxos: Boolean = true
    override val stateTypeCode: StateTypeCode = 0: Byte
    override val stateTypeName: String = "utxo"
    override val requireProofs: Boolean = false
  }

  case object Digest extends StateType {
    override val allUtxos: Boolean = false
    override val stateTypeCode: StateTypeCode = 1: Byte
    override val stateTypeName: String = "digest"
    override val requireProofs: Boolean = true
  }

  def fromCode(code: StateTypeCode): StateType = if (code == Utxo.stateTypeCode) {
    Utxo
  } else if (code == Digest.stateTypeCode) {
    Digest
  } else {
    throw new Exception(s"Unkown state type code $code")
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
