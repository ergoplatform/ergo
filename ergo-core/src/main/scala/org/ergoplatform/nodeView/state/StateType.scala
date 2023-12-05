package org.ergoplatform.nodeView.state

import org.ergoplatform.nodeView.state.StateType.StateTypeCode


sealed trait StateType {
  def stateTypeCode: StateTypeCode

  def stateTypeName: String
  def requireProofs: Boolean

  /**
    * @return whether UTXO set is fully stored in a mode
    */
  def holdsUtxoSet: Boolean = !requireProofs

  override def toString: String = stateTypeName
}

object StateType {
  type StateTypeCode = Byte

  case object Utxo extends StateType {
    override val stateTypeCode: StateTypeCode = 0: Byte
    override val stateTypeName: String = "utxo"
    override val requireProofs: Boolean = false
  }

  case object Digest extends StateType {
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

}
