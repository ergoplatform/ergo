package org.ergoplatform.api

import io.circe._
import io.circe.generic.semiauto._
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey


//todo: testnet1 - implement all the functionalities below!

case class TransactionView(id: Option[String],
                           inputs: IndexedSeq[TransactionInputView],
                           outputs: IndexedSeq[TransactionOutputView]) {

  def toTransaction: ErgoTransaction = ???

}

object TransactionView {

  def apply(tx: ErgoTransaction): TransactionView = ???

  implicit val inputEncoder: Encoder[TransactionInputView] = deriveEncoder[TransactionInputView]
  implicit val outputEncoder: Encoder[TransactionOutputView] = deriveEncoder[TransactionOutputView]
  implicit val encoder: Encoder[TransactionView] = deriveEncoder[TransactionView]
}

case class TransactionInputView(id: Option[String])

object TransactionInputView {
  def idFromADKey(id: ADKey): String = Algos.encode(id)
}

case class TransactionOutputView(id: Option[String])

object TransactionOutputView {
  def idFromBox(box: ErgoBoxCandidate): String = ???
}
