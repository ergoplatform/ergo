package org.ergoplatform.api

/*
import io.circe._
import io.circe.generic.semiauto._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction.{Nonce, Value}
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBox
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

case class TransactionView(id: Option[String],
                           inputs: IndexedSeq[TransactionInputView],
                           outputs: IndexedSeq[TransactionOutputView]) {

  def toTransaction: AnyoneCanSpendTransaction = {
    AnyoneCanSpendTransaction(inputs.map(_.nonce), outputs.map(_.value))
  }

}

object TransactionView {

  def apply(tx: AnyoneCanSpendTransaction): TransactionView =
    TransactionView(
      Option(tx.encodedId),
      (tx.boxIdsToOpen zip tx.from).map(TransactionInputView.apply),
      tx.newBoxes.toIndexedSeq.map(TransactionOutputView.apply))

  implicit val inputEncoder: Encoder[TransactionInputView] = deriveEncoder[TransactionInputView]
  implicit val outputEncoder: Encoder[TransactionOutputView] = deriveEncoder[TransactionOutputView]
  implicit val encoder: Encoder[TransactionView] = deriveEncoder[TransactionView]
}

case class TransactionInputView(id: Option[String], nonce: Nonce, signature: String)

object TransactionInputView {

  def apply(nonceById: (ADKey, Nonce)): TransactionInputView = {
    val (id, nonce) = nonceById
    TransactionInputView(Some(idFromADKey(id)), nonce, signature = "")
  }

  def idFromADKey(id: ADKey): String = Algos.encode(id)

}

case class TransactionOutputView(id: Option[String], script: String, value: Value)

object TransactionOutputView {

  def apply(box: AnyoneCanSpendNoncedBox): TransactionOutputView =
    TransactionOutputView(Some(idFromBox(box)), script = "", value = box.value)

  def idFromBox(box: AnyoneCanSpendNoncedBox): String = Base58.encode(box.id)

}
*/