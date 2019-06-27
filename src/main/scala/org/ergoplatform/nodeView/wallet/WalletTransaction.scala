package org.ergoplatform.nodeView.wallet

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.{DataInput, ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import scorex.core.serialization.ScorexSerializer
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

final case class WalletTransaction(tx: ErgoTransaction, inclusionHeight: Int) {

  def id: ModifierId = tx.id

}

object WalletTransaction {

  import ErgoTransaction._

  implicit val jsonEncoder: Encoder[WalletTransaction] = { obj =>
    obj.tx.asJson.deepMerge(Json.obj("inclusionHeight" -> obj.inclusionHeight.asJson))
  }

  implicit val jsonDecoder: Decoder[WalletTransaction] = { c =>
    for {
      inputs <- c.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- c.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- c.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
      inclusionHeight <- c.downField("inclusionHeight").as[Int]
    } yield WalletTransaction(new ErgoTransaction(inputs, dataInputs, outputsWithIndex.map(_._1)), inclusionHeight)
  }

}

object WalletTransactionSerializer extends ScorexSerializer[WalletTransaction] {

  override def serialize(wtx: WalletTransaction, w: Writer): Unit = {
    val txBytes = wtx.tx.bytes
    w.putInt(wtx.inclusionHeight)
    w.putInt(txBytes.length)
    w.putBytes(txBytes)
  }

  override def parse(r: Reader): WalletTransaction = {
    val inclusionHeight = r.getInt()
    val txBytesLen = r.getInt()
    val tx = ErgoTransactionSerializer.parseBytes(r.getBytes(txBytesLen))
    WalletTransaction(tx, inclusionHeight)
  }

}
