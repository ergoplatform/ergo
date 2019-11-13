package org.ergoplatform.modifiers.mempool

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import scorex.util.ModifierId

/**
  * Transaction and its cost.
  *
  * Please note that the cost is context-dependent, thus do not store instances of
  * this class for long time.
  *
  * @param tx - a transaction
  * @param cost - cost of the transaction
  */
case class CostedTransaction(tx: ErgoTransaction, cost: Long) {
  lazy val id: ModifierId = tx.id

  override def equals(obj: Any): Boolean = obj match {
    case c: CostedTransaction => c.id == id
    case _ => false
  }

  override def hashCode(): Int = tx.hashCode()

}

object CostedTransaction {
  import ErgoTransaction.{transactionEncoder, transactionDecoder}

  implicit val costedTransactionEncoder: Encoder[CostedTransaction] = { ctx =>
    Json.obj(
      "transaction" -> ctx.tx.asJson,
      "cost" -> ctx.cost.asJson
    )
  }

  implicit val costedTransactionDecoder: Decoder[CostedTransaction] = { c =>
    for {
      transaction <- c.downField("transaction").as[ErgoTransaction]
      cost <- c.downField("cost").as[Long]
    } yield CostedTransaction(transaction, cost)
  }

}
