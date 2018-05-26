package org.ergoplatform.modifiers.mempool

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.api.TransactionView
import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction, ErgoLikeTransactionTemplate, Input}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.serialization.{Serializer => SSerializer}
import scorex.core.transaction.Transaction
import scorex.crypto.hash.Blake2b256
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.Try


case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends Transaction with ErgoLikeTransactionTemplate[Input] with MempoolModifier {

  override type IdType = ModifierId

  override lazy val id: ModifierId = ModifierId @@ Blake2b256.hash(messageToSign)

  //todo: testnet1 - implement
  def semanticValidity: Try[Unit] = ???

  override type M = ErgoTransaction

  override def serializer: Serializer[ErgoTransaction] = ErgoTransaction.serializer
}


object ErgoTransaction {
  implicit val jsonEncoder: Encoder[ErgoTransaction] = TransactionView(_).asJson

  object serializer extends Serializer[ErgoTransaction] with SSerializer[ErgoTransaction, ErgoTransaction] {
    override def toBytes(tx: ErgoTransaction): Array[Byte] =
      flattenedTxSerializer.toBytes(tx.inputs, tx.outputCandidates)

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoTransaction, Consumed) = {
      val ((inputs, outputCandidates), consumed) = flattenedTxSerializer.parseBody(bytes, pos)
      ErgoTransaction(inputs, outputCandidates) -> consumed
    }
  }
}
