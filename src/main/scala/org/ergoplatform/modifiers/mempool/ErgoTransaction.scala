package org.ergoplatform.modifiers.mempool

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.ErgoLikeTransaction.flattenedTxSerializer
import org.ergoplatform.ErgoTransactionValidator.verifier
import org.ergoplatform.api.TransactionView
import org.ergoplatform._
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import sigmastate.serialization.{Serializer => SSerializer}
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.{Failure, Success, Try}


case class ErgoStateContext(height: Long, lastUtxoDigest: ADDigest)

case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends Transaction with ErgoLikeTransactionTemplate[Input] with MempoolModifier with ScorexLogging {

  override type IdType = ModifierId

  override lazy val id: ModifierId = ModifierId @@ Blake2b256.hash(messageToSign)

  //todo: testnet1 - implement

  /**
    * statelessValidity is checking whether aspects of a transaction is valid which do not require the state to check.
    *
    * @return Success(Unit) if transaction is valid, Failure(e) if transaction is invalid, with respect to
    *         an error encapsulated in the exception "e".
    */
  def statelessValidity: Try[Unit] =
    Try {
      require(inputs.nonEmpty, s"No inputs in transaction $toString")
      require(inputs.size <= Short.MaxValue, s"Too much inputs in transaction $toString")
      require(outputCandidates.size <= Short.MaxValue, s"Too much outputCandidates in transaction $toString")
    }

  /**
    *
    * @param boxesToSpend
    * @param blockchainState
    * @return total coimputation cost
    */
  def statefulValidity(boxesToSpend: IndexedSeq[ErgoBox], blockchainState: ErgoStateContext): Try[Long] = Try {
    require(boxesToSpend.size == inputs.size, s"boxesToSpend.size ${boxesToSpend.size} != inputs.size ${inputs.size}")

    val lastUtxoDigest = AvlTreeData(blockchainState.lastUtxoDigest, ErgoBox.BoxId.size)

    val txCost = boxesToSpend.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)

      assert(box.id sameElements input.boxId)

      val proof = input.spendingProof

      val proverExtension = inputs(idx).spendingProof.extension

      val context =
        ErgoLikeContext(blockchainState.height, lastUtxoDigest, boxesToSpend, this, box, proverExtension)

      val scriptCost: Long = verifier.verify(box.proposition, context, proof, messageToSign) match {
        case Success((res, cost)) =>
          if (!res) return Failure(new Exception(s"Validation failed for input #$idx"))
          else cost
        case Failure(e) =>
          log.warn(s"Invalid transaction $toString: ", e)
          return Failure(e)
      }
      accCost + scriptCost
    }
    txCost
  }

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
