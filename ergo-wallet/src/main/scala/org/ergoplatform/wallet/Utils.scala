package org.ergoplatform.wallet

import org.ergoplatform.{ErgoAddress, ErgoBox, ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigmastate.Values.TrueLeaf
import sigmastate.eval.Extensions._
import sigmastate.eval._

object Utils {

  /**
    * Assembles unsigned payment transaction.
    *
    * @param recipientAddress - payment recipient address
    * @param changeAddress - chnage recipient address
    * @param transferAmt - amount of ERGs to transfer
    * @param feeAmt - fee amount
    * @param changeAmt - amount to return back to `changeAddress`
    * @param inputIds - identifiers of inputs to be used in transaction
    * @param currentHeight - current blockchain height
    * @return unsigned transaction
    */
  def paymentTransaction(recipientAddress: ErgoAddress,
                         changeAddress: ErgoAddress,
                         transferAmt: Long,
                         feeAmt: Long,
                         changeAmt: Long,
                         inputIds: Array[String],
                         currentHeight: Int): UnsignedErgoLikeTransaction = {
    val payTo = new ErgoBoxCandidate(
      transferAmt,
      recipientAddress.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val fee = new ErgoBoxCandidate(
      feeAmt,
      TrueLeaf.toSigmaProp,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val change = new ErgoBoxCandidate(
      changeAmt,
      changeAddress.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )
    val unsignedInputs = inputIds
      .flatMap { id =>
        Base16.decode(id)
          .map(x => new UnsignedInput(ADKey @@ x))
          .toOption
      }
      .toIndexedSeq

    new UnsignedErgoLikeTransaction(
      unsignedInputs,
      IndexedSeq(),
      IndexedSeq(payTo, change, fee)
    )
  }

  implicit class EitherOpsFor211[+A, +B](val source: Either[A, B]) extends AnyVal {

    def mapRight[B1](f: B => B1): Either[A, B1] = source match {
      case Right(b) => Right(f(b))
      case _        => source.asInstanceOf[Either[A, B1]]
    }

    def flatMapRight[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = source match {
      case Right(b) => f(b)
      case _        => source.asInstanceOf[Either[A1, B1]]
    }
  }
}
