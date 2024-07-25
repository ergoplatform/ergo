package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoLikeTransaction, Input, UnsignedErgoLikeTransaction}
import scorex.util.encode.Base16
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.interpreter.{ContextExtension, ProverResult}

/**
  * A naive Ergo prover implementation not performing transaction cost verification.
  *
  * @note this prover is only suitable for signing only small number of simple inputs,
  *       for inputs with complex scripts use `ErgoProvingInterpreter`
  */
object ErgoUnsafeProver {

  import org.ergoplatform.wallet.crypto.ErgoSignature._

  /**
    * Signs all inputs of a given `unsignedTx`, if all the inputs are associated with the same keypair.
    *
    * @note this method does not validate the cost of the given transaction
    * @return signed transaction
    */
  def prove(unsignedTx: UnsignedErgoLikeTransaction,
            sk: DLogProverInput): ErgoLikeTransaction = {
    val sig = ProverResult(sign(unsignedTx.messageToSign, sk.w), ContextExtension.empty)
    val inputs = unsignedTx.inputs.map { unsignedInput =>
      Input(unsignedInput.boxId, sig)
    }
    new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
  }

  /**
    * Signs all inputs of a given `unsignedTx`.
    *
    * @note this method does not validate the cost of the given transaction
    * @return signed transaction
    */
  def prove(unsignedTx: UnsignedErgoLikeTransaction,
            sks: scala.collection.Map[String, DLogProverInput]): ErgoLikeTransaction = {

    val inputs = unsignedTx.inputs.map { unsignedInput =>
      val inputId = Base16.encode(unsignedInput.boxId)

      val sk = sks(inputId)

      val sig = ProverResult(sign(unsignedTx.messageToSign, sk.w), ContextExtension.empty)
      Input(unsignedInput.boxId, sig)
    }
    new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
  }

}
