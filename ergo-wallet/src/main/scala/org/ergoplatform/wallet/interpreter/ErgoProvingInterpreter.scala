package org.ergoplatform.wallet.interpreter

import java.util

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import org.ergoplatform.wallet.secrets.{ExtendedSecretKey, SecretKey}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import sigmastate.eval.{CompiletimeIRContext, IRContext}
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}

import scala.util.{Failure, Success, Try}

/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates some number of secrets (the number is provided via "dlogSecretsNumber" setting in the
  * "wallet" section) from a seed and sign a transaction (against input boxes to spend and
  * blockchain state) by using the secrets (no additional inputs, e.g. hash function preimages required in scripts,
  * are supported. Here, signing a transaction means spending proofs generation for all of its input boxes.
  *
  * @param secretKeys - secrets used by the prover
  * @param params     - ergo network parameters
  */
class ErgoProvingInterpreter(val secretKeys: IndexedSeq[SecretKey], params: ErgoLikeParameters)
                            (implicit IR: IRContext)
  extends ErgoInterpreter(params) with ProverInterpreter {

  val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = secretKeys.map(_.key)

  val hdKeys: IndexedSeq[ExtendedSecretKey] =
    secretKeys.filter(_.isInstanceOf[ExtendedSecretKey]).map(_.asInstanceOf[ExtendedSecretKey])

  val pubKeys: IndexedSeq[SigmaProtocolCommonInput[_]] =
    secrets.map(_.publicImage.asInstanceOf[SigmaProtocolCommonInput[_]])

  val hdPubKeys: IndexedSeq[ProveDlog] = hdKeys.map(_.publicImage)

  /**
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext): Try[ErgoLikeTransaction] = {
    if (unsignedTx.inputs.length != boxesToSpend.length) Failure(new Exception("Not enough boxes to spend"))
    else if (unsignedTx.dataInputs.length != dataBoxes.length) Failure(new Exception("Not enough data boxes"))
    else boxesToSpend
      .zipWithIndex
      .foldLeft(Try(IndexedSeq[Input]() -> 0L)) { case (inputsCostTry, (inputBox, boxIdx)) =>
        val unsignedInput = unsignedTx.inputs(boxIdx)
        require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

        val transactionContext = TransactionContext(boxesToSpend, dataBoxes, unsignedTx, boxIdx.toShort)

        inputsCostTry.flatMap { case (ins, totalCost) =>

          // Cost of transaction initialization: we should read and parse all inputs and data inputs,
          // and also iterate through all outputs to check rules
          val initialCost: Long = boxesToSpend.size * params.inputCost +
          dataBoxes.size * params.dataInputCost +
          unsignedTx.outputCandidates.size * params.outputCost

          val context = new ErgoLikeContext(ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
            stateContext.sigmaLastHeaders,
            stateContext.sigmaPreHeader,
            transactionContext.dataBoxes,
            transactionContext.boxesToSpend,
            transactionContext.spendingTransaction,
            transactionContext.selfIndex,
            ContextExtension.empty,
            ValidationRules.currentSettings,
            params.maxBlockCost,
            initialCost
          )

          prove(inputBox.ergoTree, context, unsignedTx.messageToSign).flatMap { proverResult =>
            val newTC = totalCost + proverResult.cost
            if (newTC > context.costLimit)
              Failure(new Exception(s"Cost of transaction $unsignedTx exceeds limit ${context.costLimit}"))
            else
              Success((ins :+ Input(unsignedInput.boxId, proverResult)) -> newTC)
          }
        }
      }
      .map { case (inputs, _) =>
        new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
      }
  }

}

object ErgoProvingInterpreter {

  def apply(secrets: IndexedSeq[SecretKey], params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(secrets, params)(new CompiletimeIRContext)

  def apply(rootSecret: ExtendedSecretKey, params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(IndexedSeq(rootSecret), params)(new CompiletimeIRContext)
}
