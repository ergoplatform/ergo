package org.ergoplatform

import java.util

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.basics.SigmaProtocolPrivateInput
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}

import scala.util.{Failure, Success, Try}

class SecretsProvingInterpreter(params: ErgoLikeParameters, s: IndexedSeq[SigmaProtocolPrivateInput[_, _]]) (implicit IR: IRContext)
  extends ErgoInterpreter(params) with ProverInterpreter {

  val secrets = s

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
            else Success((ins :+ Input(unsignedInput.boxId, proverResult)) -> newTC)
          }
        }
      }
      .map { case (inputs, _) =>
        new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
      }
  }
}
