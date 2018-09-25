package org.ergoplatform.nodeView.wallet

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Input}
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.SigmaProtocolPrivateInput
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.interpreter.{ContextExtension, CostedProverResult, ProverInterpreter}

import scala.util.{Failure, Success, Try}


/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates some number of secrets (the number is provided via "dlogSecretsNumber" setting in the
  * "wallet" section) from a seed and sign a transaction (against input boxes to spend and
  * blockchain state) by using the secrets (no additional inputs, e.g. hash function preimages required in scripts,
  * are supported. Here, signing a transaction means spending proofs generation for all of its input boxes.
  *
  * @param seed    - a secret seed value
  * @param maxCost - max cost of all the transaction input scripts combined (the prover refuses to sign a transaction
  *                if the total cost exceeds the limit)
  */

// todo: storing seed in class parameters is not very secure choice. However, storing seed in a config file like we are
// doing now is even more problematic

class ErgoProvingInterpreter(seed: String,
                             numOfSecrets: Int,
                             addressPrefix: Byte,
                             override val maxCost: Long = Constants.MaxBlockCost)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  require(numOfSecrets > 0, "non-positive number of secrets to generate")

  override lazy val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = dlogSecrets

  def secretsFromSeed(seedStr: String): IndexedSeq[BigInteger] = {
    (1 to numOfSecrets).map { i =>
      BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i + seedStr))
    }
  }

  private lazy val dlogSecrets: IndexedSeq[DLogProverInput] =
    secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dlogPubkeys: IndexedSeq[ProveDlog] = dlogSecrets.map(_.publicImage)

  def sign(unsignedTx: UnsignedErgoTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           stateContext: ErgoStateContext): Try[ErgoTransaction] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length)

    unsignedTx.inputs.zip(boxesToSpend).foldLeft(Try(IndexedSeq[Input]() -> 0L)) {
      case (inputsCostTry, (unsignedInput, inputBox)) =>
        require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

        inputsCostTry.flatMap { case (ins, totalCost) =>
          val context =
            ErgoLikeContext(
              stateContext.height + 1,
              AvlTreeData(stateContext.digest, 32),
              boxesToSpend,
              unsignedTx,
              inputBox,
              Metadata(addressPrefix),
              ContextExtension.empty)

          prove(inputBox.proposition, context, unsignedTx.messageToSign).flatMap { proverResult =>
            val newTC = totalCost + proverResult.cost
            if (newTC > maxCost) {
              Failure(new Exception(s"Computational cost of transaction $unsignedTx exceeds limit $maxCost"))
            } else {
              Success((Input(unsignedInput.boxId, proverResult) +: ins) -> 0L)
            }
          }
        }
    }.map { case (inputs, _) =>
      ErgoTransaction(inputs.reverse, unsignedTx.outputCandidates)
    }
  }.flatten
}
