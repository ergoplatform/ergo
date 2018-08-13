package org.ergoplatform.nodeView.wallet

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.ErgoStateContext
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.SigmaProtocolPrivateInput
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigmastate.utxo.CostTable

import scala.util.{Failure, Success, Try}


/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates few secrets from a seed and sign a transaction (against input boxes to spend and
  * blockchain state) by using the secrets (no additional inputs, e.g. hash function preimages required in scripts,
  * are supported. Here, signing a transaction means spending proofs generation for all of its input boxes.
  *
  * @param seed    - a secret seed value
  * @param maxCost - max cost of all the transaction input scripts combined (the prover refuses to sign a transaction
  *                if the total cost exceeds the limit)
  */

// todo: storing seed in class parameters is not very secure choice. However, storing seed in a config file like we are
// doing now is even more problematic

//todo: maxCost should be set to block limit, currently total sum per tx is not calculated
class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  override lazy val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = dlogSecrets

  private lazy val dlogSecrets: IndexedSeq[DLogProverInput] =
    ErgoProvingInterpreter.secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dlogPubkeys: IndexedSeq[ProveDlog] = dlogSecrets.map(_.publicImage)

  def sign(unsignedTx: UnsignedErgoTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           stateContext: ErgoStateContext): Try[ErgoTransaction] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length)

    unsignedTx.inputs.zip(boxesToSpend).foldLeft(Try(IndexedSeq[Input]())) {
      case (inputsTry, (unsignedInput, inputBox)) =>
        require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

        inputsTry match {
          case Success(ins) =>
            val context =
              ErgoLikeContext(
                stateContext.height + 1,
                AvlTreeData(stateContext.digest, 32),
                boxesToSpend,
                unsignedTx,
                inputBox,
                ContextExtension.empty)

            prove(inputBox.proposition, context, unsignedTx.messageToSign).map { proverResult =>
              Input(unsignedInput.boxId, proverResult) +: ins
            }

          case f: Failure[IndexedSeq[Input]] => f
        }
    }.map { inputs =>
      ErgoTransaction(inputs, unsignedTx.outputCandidates)
    }
  }.flatten
}


object ErgoProvingInterpreter {
  val Secrets = 4

  def secretsFromSeed(seedStr: String): IndexedSeq[BigInteger] = {
    (1 to Secrets).map { i =>
      BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i + seedStr))
    }
  }
}