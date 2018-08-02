package org.ergoplatform.nodeView.wallet

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.ErgoStateContext
import scapi.sigma.DLogProtocol.DLogProverInput
import scapi.sigma.SigmaProtocolPrivateInput
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigmastate.utxo.CostTable

import scala.util.Try


/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates few secrets from a seed
  *
  *
  * @param seed
  * @param maxCost
  */

//todo: storing seed in class parameters is not very secure choice. However, storing seed in a config file is even
// more problematic

//todo: maxCost should set to block limit
class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] = dlogSecrets

  private lazy val dlogSecrets: Seq[DLogProverInput] =
    ErgoProvingInterpreter.secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dlogPubkeys = dlogSecrets.map(_.publicImage)

  def sign(unsignedTx: UnsignedErgoTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           stateContext: ErgoStateContext): Try[ErgoTransaction] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length)
    val inputs = unsignedTx.inputs.zip(boxesToSpend).map { case (unsignedInput, inputBox) =>
      require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

      val context =
        ErgoLikeContext(
          stateContext.height + 1,
          AvlTreeData(stateContext.digest, 32),
          boxesToSpend,
          unsignedTx,
          inputBox,
          ContextExtension.empty)

      val proverResult = prove(inputBox.proposition, context, unsignedTx.messageToSign).get
      Input(unsignedInput.boxId, proverResult)
    }

    ErgoTransaction(inputs, unsignedTx.outputCandidates)
  }
}


object ErgoProvingInterpreter {
  def secretsFromSeed(seedStr: String): IndexedSeq[BigInteger] = {
    (1 to 4).map { i =>
      BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i + seedStr))
    }
  }
}