package org.ergoplatform.nodeView.wallet

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.ErgoStateContext
import scapi.sigma.DLogProtocol.DLogProverInput
import scapi.sigma.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import sigmastate.AvlTreeData
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigmastate.utxo.CostTable

import scala.util.Try



class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] = dlogSecrets ++ dhSecrets

  private lazy val dlogSecrets: Seq[DLogProverInput] = ErgoWallet.secretsFromSeed(seed).map(DLogProverInput.apply)

  private lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())

  lazy val dlogPubkeys = dlogSecrets.map(_.publicImage)

  def sign(unsignedTx: UnsignedErgoTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           stateContext: ErgoStateContext): Option[ErgoTransaction] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length)
    val inputs = unsignedTx.inputs.zip(boxesToSpend).map { case (unsignedInput, inputBox) =>
      require(unsignedInput.boxId.sameElements(inputBox.id))

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
  }.toOption //todo: handle errors
}
