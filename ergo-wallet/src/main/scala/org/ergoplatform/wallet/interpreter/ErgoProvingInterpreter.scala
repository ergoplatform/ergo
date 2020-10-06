package org.ergoplatform.wallet.interpreter

import java.util

import org.ergoplatform._
import org.ergoplatform.utils.ArithUtils.{addExact, multiplyExact}
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import org.ergoplatform.wallet.secrets.SecretKey
import sigmastate.basics.SigmaProtocolPrivateInput
import org.ergoplatform.wallet.secrets.{ExtendedPublicKey, ExtendedSecretKey}
import scorex.util.encode.Base16
import sigmastate.eval.IRContextFactory
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utxo.CostTable

import scala.util.{Failure, Success, Try}

/**
  * A class which is holding secrets and signing transactions.
  * Signing a transaction means producing spending proofs for all of the input boxes of the transaction.
  *
  * This interpreter also acts as a wallet, in the sense that it is a vault holding user's secrets.
  *
  * There are two basic types of secrets, hierarchical deterministic keys corresponding to BIP-32
  * implementation, and also "primitive" keys, such as just secret exponent for a Schnorr signature
  * scheme done in Ergo.
  *
  * It is considered that there could be very many hierarchical deterministic keys (for example, if
  * we are talking about an exchange there could be thousands of them), and not so many primitive keys.
  * Optimizations are centered around this assumption.
  *
  *
  * @param secretKeys         secrets used by the prover
  * @param params             ergo network parameters at the moment of proving
  * @param cachedHdPubKeysOpt optionally, public keys corresponding to the BIP32-related secrets
  *                           (to not to recompute them)
  * @param irFactory          factory to create [[sigmastate.eval.IRContext]] instances
  */
class ErgoProvingInterpreter(val secretKeys: IndexedSeq[SecretKey],
                             params: ErgoLikeParameters,
                             val cachedHdPubKeysOpt: Option[IndexedSeq[ExtendedPublicKey]] = None)
                            (implicit irFactory: IRContextFactory)
  extends ErgoInterpreter(params) with ProverInterpreter {

  /**
    * Interpreter's secrets, in form of sigma protocols private inputs
    */
  val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = secretKeys.map(_.privateInput)

  /**
    * Only secrets corresponding to hierarchical deterministic scheme (BIP-32 impl)
    */
  val hdKeys: IndexedSeq[ExtendedSecretKey] = secretKeys.collect { case ek: ExtendedSecretKey => ek }

  /**
    * Only public keys corresponding to hierarchical deterministic scheme (BIP-32 impl)
    */
  val hdPubKeys: IndexedSeq[ExtendedPublicKey] = cachedHdPubKeysOpt match {
    case Some(cachedPubKeys) =>
      if (cachedPubKeys.length != hdKeys.length) {
        log.error(
          s"ErgoProverInterpreter: pubkeys and secrets of different sizes: ${cachedPubKeys.length} and ${secrets.length}"
        )
      }
      cachedPubKeys
    case None =>
      hdKeys.map(_.publicKey) // costly operation if there are many secret keys
  }

  /**
    * Produces updated instance of ErgoProvingInterpreter with a new secret included
    * @param secret - new secret to add
    * @return modified prover
    */
  def withNewExtendedSecret(secret: ExtendedSecretKey): (ErgoProvingInterpreter, ExtendedPublicKey) = {
    val newPk = secret.publicKey
    val sks   = secretKeys :+ secret
    val pks   = hdPubKeys :+ newPk
    log.info(s"New secret created, public image: ${Base16.encode(newPk.key.pkBytes)}")
    new ErgoProvingInterpreter(sks, params, Some(pks)) -> newPk
  }

  /**
    * Produces updated instance of ErgoProvingInterpreter with updated parameters
    * @param newParams - updated parameters
    * @return modified prover
    */
  def withNewParameters(newParams: ErgoLikeParameters): ErgoProvingInterpreter = {
    new ErgoProvingInterpreter(secretKeys, newParams, cachedHdPubKeysOpt)
  }

  /**
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext): Try[ErgoLikeTransaction] = {

    if (unsignedTx.inputs.length != boxesToSpend.length) {
      Failure(new Exception("Not enough boxes to spend"))
    } else if (unsignedTx.dataInputs.length != dataBoxes.length) {
      Failure(new Exception("Not enough data boxes"))
    } else {
      // Cost of transaction initialization: we should read and parse all inputs and data inputs,
      // and also iterate through all outputs to check rules, also we add some constant for interpreter initialization
      val initialCost: Long = addExact(
        CostTable.interpreterInitCost,
        multiplyExact(boxesToSpend.size, params.inputCost),
        multiplyExact(dataBoxes.size, params.dataInputCost),
        multiplyExact(unsignedTx.outputCandidates.size, params.outputCost)
      )

      boxesToSpend
        .zipWithIndex
        .foldLeft(Try(IndexedSeq[Input]() -> initialCost)) { case (inputsCostTry, (inputBox, boxIdx)) =>
          val unsignedInput = unsignedTx.inputs(boxIdx)
          require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

          val transactionContext = TransactionContext(boxesToSpend, dataBoxes, unsignedTx, boxIdx.toShort)

          inputsCostTry.flatMap { case (ins, totalCost) =>

            val context = new ErgoLikeContext(ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
              stateContext.sigmaLastHeaders,
              stateContext.sigmaPreHeader,
              transactionContext.dataBoxes,
              transactionContext.boxesToSpend,
              transactionContext.spendingTransaction,
              transactionContext.selfIndex,
              unsignedInput.extension,
              ValidationRules.currentSettings,
              params.maxBlockCost,
              totalCost
            )

            prove(inputBox.ergoTree, context, unsignedTx.messageToSign).flatMap { proverResult =>
              //prove() is accumulating cost under the hood, so proverResult.cost = totalCost + input check cost
              val newTC = proverResult.cost
              if (newTC > context.costLimit) {
                Failure(new Exception(s"Cost of transaction $unsignedTx exceeds limit ${context.costLimit}"))
              } else {
                Success((ins :+ Input(unsignedInput.boxId, proverResult)) -> newTC)
              }
            }
          }
        }
        .map { case (inputs, _) =>
          new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
        }
    }

  }

}

object ErgoProvingInterpreter {

  def apply(secrets: IndexedSeq[SecretKey], params: ErgoLikeParameters): ErgoProvingInterpreter = {
    new ErgoProvingInterpreter(secrets, params)(ErgoInterpreter.DefaultIRContextFactory)
  }

  def apply(rootSecret: ExtendedSecretKey, params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(IndexedSeq(rootSecret), params)(ErgoInterpreter.DefaultIRContextFactory)

}
