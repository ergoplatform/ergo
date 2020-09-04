package org.ergoplatform.wallet.interpreter

import java.math.BigInteger
import java.util

import org.ergoplatform._
import org.ergoplatform.utils.ArithUtils.{addExact, multiplyExact}
import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.AvlTreeData
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.{DLogInteractiveProver, ProveDlog}
import sigmastate.basics.{DiffieHellmanTupleInteractiveProver, FirstProverMessage, ProveDHTuple}
import sigmastate.interpreter.{ContextExtension, HintsBag}
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter.TransactionHintsBag
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext}
import org.ergoplatform.wallet.secrets.SecretKey
import sigmastate.basics.SigmaProtocolPrivateInput
import org.ergoplatform.wallet.secrets.{ExtendedPublicKey, ExtendedSecretKey}
import scorex.util.encode.Base16
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utxo.CostTable
import special.collection.Coll
import special.sigma.{Header, PreHeader}

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
  * @param secretKeys - secrets used by the prover
  * @param params     - ergo network parameters at the moment of proving
  * @param cachedHdPubKeysOpt - optionally, public keys corresponding to the BIP32-related secrets
  *                           (to not to recompute them)
  */
class ErgoProvingInterpreter(val secretKeys: IndexedSeq[SecretKey],
                             params: ErgoLikeParameters,
                             val cachedHdPubKeysOpt: Option[IndexedSeq[ExtendedPublicKey]] = None)
                            (implicit IR: IRContext)
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
    new ErgoProvingInterpreter(secretKeys, newParams, this.cachedHdPubKeysOpt)
  }

  def signInputs(unsignedTx: UnsignedErgoLikeTransaction,
                 boxesToSpend: IndexedSeq[ErgoBox],
                 dataBoxes: IndexedSeq[ErgoBox],
                 stateContext: ErgoLikeStateContext,
                 txHints: TransactionHintsBag): Try[(IndexedSeq[Input], Long)] = {
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

          inputsCostTry.flatMap { case (ins, totalCost) =>
            val context = new ErgoLikeContext(
              ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
              stateContext.sigmaLastHeaders,
              stateContext.sigmaPreHeader,
              dataBoxes,
              boxesToSpend,
              unsignedTx,
              boxIdx.toShort,
              unsignedInput.extension,
              ValidationRules.currentSettings,
              params.maxBlockCost,
              totalCost
            )

            val hints = txHints.bags.getOrElse(boxIdx, HintsBag.empty)
            prove(inputBox.ergoTree, context, unsignedTx.messageToSign, hints).flatMap { proverResult =>
              //prove is accumulating cost under the hood, so proverResult.cost = totalCost + input check cost
              val newTC = proverResult.cost
              if (newTC > context.costLimit) {
                Failure(new Exception(s"Cost of transaction $unsignedTx exceeds limit ${context.costLimit}"))
              } else {
                Success((ins :+ Input(unsignedInput.boxId, proverResult)) -> newTC)
              }
            }
          }
        }
    }
  }

  /**
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext,
           txHints: TransactionHintsBag = TransactionHintsBag.empty): Try[ErgoLikeTransaction] = {

    val signedInputs: Try[(IndexedSeq[Input], Long)] =
      signInputs(unsignedTx, boxesToSpend, dataBoxes, stateContext, txHints)
    signedInputs.map { case (inputs, _) =>
      new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
    }
  }

  /**
    *
    *
    * @param tx
    * @param boxesToSpend
    * @param dataBoxes
    * @param stateContext
    * @param realSecretsToExtract
    * @param simulatedSecretsToExtract
    * @return
    */
  def bagForTransaction(tx: ErgoLikeTransaction,
                        boxesToSpend: IndexedSeq[ErgoBox],
                        dataBoxes: IndexedSeq[ErgoBox],
                        stateContext: ErgoLikeStateContext,
                        realSecretsToExtract: Seq[SigmaBoolean],
                        simulatedSecretsToExtract: Seq[SigmaBoolean]): TransactionHintsBag = {
    val augmentedInputs = tx.inputs.zipWithIndex.zip(boxesToSpend)
    require(augmentedInputs.forall { case ((input, _), box) => input.boxId.sameElements(box.id) }, "Wrong boxes")

    augmentedInputs.foldLeft(TransactionHintsBag.empty) { case (bag, ((input, idx), box)) =>
      val exp = box.ergoTree
      val proof = input.spendingProof.proof

      val lastBlockUtxoRoot: AvlTreeData = ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest)
      val headers: Coll[Header] = stateContext.sigmaLastHeaders
      val preHeader: PreHeader = stateContext.sigmaPreHeader
      val spendingTransaction = tx
      val selfIndex: Int = idx
      val extension: ContextExtension = input.spendingProof.extension
      val validationSettings: SigmaValidationSettings = ValidationRules.currentSettings
      val costLimit: Long = params.maxBlockCost
      val initCost: Long = 0

      val ctx: ErgoLikeContext = new ErgoLikeContext(lastBlockUtxoRoot, headers, preHeader, dataBoxes, boxesToSpend,
        spendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost)

      bag.putHints(idx, bagForMultisig(ctx, exp, proof, realSecretsToExtract, simulatedSecretsToExtract))
    }
  }

}

object ErgoProvingInterpreter {

  case class TransactionHintsBag(bags: Map[Int, HintsBag]) {
    def putHints(index: Int, hintsBag: HintsBag): TransactionHintsBag = {
      TransactionHintsBag(bags.updated(index, hintsBag))
    }
  }

  object TransactionHintsBag {
    val empty: TransactionHintsBag = TransactionHintsBag(Map.empty)
  }

  def apply(secrets: IndexedSeq[SecretKey],
            params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(secrets, params)(new RuntimeIRContext)

  def apply(rootSecret: ExtendedSecretKey,
            params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(IndexedSeq(rootSecret), params)(new RuntimeIRContext)


  /**
    * A method which is generating a commitment to randomness, which is a first step to prove
    * knowledge of a secret. Method checks whether secret is known to the prover, and returns
    * None if the secret is not known.
    *
    * @param pubkey - public image of a secret
    * @return (r, cmt), a commitment to (secret) randomness "cmt" along with the randomness "r"
    */
  def generateCFor(pubkey: SigmaBoolean): (BigInteger, FirstProverMessage) = {
    pubkey match {
      case _: ProveDlog =>
        DLogInteractiveProver.firstMessage()
      case dh: ProveDHTuple =>
        DiffieHellmanTupleInteractiveProver.firstMessage(dh)
      case _ =>
        // other options not supported yet but possible,
        // e.g. a sub-tree like ("pk_A || pkC")
        // corresponding to the complex statement ("(pk_A || pkC) && (pk_D || pkE)")
        ???
    }
  }

}
