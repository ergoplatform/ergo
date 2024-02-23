package org.ergoplatform.wallet.interpreter

import org.ergoplatform._
import org.ergoplatform.sdk.BlockchainParameters
import org.ergoplatform.sdk.utils.ArithUtils.{addExact, multiplyExact}
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.ergoplatform.sdk.wallet.secrets.{ExtendedPublicKey, ExtendedSecretKey, SecretKey}
import org.ergoplatform.validation.{SigmaValidationSettings, ValidationRules}
import org.ergoplatform.wallet.boxes.ErgoBoxAssetExtractor
import scorex.util.encode.Base16
import sigmastate.AvlTreeData
import sigmastate.Values.SigmaBoolean
import sigmastate.crypto.SigmaProtocolPrivateInput
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigma.{Coll, Header, PreHeader}

import java.util
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
                             params: BlockchainParameters,
                             val cachedHdPubKeysOpt: Option[IndexedSeq[ExtendedPublicKey]] = None)
  extends ErgoInterpreter(params) with ProverInterpreter {

  /**
    * Interpreter's secrets, in form of sigma protocols private inputs
    */
  val secrets: IndexedSeq[SigmaProtocolPrivateInput[_]] = secretKeys.map(_.privateInput)

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
    * Activated script version, 0 is for Ergo mainnet since block #1 until 417,792, 1 for Ergo mainnet since 417,792,
    * etc.
    * Note: version N of ErgoProtocol corresponds to version N-1 of ErgoTree (aka script version)
    */
  val activatedScriptVersion: Byte = (params.blockVersion - 1).toByte

  /**
    * Produces updated instance of ErgoProvingInterpreter with a new secret included
    * @param secret - new secret to add
    * @return modified prover
    */
  def withNewExtendedSecret(secret: ExtendedSecretKey): ErgoProvingInterpreter = {
    val newPk = secret.publicKey
    val sks   = secretKeys :+ secret
    val pks   = hdPubKeys :+ newPk
    log.info(s"New secret created, public image: ${Base16.encode(newPk.key.pkBytes)}")
    new ErgoProvingInterpreter(sks, params, Some(pks))
  }

  /**
    * Produces updated instance of ErgoProvingInterpreter with updated parameters
    * @param newParams - updated parameters
    * @return modified prover
    */
  def withNewParameters(newParams: BlockchainParameters): ErgoProvingInterpreter = {
    new ErgoProvingInterpreter(this.secretKeys, newParams, this.cachedHdPubKeysOpt)
  }

  def signInputs(unsignedTx: UnsignedErgoLikeTransaction,
                 boxesToSpend: IndexedSeq[ErgoBox],
                 dataBoxes: IndexedSeq[ErgoBox],
                 stateContext: BlockchainStateContext,
                 txHints: TransactionHintsBag): Try[(IndexedSeq[Input], Long)] = {
    if (unsignedTx.inputs.length != boxesToSpend.length) {
      Failure(new Exception("Not enough boxes to spend"))
    } else if (unsignedTx.dataInputs.length != dataBoxes.length) {
      Failure(new Exception("Not enough data boxes"))
    } else {
      ErgoBoxAssetExtractor.extractTotalAssetsAccessCost(boxesToSpend, unsignedTx.outputCandidates, params.tokenAccessCost)
        .flatMap { totalAssetsAccessCost =>

          // Cost of transaction initialization: we should read and parse all inputs and data inputs,
          // and also iterate through all outputs to check rules, also we add some constant for interpreter initialization
          val initialCost: Long = addExact(
            ErgoInterpreter.interpreterInitCost,
            multiplyExact(boxesToSpend.size, params.inputCost),
            multiplyExact(dataBoxes.size, params.dataInputCost),
            multiplyExact(unsignedTx.outputCandidates.size, params.outputCost),
            totalAssetsAccessCost
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
                  totalCost,
                  activatedScriptVersion
                )

                val hints = txHints.allHintsForInput(boxIdx)
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
  }

  /**
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: BlockchainStateContext,
           txHints: TransactionHintsBag = TransactionHintsBag.empty): Try[ErgoLikeTransaction] = {

    val signedInputs: Try[(IndexedSeq[Input], Long)] =
      signInputs(unsignedTx, boxesToSpend, dataBoxes, stateContext, txHints)
    signedInputs.map { case (inputs, _) =>
      new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
    }
  }

  /**
    * A method which is generating commitments to randomness. A commitment is about a first step
    * of a zero-knowledge proof-of-knowledge knowledge protocol.
    *
    * Method checks whether secret is known to the prover, and returns
    * None if the secret is not known.
    *
    * @param unsignedTx - transaction to be signed with commitments to be generated first
    * @param boxesToSpend - boxes the transaction is spending
    * @param dataBoxes - read-only inputs of the transaction
    * @param stateContext - context used for signing
    * @return - hints for signing transaction
    */
  def generateCommitmentsFor(unsignedTx: UnsignedErgoLikeTransaction,
                             boxesToSpend: IndexedSeq[ErgoBox],
                             dataBoxes: IndexedSeq[ErgoBox],
                             stateContext: BlockchainStateContext): Try[TransactionHintsBag] = Try {
    val inputCmts = unsignedTx.inputs.zipWithIndex.map { case (unsignedInput, inpIndex) =>

      val inputBox = boxesToSpend(inpIndex)

      val context = new ErgoLikeContext(
        ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
        stateContext.sigmaLastHeaders,
        stateContext.sigmaPreHeader,
        dataBoxes,
        boxesToSpend,
        unsignedTx,
        inpIndex.toShort,
        unsignedInput.extension,
        ValidationRules.currentSettings,
        params.maxBlockCost,
        0L, // initial cost
        activatedScriptVersion
      )
      val scriptToReduce = inputBox.ergoTree
      inpIndex -> generateCommitments(scriptToReduce, context)
    }

    TransactionHintsBag(inputCmts.toMap)
  }

  /**
    * Extract hints from (supposedly, partially) signed transaction. Useful for distributed signing.
    *
    * @param tx - signed transaction
    * @param boxesToSpend - input boxes the transaction are spending
    * @param dataBoxes - read-only inputs of the transaction
    * @param stateContext - context used for signing
    * @param realSecretsToExtract - public images of secrets used in signing
    * @param simulatedSecretsToExtract - public images of simulated secrets
    * @return hints for (further) transaction signing
    */
  def bagForTransaction(tx: ErgoLikeTransaction,
                        boxesToSpend: IndexedSeq[ErgoBox],
                        dataBoxes: IndexedSeq[ErgoBox],
                        stateContext: BlockchainStateContext,
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
        spendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost, activatedScriptVersion)

      bag.replaceHintsForInput(idx, bagForMultisig(ctx, exp, proof, realSecretsToExtract, simulatedSecretsToExtract))
    }
  }

}

object ErgoProvingInterpreter {

  def apply(secrets: IndexedSeq[SecretKey],
            params: BlockchainParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(secrets, params)

  def apply(rootSecret: ExtendedSecretKey,
            params: BlockchainParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(IndexedSeq(rootSecret), params)

}
