package org.ergoplatform.nodeView.wallet

import cats.implicits._
import cats.Traverse
import org.ergoplatform.ErgoBox.{BoxId, R4, R5, R6}
import org.ergoplatform.{DataInput, ErgoAddress, ErgoAddressEncoder, ErgoBox, ErgoBoxCandidate, P2PKAddress, UnsignedInput}
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest, TransactionGenerationRequest}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.utils.BoxUtils
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.{BoxSelector, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedPublicKey, ExtendedSecretKey}
import org.ergoplatform.wallet.transactions.TransactionBuilder
import scorex.crypto.hash.Digest32
import scorex.util.{ScorexLogging, idToBytes}
import sigmastate.Values.ByteArrayConstant
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.Extensions._
import sigmastate.eval._

import scala.util.{Failure, Success, Try}

trait ErgoWalletSupport extends ScorexLogging {

  protected def buildProverFromMnemonic(mnemonic: String, keysQty: Option[Int], parameters: Parameters): ErgoProvingInterpreter = {
    val seed = Mnemonic.toSeed(mnemonic)
    val rootSk = ExtendedSecretKey.deriveMasterKey(seed)
    val childSks = keysQty.toIndexedSeq.flatMap(x => (0 until x).map(rootSk.child))
    ErgoProvingInterpreter(rootSk +: childSks, parameters)
  }

  protected def addSecretToStorage(state: ErgoWalletState, secret: ExtendedSecretKey): Try[ErgoWalletState] =
    state.walletVars.withExtendedKey(secret).flatMap { newWalletVars =>
      Try {
        state.storage.addKey(secret.publicKey)
        state.copy(walletVars = newWalletVars)
      }
    }

  // call nextPath and derive next key from it
  protected def deriveNextKeyForMasterKey(state: ErgoWalletState, masterKey: ExtendedSecretKey, usePreEip3Derivation: Boolean)
                                       (implicit addrEncoder: ErgoAddressEncoder): Try[(DeriveNextKeyResult, ErgoWalletState)] = {
    val secrets = state.walletVars.proverOpt.toIndexedSeq.flatMap(_.hdKeys)
    val derivationResult = DerivationPath.nextPath(secrets, usePreEip3Derivation).map { path =>
      val secret = masterKey.derive(path)
      (path, P2PKAddress(secret.publicKey.key), secret)
    }
    derivationResult.map(_._3)
      .flatMap(secret => addSecretToStorage(state, secret))
      .map( newState => DeriveNextKeyResult(derivationResult) -> newState )
  }

  protected def updatePublicKeys(state: ErgoWalletState, masterKey: ExtendedSecretKey, pks: IndexedSeq[ExtendedPublicKey]): ErgoWalletState = {
    // Secrets corresponding to public keys
    val sks =
      pks.map { pk =>
        val path = pk.path.toPrivateBranch
        masterKey.derive(path)
      }
    // If no master key in the secrets corresponding to public keys,
    // add master key so then it is not available to the user but presents in the prover
    val secrets = if (sks.headOption.contains(masterKey)) {
      sks
    } else {
      masterKey +: sks
    }
    val prover = ErgoProvingInterpreter(secrets, state.parameters)
    state.copy(walletVars = state.walletVars.withProver(prover))
  }

  private def convertLegacyClientPaths(storage: WalletStorage, masterKey: ExtendedSecretKey): Unit = {
    // first, we're trying to find in the database paths written by clients prior 3.3.0 and convert them into a new format (pubkeys with paths stored instead of paths)
    val oldPaths = storage.readPaths()
    if (oldPaths.nonEmpty) {
      val oldDerivedSecrets = masterKey +: oldPaths.map {
        path => masterKey.derive(path)
      }
      val oldPubKeys = oldDerivedSecrets.map(_.publicKey)
      oldPubKeys.foreach(storage.addKey)
      storage.removePaths()
    }
  }

  protected def processUnlock(state: ErgoWalletState, masterKey: ExtendedSecretKey, usePreEip3Derivation: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[ErgoWalletState] = {
    log.info("Starting wallet unlock")
    convertLegacyClientPaths(state.storage, masterKey)
    // Now we read previously stored, or just stored during the conversion procedure above, public keys
    // If no public keys in the database yet, add master's public key into it
    if (state.storage.readAllKeys().toIndexedSeq.isEmpty) {
      if (usePreEip3Derivation) {
        // If usePreEip3Derivation flag is set in the wallet settings, the first key is the master key
        val masterPubKey = masterKey.publicKey
        state.storage.addKey(masterPubKey)
        log.info("Wallet unlock finished using usePreEip3Derivation")
        Try(updatePublicKeys(state, masterKey, Vector(masterPubKey)))
      } else {
        // If no usePreEip3Derivation flag is set, add first derived key (for m/44'/429'/0'/0/0) to the db
        deriveNextKeyForMasterKey(state, masterKey, usePreEip3Derivation).flatMap { case (derivationResult, newState) =>
          derivationResult.result.map { case (_, _, firstSk) =>
            val firstPk = firstSk.publicKey
            newState.storage.addKey(firstPk)
            newState.storage.updateChangeAddress(P2PKAddress(firstPk.key))
            log.info("Wallet unlock finished")
            updatePublicKeys(newState, masterKey, Vector(firstPk))
          }
        }
      }
    } else {
      log.info("Wallet unlock finished using existing keys in storage")
      Success(state)
    }
  }


  /**
    * Convert requests (to make payments or to issue an asset) to transaction outputs
    * There can be only one asset issuance request in the input sequence.
    *
    * @param requests - an input sequence of requests
    * @return sequence of transaction outputs or failure if inputs are incorrect
    */
  protected def requestsToBoxCandidates(
                                         requests: Seq[TransactionGenerationRequest],
                                         assetId: BoxId,
                                         fullHeight: Int,
                                         parameters: Parameters,
                                         publicKeyAddresses: Seq[P2PKAddress]): Try[Seq[ErgoBoxCandidate]] =
    Traverse[List].sequence {
      requests.toList
        .map {
          case PaymentRequest(address, value, assets, registers) =>
            Success(new ErgoBoxCandidate(value, address.script, fullHeight, assets.toColl, registers))
          case AssetIssueRequest(addressOpt, valueOpt, amount, name, description, decimals, registers) =>
            // Check that auxiliary registers do not try to rewrite registers R0...R6
            if (registers.exists(_.exists(_._1.number < 7))) {
              Failure(new Exception("Additional registers contain R0...R6"))
            } else {
              (addressOpt orElse publicKeyAddresses.headOption)
                .fold[Try[ErgoAddress]](Failure(new Exception("No address available for box locking")))(Success(_))
                .map { lockWithAddress =>
                  val nonMandatoryRegisters = scala.Predef.Map(
                    R4 -> ByteArrayConstant(name.getBytes("UTF-8")),
                    R5 -> ByteArrayConstant(description.getBytes("UTF-8")),
                    R6 -> ByteArrayConstant(String.valueOf(decimals).getBytes("UTF-8"))
                  ) ++ registers.getOrElse(Map())

                  def minimalErgoAmount: Long =
                    BoxUtils.minimalErgoAmountSimulated(
                      lockWithAddress.script,
                      Colls.fromItems((Digest32 @@ assetId) -> amount),
                      nonMandatoryRegisters,
                      parameters
                    )

                  new ErgoBoxCandidate(
                    valueOpt.getOrElse(minimalErgoAmount),
                    lockWithAddress.script,
                    fullHeight,
                    Colls.fromItems((Digest32 @@ assetId) -> amount),
                    nonMandatoryRegisters
                  )
                }
            }
          case other =>
            Failure(new Exception(s"Unknown TransactionRequest type: $other"))
        }
    }

  protected def prepareUnsignedTransaction(payTo: Seq[ErgoBoxCandidate],
                                           walletHeight: Int,
                                           selectionResult: BoxSelectionResult[TrackedBox],
                                           dataInputBoxes: IndexedSeq[ErgoBox],
                                           changeAddressOpt: Option[ProveDlog]): Try[UnsignedErgoTransaction] = Try {
    if (selectionResult.changeBoxes.nonEmpty) {
      require(changeAddressOpt.isDefined, "Does not have change address to send change to")
    }

    val dataInputs = dataInputBoxes.map(dataInputBox => DataInput(dataInputBox.id))
    val changeBoxCandidates = selectionResult.changeBoxes.map { changeBox =>
      val assets = changeBox.tokens.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
      new ErgoBoxCandidate(changeBox.value, changeAddressOpt.get, walletHeight, assets.toColl)
    }
    val inputBoxes = selectionResult.boxes.toIndexedSeq
    new UnsignedErgoTransaction(
      inputBoxes.map(_.box.id).map(id => new UnsignedInput(id)),
      dataInputs,
      (payTo ++ changeBoxCandidates).toIndexedSeq
    )
  }

  /**
    * Generates new unsigned transaction according to given requests using stored or provided boxes.
    *
    * @param requests      - requests to transfer funds or to issue an asset
    * @param inputsRaw     - user-provided inputs. If empty then wallet is looking for inputs itself. If non-empty, then
    *                      the wallet is not adding anything, thus the user in this case should take care about satisfying
    *                      the (sum(inputs) == sum(outputs)) preservation rule for ergs.
    * @param dataInputsRaw - user-provided data (read-only) inputs. Wallet is not able to figure out needed data inputs
    *                      (to spend the spendable inputs).
    * @return generated transaction along with its inputs and data-inputs, or an error
    */
  protected def generateUnsignedTransaction(
                                           state: ErgoWalletState,
                                           boxSelector: BoxSelector,
                                           requests: Seq[TransactionGenerationRequest],
                                           inputsRaw: Seq[String],
                                           dataInputsRaw: Seq[String]
                                         )(implicit addrEncoder: ErgoAddressEncoder): Try[(UnsignedErgoTransaction, IndexedSeq[ErgoBox], IndexedSeq[ErgoBox])] = Try {
    require(requests.count(_.isInstanceOf[AssetIssueRequest]) <= 1, "Too many asset issue requests")

    val userInputs = ErgoWalletService.stringsToBoxes(inputsRaw)

    val (inputBoxes, filter) = if (userInputs.nonEmpty) {
      // make TrackedBox sequence out of boxes provided
      val boxesToFakeTracked =
        userInputs.map { box => // declare fake inclusion height in order to confirm the box is onchain
          TrackedBox(box.transactionId, box.index, Some(1), None, None, box, Set(PaymentsScanId))
        }
      //inputs are provided externally, no need for filtering
      (boxesToFakeTracked, ErgoWalletState.noWalletFilter)
    } else {
      state.walletVars.proverOpt match {
        case Some(_) =>
          //inputs are to be selected by the wallet
          (state.getBoxesToSpend, state.walletFilter)
        case None =>
          throw new Exception(s"Cannot generateUnsignedTransaction($requests, $inputsRaw): wallet is locked")
      }
    }

    //We're getting id of the first input, it will be used in case of asset issuance (asset id == first input id)
    requestsToBoxCandidates(requests, inputBoxes.head.box.id, state.fullHeight, state.parameters, state.walletVars.publicKeyAddresses)
      .flatMap { outputs =>
        require(outputs.forall(c => c.value >= BoxUtils.minimalErgoAmountSimulated(c, state.parameters)), "Minimal ERG value not met")
        require(outputs.forall(_.additionalTokens.forall(_._2 > 0)), "Non-positive asset value")

        val assetIssueBox = outputs
          .zip(requests)
          .filter(_._2.isInstanceOf[AssetIssueRequest])
          .map(_._1)
          .headOption

        val targetBalance = outputs.map(_.value).sum
        val targetAssets = TransactionBuilder.collectOutputTokens(outputs.filterNot(bx => assetIssueBox.contains(bx)))

        val selectionOpt = boxSelector.select(inputBoxes.iterator, filter, targetBalance, targetAssets)
        val dataInputs = ErgoWalletService.stringsToBoxes(dataInputsRaw).toIndexedSeq
        selectionOpt.map { selectionResult =>
          val changeAddressOpt: Option[ProveDlog] = state.getChangeAddress.map(_.pubkey)
          prepareUnsignedTransaction(outputs, state.getWalletHeight, selectionResult, dataInputs, changeAddressOpt) -> selectionResult.boxes
        } match {
          case Right((txTry, inputs)) => txTry.map(tx => (tx, inputs.map(_.box).toIndexedSeq, dataInputs))
          case Left(e) => Failure(
            new Exception(s"Failed to find boxes to assemble a transaction for $outputs, \nreason: $e")
          )
        }
    }
  }.flatten

}
