package org.ergoplatform.nodeView.wallet

import cats.implicits._
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.{ErgoStateContext, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.models.{ChangeBox, CollectedBoxes}
import org.ergoplatform.nodeView.wallet.persistence.{WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.{BoxSelector, ErgoBoxSerializer}
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.interpreter.{ErgoProvingInterpreter, TransactionHintsBag}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey, JsonSecretStorage}
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.DLogProverInput

import java.io.FileNotFoundException
import scala.util.{Failure, Success, Try}

/**
  * Operations accessible from [[ErgoWalletActor]]
  */
trait ErgoWalletService {

  val ergoSettings: ErgoSettings

  /**
    * Read encrypted wallet json file or bypass it by providing mnemonic directly (test mode only)
    * @param state current wallet state
    * @param testMnemonic for bypassing secret storage reading
    * @param testKeysQty how many children keys to derive from master key
    * @param secretStorageSettings to be used for reading wallet when testMnemonic is not provided
    * @return new wallet state
    */
  def readWallet(state: ErgoWalletState,
                 testMnemonic: Option[SecretString],
                 testKeysQty: Option[Int],
                 secretStorageSettings: SecretStorageSettings): ErgoWalletState

  /**
    * Initializes JsonSecretStorage with new wallet file encrypted with given `walletPass`
    * @param state current wallet state
    * @param settings settings read from config file
    * @param walletPass to encrypt wallet file with
    * @param mnemonicPassOpt optional mnemonic password included into seed. Needed for restoring wallet
    * @return generated mnemonic the JsonSecretStorage was initialized with -> new wallet state
    */
  def initWallet(state: ErgoWalletState,
                 settings: ErgoSettings,
                 walletPass: SecretString,
                 mnemonicPassOpt: Option[SecretString]): Try[(SecretString, ErgoWalletState)]

  /**
    * @param state current wallet state
    * @param settings settings read from config file
    * @param mnemonic that was used to initialize wallet with
    * @param mnemonicPassOpt that was used to initialize wallet with
    * @param walletPass that was used to initialize wallet with
    * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
    * @return new wallet state
    */
  def restoreWallet(state: ErgoWalletState,
                    settings: ErgoSettings,
                    mnemonic: SecretString,
                    mnemonicPassOpt: Option[SecretString],
                    walletPass: SecretString, 
                    usePre1627KeyDerivation: Boolean): Try[ErgoWalletState]

  /**
    * Decrypt underlying encrypted storage using `walletPass` and update public keys
    * @param state current wallet state
    * @param walletPass used for wallet initialization to decrypt wallet json file with
    * @param usePreEip3Derivation if true, the first key is the master key
    * @return Try of new wallet state
    */
  def unlockWallet(state: ErgoWalletState, walletPass: SecretString, usePreEip3Derivation: Boolean): Try[ErgoWalletState]

  /**
    * Clear secret from previously decrypted json storage and reset prover
    * @param state current wallet state
    * @return Try of new wallet state
    */
  def lockWallet(state: ErgoWalletState): ErgoWalletState

  /**
    * Close it, recursively delete registryFolder from filesystem if present and create new registry
    * @param state current wallet state
    * @param settings settings read from config file
    * @return Try of new wallet state
    */
  def recreateRegistry(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState]

  /**
    * Close it, recursively delete storageFolder from filesystem if present and create new storage
    * @param state current wallet state
    * @param settings settings read from config file
    * @return Try of new wallet state
    */
  def recreateStorage(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState]

  def getWalletBoxes(state: ErgoWalletState, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox]

  /**
    * @param state               current wallet state
    * @param scanId              to get boxes for
    * @param considerUnconfirmed whether to look for boxes in off-chain registry
    * @param minHeight           min inclusion height of unspent boxes
    * @param maxHeight           max inclusion height of unspent boxes
    * @return Unspent wallet boxes corresponding to `scanId`
    */
  def getScanUnspentBoxes(state: ErgoWalletState, scanId: ScanId, considerUnconfirmed: Boolean, minHeight: Int, maxHeight: Int): Seq[WalletBox]

  /**
    * @param state current wallet state
    * @param scanId to get boxes for
    * @return Spent wallet boxes corresponding to `scanId`
    */
  def getScanSpentBoxes(state: ErgoWalletState, scanId: ScanId): Seq[WalletBox]

  /**
    * @param registry - wallet registry database
    * @param fullHeight - current height (to construct AugWalletTransaction instances with confirmations included)
    * @return - wallet transactions augmented with confirmations numbers
    */
  def getTransactions(registry: WalletRegistry,
                      fullHeight: Int
                      ): Seq[AugWalletTransaction]

  /**
    * @param txId - transaction identifier
    * @param registry - wallet registry database
    * @param fullHeight - current height (to construct AugWalletTransaction instances with confirmations included)
    * @return - wallet transaction augmented with confirmations number, or None is no such transaction in the wallet
    */
  def getTransactionsByTxId(txId: ModifierId, registry: WalletRegistry, fullHeight: Int): Option[AugWalletTransaction]

  /**
    * Collects boxes according to given request
    * @param targetBalance - Balance requested by user
    * @param targetAssets  - IDs and amounts of other tokens
    * @return collected ErgoBoxes and ChangeBoxes
    */
  def collectBoxes(state: ErgoWalletState, boxSelector: BoxSelector, targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long]): Try[CollectedBoxes]

  /**
    * Derive key from given encoded path according to BIP-32
    * @param state current wallet state
    * @param encodedPath derivation path from the master key
    * @return Try of pay-to-public-key-address and new wallet state
    */
  def deriveKeyFromPath(state: ErgoWalletState, encodedPath: String, addrEncoder: ErgoAddressEncoder): Try[(P2PKAddress, ErgoWalletState)]

  /**
   * Get the secret key for a give derivation path.
   * @param state current wallet state
   * @param path derivation path from the master key
   * @return Try of private key
   */
  def getPrivateKeyFromPath(state: ErgoWalletState, path: DerivationPath): Try[DLogProverInput]

  /**
    * Derive next key from master key
    * @param state current wallet state
    * @param usePreEip3Derivation whether to use pre-EIP3 derivation or not
    * @return Try of the derived key and new wallet state
    */
  def deriveNextKey(state: ErgoWalletState, usePreEip3Derivation: Boolean): Try[(DeriveNextKeyResult, ErgoWalletState)]

  /**
    * Get unconfirmed transactions from mempool that are associated with given scan id
    * @param state current wallet state
    * @param scanId to get transactions for
    * @return Unconfirmed transactions for `scanId`
    */
  def getUnconfirmedTransactions(state: ErgoWalletState, scanId: ScanId): Seq[AugWalletTransaction]

  /**
    * Get transactions aassociated with given scan id
    * @param state current wallet state
    * @param scanId to get transactions for
    * @param fullHeight of the chain (last blocked applied to the state, not the wallet)
    * @param includeUnconfirmed whether to include transactions from mempool that match given scanId
    * @return Wallet transactions for `scanId`
    */
  def getScanTransactions(state: ErgoWalletState,
                          scanId: ScanId,
                          fullHeight: Int,
                          includeUnconfirmed: Boolean): Seq[AugWalletTransaction]

  def addScan(state: ErgoWalletState, appRequest: ScanRequest): Try[(Scan, ErgoWalletState)]

  def removeScan(state: ErgoWalletState, scanId: ScanId): Try[ErgoWalletState]

  /**
    * Update UtxoState with mempool
    * @param state current wallet state
    * @return new wallet state
    */
  def updateUtxoState(state: ErgoWalletState): ErgoWalletState

  /**
    * Process the block transactions and update database and in-memory structures for offchain data accordingly
    *
    * @param state current wallet state
    * @param block - block to scan
    * @param dustLimit - Boxes with value smaller than dustLimit are disregarded in wallet scan logic
    */
  def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock, dustLimit: Option[Long]): Try[ErgoWalletState]

  /**
    * Sign a transaction
    */
  def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
                      tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpendOpt: Option[Seq[ErgoBox]],
                      dataBoxesOpt: Option[Seq[ErgoBox]],
                      parameters: Parameters,
                      stateContext: ErgoStateContext)(extract: BoxId => Option[ErgoBox]): Try[ErgoTransaction]

  /**
    * Generate signed or unsigned transaction.
    */
  def generateTransaction(state: ErgoWalletState,
                          boxSelector: BoxSelector,
                          requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String],
                          dataInputsRaw: Seq[String],
                          sign: Boolean): Try[ErgoLikeTransactionTemplate[_]]

  /**
    * Generate commitments to be used then to sign a transaction.
    * See EIP-11 for details.
    */
  def generateCommitments(state: ErgoWalletState,
                          unsignedTx: UnsignedErgoTransaction,
                          externalSecretsOpt: Option[Seq[ExternalSecret]],
                          externalInputsOpt: Option[Seq[ErgoBox]],
                          externalDataInputsOpt: Option[Seq[ErgoBox]]): Try[TransactionHintsBag]

  /**
    * Extract hints from (supposedly, partially) signed transaction. Useful for distributed signing.
    * See EIP-11 for details.
    */
  def extractHints(state: ErgoWalletState,
                   tx: ErgoTransaction,
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean],
                   boxesToSpendOpt: Option[Seq[ErgoBox]],
                   dataBoxesOpt: Option[Seq[ErgoBox]]): TransactionHintsBag

}

class ErgoWalletServiceImpl(override val ergoSettings: ErgoSettings) extends ErgoWalletService with ErgoWalletSupport with FileUtils {


  override def readWallet(state: ErgoWalletState,
                 testMnemonic: Option[SecretString],
                 testKeysQty: Option[Int],
                 secretStorageSettings: SecretStorageSettings): ErgoWalletState = {
    testMnemonic match {
      case Some(mnemonic) =>
        log.warn("Avoiding wallet reading in test mode by building prover from mnemonic. Switch to secure mode for production usage.")
        val prover = buildProverFromMnemonic(mnemonic, testKeysQty, state.parameters)
        state.copy(walletVars = state.walletVars.withProver(prover))
      case None =>
        log.info("Trying to read wallet in secure mode ..")
        JsonSecretStorage.readFile(secretStorageSettings).fold(
          e => {
            e match {
              case e: FileNotFoundException =>
                log.info(s"Wallet secret storage not found. Details: {}", e.getMessage)
              case _ =>
                log.warn(s"Failed to read wallet. Manual initialization is required. Details: ", e)
            }
            state
          },
          secretStorage => {
            log.info("Wallet loaded successfully and locked")
            state.copy(secretStorageOpt = Some(secretStorage))
          }
        )
    }
  }

  override def initWallet(state: ErgoWalletState,
                 settings: ErgoSettings,
                 walletPass: SecretString,
                 mnemonicPassOpt: Option[SecretString]): Try[(SecretString, ErgoWalletState)] = {
    val walletSettings = settings.walletSettings
    //Read high-quality random bits from Java's SecureRandom
    val entropy = scorex.utils.Random.randomBytes(walletSettings.seedStrengthBits / 8)
    log.info("Initializing wallet")

    def initStorage(mnemonic: SecretString): Try[JsonSecretStorage] =
      Try(JsonSecretStorage.init(Mnemonic.toSeed(mnemonic, mnemonicPassOpt), walletPass, usePre1627KeyDerivation = false)(walletSettings.secretStorage))

    val result =
      new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .flatMap { mnemonic =>
          initStorage(mnemonic).flatMap { newSecretStorage =>
            // remove old wallet state, see https://github.com/ergoplatform/ergo/issues/1313
            recreateRegistry(state, settings).flatMap { stateV1 =>
              recreateStorage(stateV1, settings).map { stateV2 =>
                mnemonic -> stateV2.copy(secretStorageOpt = Some(newSecretStorage))
              }
            }
          }
        }

    java.util.Arrays.fill(entropy, 0: Byte)
    result
  }

  override def restoreWallet(state: ErgoWalletState,
                    settings: ErgoSettings,
                    mnemonic: SecretString,
                    mnemonicPassOpt: Option[SecretString],
                    walletPass: SecretString,
                    usePre1627KeyDerivation: Boolean): Try[ErgoWalletState] =
    if (settings.nodeSettings.isFullBlocksPruned)
      Failure(new IllegalArgumentException("Unable to restore wallet when pruning is enabled"))
    else
      Try(JsonSecretStorage.restore(mnemonic, mnemonicPassOpt, walletPass, settings.walletSettings.secretStorage, usePre1627KeyDerivation))
        .flatMap { secretStorage =>
          // remove old wallet state, see https://github.com/ergoplatform/ergo/issues/1313
          recreateRegistry(state, settings).flatMap { stateV1 =>
            recreateStorage(stateV1, settings).map { stateV2 =>
              stateV2.copy(secretStorageOpt = Some(secretStorage))
            }
          }
        }


  override def unlockWallet(state: ErgoWalletState,
                   walletPass: SecretString,
                   usePreEip3Derivation: Boolean): Try[ErgoWalletState] = {
    if (state.walletVars.proverOpt.isEmpty) {
      state.secretStorageOpt match {
        case Some(secretStorage) =>
          secretStorage.unlock(walletPass).flatMap { _ =>
            secretStorage.secret match {
              case None =>
                Failure(new Exception("Master key is not available for wallet unlocking"))
              case Some(masterKey) =>
                processUnlock(state, masterKey, usePreEip3Derivation)
            }
          }
        case None =>
          Failure(new Exception("Wallet not initialized"))
      }
    } else {
      log.info("Wallet already unlocked")
      Failure(new Exception("Wallet already unlocked"))
    }
  }

  override def lockWallet(state: ErgoWalletState): ErgoWalletState = {
    state.secretStorageOpt.foreach(_.lock())
    state.copy(walletVars = state.walletVars.resetProver())
  }

  override def recreateRegistry(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState] = {
    val registryFolder = WalletRegistry.registryFolder(settings)
    log.info(s"Removing the registry folder $registryFolder")
    state.registry.close()

    deleteRecursive(registryFolder)

    WalletRegistry.apply(settings).map { reg =>
      state.copy(registry = reg)
    }
  }

  override def recreateStorage(state: ErgoWalletState, settings: ErgoSettings): Try[ErgoWalletState] =
    Try {
      val storageFolder = WalletStorage.storageFolder(settings)
      log.info(s"Removing the wallet storage folder $storageFolder")
      state.storage.close()
      deleteRecursive(storageFolder)
      state.copy(storage = WalletStorage.readOrCreate(settings))
    }

  override def getWalletBoxes(state: ErgoWalletState, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox] = {
    val currentHeight = state.fullHeight
    val boxes = if (unspentOnly) {
      val confirmed = state.registry.walletUnspentBoxes(state.maxInputsToUse * BoxSelector.ScanDepthFactor)
      if (considerUnconfirmed) {
        // We filter out spent boxes in the same way as wallet does when assembling a transaction
        (confirmed ++ state.offChainRegistry.offChainBoxes).filter(state.walletFilter)
      } else {
        confirmed
      }
    } else {
      val confirmed = state.registry.walletConfirmedBoxes()
      if (considerUnconfirmed) {
        // Just adding boxes created off-chain
        confirmed ++ state.offChainRegistry.offChainBoxes
      } else {
        confirmed
      }
    }
    boxes.map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
  }

  override def getScanUnspentBoxes(state: ErgoWalletState, scanId: ScanId, considerUnconfirmed: Boolean, minHeight: Int, maxHeight: Int): Seq[WalletBox] = {
    val unconfirmed = if (considerUnconfirmed) {
      state.offChainRegistry.offChainBoxes.filter(_.scans.contains(scanId))
    } else {
      Array.empty
    }

    val currentHeight = state.fullHeight
    val unspentBoxes = state.registry.unspentBoxesByInclusionHeight(scanId, minHeight, maxHeight)
    (unspentBoxes ++ unconfirmed).map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
  }

  override def getScanSpentBoxes(state: ErgoWalletState, scanId: ScanId): Seq[WalletBox] = {
    val currentHeight = state.fullHeight
    val boxes = state.registry.spentBoxes(scanId)
    boxes.map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
  }

  override def getTransactions(registry: WalletRegistry,
                      fullHeight: Int
                     ): Seq[AugWalletTransaction] = {
    registry.allWalletTxs().sortBy(-_.inclusionHeight)
      .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))
  }

  override def getTransactionsByTxId(txId: ModifierId, registry: WalletRegistry, fullHeight: Int): Option[AugWalletTransaction] =
    registry.getTx(txId)
      .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

  override def collectBoxes(state: ErgoWalletState, boxSelector: BoxSelector, targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long]): Try[CollectedBoxes] = {
    val assetsMap = targetAssets.map(t => bytesToId(t._1) -> t._2)
    val inputBoxes = state.getBoxesToSpend
    boxSelector
      .select(inputBoxes.iterator, state.walletFilter, targetBalance, assetsMap)
      .leftMap(m => new Exception(m.message))
      .map { res =>
        val ergoBoxes = res.inputBoxes.map(_.box)
        val changeBoxes = res.changeBoxes.map(b => ChangeBox(b.value, b.tokens))
        CollectedBoxes(ergoBoxes, changeBoxes)
      }.toTry
  }

  override def generateTransaction(state: ErgoWalletState,
                          boxSelector: BoxSelector,
                          requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String],
                          dataInputsRaw: Seq[String],
                          sign: Boolean): Try[ErgoLikeTransactionTemplate[_]] = {
    val tx = generateUnsignedTransaction(state, boxSelector, requests, inputsRaw, dataInputsRaw)
    if (sign) {
      tx.flatMap { case (unsignedTx, inputs, dataInputs) =>
        state.walletVars.proverOpt match {
          case Some(prover) =>
              prover.sign(unsignedTx, inputs, dataInputs, state.stateContext, TransactionHintsBag.empty)
                .map(ErgoTransaction.apply)
                .fold(
                  e => Failure(new Exception(s"Failed to sign boxes due to ${e.getMessage}: $inputs", e)),
                  tx => Success(tx))
          case None =>
            Failure(new Exception(s"Cannot sign the transaction $unsignedTx, wallet locked or not initialized"))
        }
      }
    } else {
      tx.map(_._1)
    }
  }

  override def generateCommitments(state: ErgoWalletState,
                          unsignedTx: UnsignedErgoTransaction,
                          externalSecretsOpt: Option[Seq[ExternalSecret]],
                          externalInputsOpt: Option[Seq[ErgoBox]],
                          externalDataInputsOpt: Option[Seq[ErgoBox]]): Try[TransactionHintsBag] = {
    val walletSecrets = state.walletVars.proverOpt.map(_.secretKeys).getOrElse(Seq.empty)
    val secrets = walletSecrets ++ externalSecretsOpt.getOrElse(Seq.empty).map(_.key)
    val prover: ErgoProvingInterpreter = ErgoProvingInterpreter(secrets.toIndexedSeq, state.parameters)

    val inputBoxes = externalInputsOpt.map(_.toIndexedSeq).getOrElse {
      unsignedTx.inputs.flatMap { unsignedInput =>
        state.readBoxFromUtxoWithWalletFallback(unsignedInput.boxId)
      }
    }

    val dataBoxes = externalDataInputsOpt.map(_.toIndexedSeq).getOrElse {
      unsignedTx.dataInputs.flatMap { dataInput =>
        state.readBoxFromUtxoWithWalletFallback(dataInput.boxId)
      }
    }

    prover.generateCommitmentsFor(unsignedTx, inputBoxes, dataBoxes, state.stateContext)
  }

  override def extractHints(state: ErgoWalletState,
                   tx: ErgoTransaction,
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean],
                   boxesToSpendOpt: Option[Seq[ErgoBox]],
                   dataBoxesOpt: Option[Seq[ErgoBox]]): TransactionHintsBag = {
    val inputBoxes = boxesToSpendOpt.map(_.toIndexedSeq).getOrElse {
      tx.inputs.flatMap { input =>
        state.readBoxFromUtxoWithWalletFallback(input.boxId)
      }
    }

    val dataBoxes = dataBoxesOpt.map(_.toIndexedSeq).getOrElse {
      tx.dataInputs.flatMap { dataInput =>
        state.readBoxFromUtxoWithWalletFallback(dataInput.boxId)
      }
    }

    val prover = state.walletVars.proverOpt.getOrElse(ErgoProvingInterpreter(IndexedSeq.empty, state.parameters))
    prover.bagForTransaction(tx, inputBoxes, dataBoxes, state.stateContext, real, simulated)
  }

  override def deriveKeyFromPath(state: ErgoWalletState, encodedPath: String, addrEncoder: ErgoAddressEncoder): Try[(P2PKAddress, ErgoWalletState)] =
    state.secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        val rootSecret = secretStorage.secret.get // unlocked means Some(secret)
        DerivationPath.fromEncoded(encodedPath) match {
          case Success(path) if !path.publicBranch =>
            val secret = rootSecret.derive(path)
            addSecretToStorage(state, secret) match {
              case Success(newState) =>
                Success(P2PKAddress(secret.publicKey.key)(addrEncoder) -> newState)
              case Failure(t) =>
                Failure(t)
            }
          case Success(path) =>
            Failure(new Exception(s"A private path is expected, but the public one given: $path"))
          case Failure(t) =>
            Failure(t)
        }
      case Some(_) =>
        Failure(new Exception("Unable to derive key from path, wallet is locked"))
      case None =>
        Failure(new Exception("Unable to derive key from path, wallet is not initialized"))
    }

  override def getPrivateKeyFromPath(state: ErgoWalletState, path: DerivationPath): Try[DLogProverInput] =
    state.secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        val rootSecret = secretStorage.secret.get // unlocked means Some(secret)
        Success(rootSecret.derive(path.toPrivateBranch).privateInput)
      case Some(_) =>
        Failure(new Exception("Unable to derive key from path, wallet is locked"))
      case None =>
        Failure(new Exception("Unable to derive key from path, wallet is not initialized"))
    }


  override def deriveNextKey(state: ErgoWalletState, usePreEip3Derivation: Boolean): Try[(DeriveNextKeyResult, ErgoWalletState)] =
    state.secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        val masterKey = secretStorage.secret.get // unlocked means Some(secret)
        deriveNextKeyForMasterKey(state, masterKey, usePreEip3Derivation)
      case Some(_) =>
        Failure(new Exception("Unable to derive key, wallet is locked"))
      case None =>
        Failure(new Exception("Unable to derive key, wallet is not initialized"))
    }

  override def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock, dustLimit: Option[Long]): Try[ErgoWalletState] =
      WalletScanLogic.scanBlockTransactions(
        state.registry,
        state.offChainRegistry,
        state.walletVars,
        block,
        state.outputsFilter,
        dustLimit,
        ergoSettings.walletSettings.walletProfile).map { case (reg, offReg, updatedOutputsFilter) =>
        state.copy(registry = reg, offChainRegistry = offReg, outputsFilter = Some(updatedOutputsFilter))
      }

  override def updateUtxoState(state: ErgoWalletState): ErgoWalletState = {
    (state.mempoolReaderOpt, state.stateReaderOpt) match {
      case (Some(mr), Some(sr)) =>
        sr match {
          case u: UtxoStateReader =>
            state.copy(utxoStateReaderOpt = Some(u.withMempool(mr)))
          case _ =>
            state
        }
      case (_, _) =>
        state
    }
  }

  override def removeScan(state: ErgoWalletState, scanId: ScanId): Try[ErgoWalletState] =
    state.storage.getScan(scanId) match {
      case None =>
        Failure(new Exception(s"Scan #$scanId not found"))
      case Some(_) =>
        state.storage.removeScan(scanId).map { _ =>
          state.copy(walletVars = state.walletVars.removeScan(scanId))
        }
    }

  override def addScan(state: ErgoWalletState, scanRequest: ScanRequest): Try[(Scan, ErgoWalletState)] =
    state.storage.addScan(scanRequest).map { scan =>
      scan -> state.copy(walletVars = state.walletVars.addScan(scan))
    }

  override def getUnconfirmedTransactions(state: ErgoWalletState, scanId: ScanId): Seq[AugWalletTransaction] = {
    state.mempoolReaderOpt.flatMap { mempool =>
      state.storage.getScan(scanId).map { scan =>
        mempool.getAllPrioritized.filter { unconfirmedTx =>
          unconfirmedTx.transaction.outputs.exists(scan.trackingRule.filter)
        }.map { unconfirmedTx =>
          // unconfirmed transaction has 0 confirmations
          AugWalletTransaction(WalletTransaction(unconfirmedTx.transaction, state.fullHeight, Seq(scanId)), 0)
        }
      }
    }.getOrElse(Nil)
  }

  override def getScanTransactions(state: ErgoWalletState,
                          scanId: ScanId,
                          fullHeight: Int,
                          includeUnconfirmed: Boolean = false): Seq[AugWalletTransaction] = {
    val walletTxs =
      state.registry.allWalletTxs().filter(wtx => wtx.scanIds.contains(scanId))
        .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

    val unconfirmedTxs =
      if (includeUnconfirmed) {
        getUnconfirmedTransactions(state, scanId)
      } else {
        Nil
      }
    walletTxs ++ unconfirmedTxs
  }

  override def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
                      tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpendOpt: Option[Seq[ErgoBox]],
                      dataBoxesOpt: Option[Seq[ErgoBox]],
                      parameters: Parameters,
                      stateContext: ErgoStateContext)(extract: BoxId => Option[ErgoBox]): Try[ErgoTransaction] = {
    val boxesToSpend = boxesToSpendOpt.getOrElse(tx.inputs.flatMap { input =>
      extract(input.boxId)
    })
    val dataBoxes = dataBoxesOpt.getOrElse(tx.dataInputs.flatMap { dataInput =>
      extract(dataInput.boxId)
    })

    val proverSecrets = proverOpt.map(_.secretKeys).getOrElse(Seq.empty)
    val secretsWrapped = secrets.map(_.key).toIndexedSeq
    val secretsProver = ErgoProvingInterpreter(secretsWrapped ++ proverSecrets, parameters)
    secretsProver
      .sign(tx, boxesToSpend.toIndexedSeq, dataBoxes.toIndexedSeq, stateContext, hints)
      .map(ErgoTransaction.apply)
  }

}

object ErgoWalletService {
  /**
    * Result of "deriveNextKey" operation
    */
  case class DeriveNextKeyResult(result: Try[(DerivationPath, P2PKAddress, ExtendedSecretKey)])

  // A helper which is deserializing Base16-encoded boxes to ErgoBox instances
  def stringsToBoxes(strings: Seq[String]): Seq[ErgoBox] =
    strings.map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry)).map(_.get)

}
