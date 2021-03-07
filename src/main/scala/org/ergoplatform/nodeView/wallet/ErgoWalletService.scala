package org.ergoplatform.nodeView.wallet

import cats.implicits._
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.{ErgoStateContext, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{InitWallet, RestoreWallet}
import org.ergoplatform.nodeView.wallet.ErgoWalletService.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.models.{ChangeBox, CollectedBoxes}
import org.ergoplatform.nodeView.wallet.persistence.WalletRegistry
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.utils.FileUtils
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.{BoxSelector, ErgoBoxSerializer}
import org.ergoplatform.wallet.interpreter.{ErgoProvingInterpreter, TransactionHintsBag}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey, JsonSecretStorage}
import org.ergoplatform.wallet.settings.SecretStorageSettings
import scorex.util.encode.Base16
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.SigmaBoolean

import java.io.File
import scala.util.{Failure, Success, Try}

trait ErgoWalletService {
  def setupWallet(state: ErgoWalletState, testMnemonic: Option[String], testKeysQty: Option[Int], secretStorage: SecretStorageSettings): ErgoWalletState
  def initWallet(state: ErgoWalletState, seedStrengthBits: Int, mnemonicPhraseLanguage: String, secretStorage: SecretStorageSettings, initWallet: InitWallet): Try[(String, ErgoWalletState)]
  def restoreWallet(state: ErgoWalletState, restoreWallet: RestoreWallet, secretStorageSettings: SecretStorageSettings): Try[ErgoWalletState]
  def unlockWallet(state: ErgoWalletState, walletPass: String, usePreEip3Derivation: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[ErgoWalletState]
  def rescanWallet(state: ErgoWalletState, registryFolder: File)(newRegistry: => WalletRegistry): Try[ErgoWalletState]
  def getWalletBoxes(state: ErgoWalletState, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox]
  def getScannedBoxes(state: ErgoWalletState, scanId: ScanId, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox]
  def getTransactions(registry: WalletRegistry, fullHeight: Int): Seq[AugWalletTransaction]
  def getTransactionsByTxId(txId: ModifierId, registry: WalletRegistry, fullHeight: Int): Option[AugWalletTransaction]
  def collectBoxes(state: ErgoWalletState, boxSelector: BoxSelector, targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long]): Try[CollectedBoxes]
  def deriveKeyFromPath(state: ErgoWalletState, encodedPath: String)(implicit addrEncoder: ErgoAddressEncoder): Try[(P2PKAddress, ErgoWalletState)]
  def deriveNextKey(state: ErgoWalletState, usePreEip3Derivation: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[(DeriveNextKeyResult, ErgoWalletState)]
  def getScanTransactions(scanId: ScanId, registry: WalletRegistry, fullHeight: Int): Seq[AugWalletTransaction]
  def addScan(state: ErgoWalletState, appRequest: ScanRequest): Try[(Scan, ErgoWalletState)]
  def removeScan(state: ErgoWalletState, scanId: ScanId): Try[ErgoWalletState]
  def updateUtxoSet(state: ErgoWalletState): ErgoWalletState
  def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock): ErgoWalletState
  def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
                      tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpendOpt: Option[Seq[ErgoBox]],
                      dataBoxesOpt: Option[Seq[ErgoBox]],
                      parameters: Parameters,
                      stateContext: ErgoStateContext)(extract: BoxId => Option[ErgoBox]): Try[ErgoTransaction]
  def generateTransaction(state: ErgoWalletState,
                          boxSelector: BoxSelector,
                          requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String],
                          dataInputsRaw: Seq[String],
                          sign: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[ErgoLikeTransactionTemplate[_]]
  def generateCommitments(state: ErgoWalletState,
                          unsignedTx: UnsignedErgoTransaction,
                          externalSecretsOpt: Option[Seq[ExternalSecret]],
                          externalInputsOpt: Option[Seq[ErgoBox]],
                          externalDataInputsOpt: Option[Seq[ErgoBox]]): Try[TransactionHintsBag]
  def extractHints(state: ErgoWalletState,
                   tx: ErgoTransaction,
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean],
                   boxesToSpendOpt: Option[Seq[ErgoBox]],
                   dataBoxesOpt: Option[Seq[ErgoBox]]): TransactionHintsBag
}

class ErgoWalletServiceImpl extends ErgoWalletService with ErgoWalletSupport {

  def setupWallet(state: ErgoWalletState, testMnemonic: Option[String], testKeysQty: Option[Int], secretStorage: SecretStorageSettings): ErgoWalletState = {
    testMnemonic match {
      case Some(mnemonic) =>
        log.warn("Setting up wallet in test mode. Switch to secure mode for production usage.")
        val prover = buildProverFromMnemonic(mnemonic, testKeysQty, state.parameters)
        state.copy(walletVars = state.walletVars.withProver(prover))
      case None =>
        log.info("Trying to read wallet in secure mode ..")
        JsonSecretStorage.readFile(secretStorage).fold(
          e => {
            log.info(s"Failed to read wallet. Manual initialization is required to sign transactions. Cause: ${e.getCause}")
            state
          },
          secretStorage => {
            log.info("Wallet loaded successfully and locked")
            state.copy(secretStorageOpt = Some(secretStorage))
          }
        )
    }
  }

  def initWallet(state: ErgoWalletState, seedStrengthBits: Int, mnemonicPhraseLanguage: String, secretStorage: SecretStorageSettings, initWallet: InitWallet): Try[(String, ErgoWalletState)] = {
    //Read high-quality random bits from Java's SecureRandom
    val entropy = scorex.utils.Random.randomBytes(seedStrengthBits / 8)
    log.warn("Initializing wallet")
    val result =
      new Mnemonic(mnemonicPhraseLanguage, seedStrengthBits)
        .toMnemonic(entropy)
        .flatMap { mnemonic =>
          Try(JsonSecretStorage.init(Mnemonic.toSeed(mnemonic, initWallet.mnemonicPassOpt), initWallet.walletPass)(secretStorage))
            .map( newSecretStorage => mnemonic -> state.copy(secretStorageOpt = Some(newSecretStorage)))
        }
    java.util.Arrays.fill(entropy, 0: Byte)
    result
  }

  def restoreWallet(state: ErgoWalletState, restoreWallet: RestoreWallet, secretStorageSettings: SecretStorageSettings): Try[ErgoWalletState] =
    Try(JsonSecretStorage.restore(restoreWallet.mnemonic, restoreWallet.mnemonicPassOpt, restoreWallet.walletPass, secretStorageSettings))
      .map (secretStorage => state.copy(secretStorageOpt = Some(secretStorage)))


  def unlockWallet(state: ErgoWalletState, walletPass: String, usePreEip3Derivation: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[ErgoWalletState] =
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

  def rescanWallet(state: ErgoWalletState, registryFolder: File)(newRegistry: => WalletRegistry): Try[ErgoWalletState] =
    Try {
      // We do wallet rescan by closing the wallet's database, deleting it from the disk, then reopening it and sending a rescan signal.
      log.info(s"Rescanning the wallet, the registry is in $registryFolder")
      state.registry.close()
      FileUtils.deleteRecursive(registryFolder)
      state.copy(registry = newRegistry)
    }

  def getWalletBoxes(state: ErgoWalletState, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox] = {
    val currentHeight = state.fullHeight
    val boxes = if (unspentOnly) {
      val confirmed = state.registry.walletUnspentBoxes()
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

  def getScannedBoxes(state: ErgoWalletState, scanId: ScanId, unspentOnly: Boolean, considerUnconfirmed: Boolean): Seq[WalletBox] = {
    val unconfirmed = if (considerUnconfirmed) {
      state.offChainRegistry.offChainBoxes.filter(_.scans.contains(scanId))
    } else Seq.empty

    val currentHeight = state.fullHeight
    val boxes = (if (unspentOnly) {
      state.registry.unspentBoxes(scanId)
    } else {
      state.registry.confirmedBoxes(scanId)
    }) ++ unconfirmed
    boxes.map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)
  }

  def getTransactions(registry: WalletRegistry, fullHeight: Int): Seq[AugWalletTransaction] =
    registry.allWalletTxs()
      .sortBy(-_.inclusionHeight)
      .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

  def getTransactionsByTxId(txId: ModifierId, registry: WalletRegistry, fullHeight: Int): Option[AugWalletTransaction] =
    registry.getTx(txId)
      .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

  /**
    * Collects boxes according to given request
    *
    * @param targetBalance - Balance requested by user
    * @param targetAssets  - IDs and amounts of other tokens
    * @return collected ErgoBoxes and ChangeBoxes
    */
  def collectBoxes(state: ErgoWalletState, boxSelector: BoxSelector, targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long]): Try[CollectedBoxes] = {
    val assetsMap = targetAssets.map(t => bytesToId(t._1) -> t._2)
    val inputBoxes = state.getBoxesToSpend
    boxSelector
      .select(inputBoxes.iterator, state.walletFilter, targetBalance, assetsMap)
      .leftMap(m => new Exception(m.message))
      .map { res =>
        val ergoBoxes = res.boxes.map(_.box)
        val changeBoxes = res.changeBoxes.map(b => ChangeBox(b.value, b.tokens))
        CollectedBoxes(ergoBoxes, changeBoxes)
      }.toTry
  }

  def generateTransaction(state: ErgoWalletState,
                          boxSelector: BoxSelector,
                          requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String],
                          dataInputsRaw: Seq[String],
                          sign: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[ErgoLikeTransactionTemplate[_]] = {
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

  def generateCommitments(state: ErgoWalletState,
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

  def extractHints(state: ErgoWalletState,
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

  def deriveKeyFromPath(state: ErgoWalletState, encodedPath: String)(implicit addrEncoder: ErgoAddressEncoder): Try[(P2PKAddress, ErgoWalletState)] =
    state.secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        val rootSecret = secretStorage.secret.get // unlocked means Some(secret)
        DerivationPath.fromEncoded(encodedPath) match {
          case Success(path) if !path.publicBranch =>
            val secret = rootSecret.derive(path).asInstanceOf[ExtendedSecretKey]
            addSecretToStorage(state, secret) match {
              case Success(newState) =>
                Success(P2PKAddress(secret.publicKey.key) -> newState)
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

  def deriveNextKey(state: ErgoWalletState, usePreEip3Derivation: Boolean)(implicit addrEncoder: ErgoAddressEncoder): Try[(DeriveNextKeyResult, ErgoWalletState)] =
    state.secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        val masterKey = secretStorage.secret.get // unlocked means Some(secret)
        deriveNextKeyForMasterKey(state, masterKey, usePreEip3Derivation)
      case Some(_) =>
        Failure(new Exception("Unable to derive key, wallet is locked"))
      case None =>
        Failure(new Exception("Unable to derive key, wallet is not initialized"))
    }

  /**
    * Process the block transactions and update database and in-memory structures for offchain data accordingly
    * @param block - block to scan
    */
  def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock): ErgoWalletState = {
    val (reg, offReg, updatedOutputsFilter) =
      WalletScanLogic.scanBlockTransactions(state.registry, state.offChainRegistry, state.stateContext, state.walletVars, block, state.outputsFilter)
    state.copy(registry = reg, offChainRegistry = offReg, outputsFilter = Some(updatedOutputsFilter))
  }

  def updateUtxoSet(state: ErgoWalletState): ErgoWalletState = {
    (state.mempoolReaderOpt, state.stateReaderOpt) match {
      case (Some(mr), Some(sr)) =>
        sr match {
          case u: UtxoStateReader =>
            state.copy(utxoReaderOpt = Some(u.withMempool(mr)))
          case _ =>
            state
        }
      case (_, _) =>
        state
    }
  }


  def removeScan(state: ErgoWalletState, scanId: ScanId): Try[ErgoWalletState] =
    state.storage.getScan(scanId) match {
      case None =>
        Failure(new Exception(s"Scan #$scanId not found"))
      case Some(_) =>
        Try {
          state.storage.removeScan(scanId)
          state.copy(walletVars = state.walletVars.removeScan(scanId))
        }
    }

  def addScan(state: ErgoWalletState, appRequest: ScanRequest): Try[(Scan, ErgoWalletState)] =
    state.storage.addScan(appRequest).map { scan =>
      scan -> state.copy(walletVars = state.walletVars.addScan(scan))
    }

  def getScanTransactions(scanId: ScanId, registry: WalletRegistry, fullHeight: Int): Seq[AugWalletTransaction] = {
    registry.allWalletTxs().filter(wtx => wtx.scanIds.contains(scanId))
      .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

  }

  def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
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
