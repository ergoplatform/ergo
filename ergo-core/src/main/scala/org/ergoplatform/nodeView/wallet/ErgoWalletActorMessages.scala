package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.models.CollectedBoxes
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest}
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.ChainStatus
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform._
import scorex.core.VersionTag
import scorex.util.ModifierId
import sigmastate.Values.SigmaBoolean
import sigmastate.crypto.DLogProtocol.{DLogProverInput, ProveDlog}
import scala.util.Try

object ErgoWalletActorMessages {

  // Private signals the wallet actor sends to itself
  /**
   * A signal the wallet actor sends to itself to scan a block in the past
   *
   * @param blockHeight - height of a block to scan
   * @param rescan - scan a block even if height is out of order, to serve rescan requests from arbitrary height
   */
  final case class ScanInThePast(blockHeight: ErgoHistory.Height, rescan: Boolean)


  // Publicly available signals for the wallet actor

  /**
   * Command to scan offchain transaction
   *
   * @param tx - offchain transaction
   */
  final case class ScanOffChain(tx: ErgoTransaction)

  /**
   * Command to scan a block
   *
   * @param block - block to scan
   */
  final case class ScanOnChain(block: ErgoFullBlock)

  /**
   * Rollback to previous version of the wallet, by throwing away effects of blocks after the version
   *
   * @param version
   */
  final case class Rollback(version: VersionTag)

  /**
   * Generate new transaction fulfilling given requests
   *
   * @param requests
   * @param inputsRaw
   * @param dataInputsRaw
   * @param sign
   */
  final case class GenerateTransaction(requests: Seq[TransactionGenerationRequest],
                                       inputsRaw: Seq[String],
                                       dataInputsRaw: Seq[String],
                                       sign: Boolean)

  /**
   * Request to generate commitments for an unsigned transaction
   *
   * @param utx           - unsigned transaction
   * @param secrets       - optionally, externally provided secrets
   * @param inputsOpt     - optionally, externally provided inputs
   * @param dataInputsOpt - optionally, externally provided inputs
   */
  case class GenerateCommitmentsFor(utx: UnsignedErgoTransaction,
                                    secrets: Option[Seq[ExternalSecret]],
                                    inputsOpt: Option[Seq[ErgoBox]],
                                    dataInputsOpt: Option[Seq[ErgoBox]])

  /**
   * Response for commitments generation request
   *
   * @param response - hints to sign a transaction
   */
  case class GenerateCommitmentsResponse(response: Try[TransactionHintsBag])

  /**
   * A request to sign a transaction
   *
   * @param utx          - unsigned transaction
   * @param secrets      - additional secrets given to the prover
   * @param hints        - hints used for transaction signing (commitments and partial proofs)
   * @param boxesToSpend - boxes the transaction is spending
   * @param dataBoxes    - read-only inputs of the transaction
   */
  case class SignTransaction(utx: UnsignedErgoTransaction,
                             secrets: Seq[ExternalSecret],
                             hints: TransactionHintsBag,
                             boxesToSpend: Option[Seq[ErgoBox]],
                             dataBoxes: Option[Seq[ErgoBox]])

  /**
   *
   * @param chainStatus
   */
  final case class ReadBalances(chainStatus: ChainStatus)

  /**
   * Read a slice of wallet public keys
   *
   * @param from
   * @param until
   */
  final case class ReadPublicKeys(from: Int, until: Int)

  /**
   * Read all wallet public keys
   */
  final case class ReadExtendedPublicKeys()

  /**
   * Get the private key from seed based on a given derivation path
   */
  final case class GetPrivateKeyFromPath(path: DerivationPath)

  /**
   * Read wallet either from mnemonic or from secret storage
   */
  final case class ReadWallet(state: ErgoWalletState)

  /**
   * Initialize wallet with given wallet pass and optional mnemonic pass (according to BIP-32)
   *
   * @param walletPass
   * @param mnemonicPassOpt
   */
  final case class InitWallet(walletPass: SecretString, mnemonicPassOpt: Option[SecretString])

  /**
   * Restore wallet with mnemonic, optional mnemonic password and (mandatory) wallet encryption password
   *
   * @param mnemonic
   * @param mnemonicPassOpt
   * @param walletPass
   */
  final case class RestoreWallet(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString], walletPass: SecretString, usePre1627KeyDerivation: Boolean)

  /**
   * Unlock wallet with wallet password
   *
   * @param walletPass
   */
  final case class UnlockWallet(walletPass: SecretString)

  /**
   * Derive key with given path according to BIP-32
   *
   * @param path
   */
  final case class DeriveKey(path: String)

  /**
   * Get boxes related to P2PK payments
   *
   * @param unspentOnly         - return only unspent boxes
   * @param considerUnconfirmed - consider mempool (filter our unspent boxes spent in the pool if unspent = true, add
   *                            boxes created in the pool for both values of unspentOnly).
   */
  final case class GetWalletBoxes(unspentOnly: Boolean, considerUnconfirmed: Boolean)

  /**
   * Get boxes by requested params
   *
   * @param targetBalance - Balance requested by user
   * @param targetAssets  - IDs and amounts of other tokens
   */
  final case class CollectWalletBoxes(targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long])

  /**
   * Wallet's response for requested boxes
   *
   * @param result
   */
  final case class ReqBoxesResponse(result: Try[CollectedBoxes])

  /**
   * Get scan related transactions
   *
   * @param scanId  - Scan identifier
   * @param includeUnconfirmed  - whether to include transactions from mempool that match given scanId
   */
  final case class GetScanTransactions(scanId: ScanId, includeUnconfirmed: Boolean)

  /**
   * Response for requested scan related transactions
   *
   * @param result
   */
  final case class ScanRelatedTxsResponse(result: Seq[AugWalletTransaction])

  /**
   * Get unspent boxes related to a scan
   *
   * @param scanId              - scan identifier
   * @param considerUnconfirmed - consider boxes from mempool
   * @param minHeight - min inclusion height of unspent boxes
   * @param maxHeight - max inclusion height of unspent boxes
   */
  final case class GetScanUnspentBoxes(scanId: ScanId, considerUnconfirmed: Boolean, minHeight: Int, maxHeight: Int)

  /**
   * Get spent boxes related to a scan
   *
   * @param scanId - scan identifier
   */
  final case class GetScanSpentBoxes(scanId: ScanId)

  /**
   * Set or update address for change outputs. Initially the address is set to root key address
   *
   * @param address
   */
  final case class UpdateChangeAddress(address: P2PKAddress)

  /**
   * Command to register new scan
   *
   * @param appRequest
   */
  final case class AddScan(appRequest: ScanRequest)

  /**
   * Wallet's response for scan registration request
   *
   * @param response
   */
  final case class AddScanResponse(response: Try[Scan])

  /**
   * Command to deregister a scan
   *
   * @param scanId
   */
  final case class RemoveScan(scanId: ScanId)

  /**
   * Wallet's response for scan removal request
   *
   * @param response
   */
  final case class RemoveScanResponse(response: Try[Unit])

  /**
   * Get wallet-related transaction
   *
   * @param id
   */
  final case class GetTransaction(id: ModifierId)

  final case class CheckSeed(mnemonic: SecretString, passOpt: Option[SecretString])

  /**
   * Get wallet-related transaction
   */
  case object GetTransactions

  /**
   * Get filtered scan-related txs
   * @param scanIds - scan identifiers
   * @param minHeight - minimal tx inclusion height
   * @param maxHeight - maximal tx inclusion height
   * @param minConfNum - minimal confirmations number
   * @param maxConfNum - maximal confirmations number
   * @param includeUnconfirmed - whether to include transactions from mempool that match given scanId
   */
  case class GetFilteredScanTxs(scanIds: List[ScanId],
                                minHeight: Int,
                                maxHeight: Int,
                                minConfNum: Int,
                                maxConfNum: Int,
                                includeUnconfirmed: Boolean)

  /**
   * Derive next key-pair according to BIP-32
   * //todo: describe procedure or provide a link
   */
  case object DeriveNextKey

  /**
   * Lock wallet
   */
  case object LockWallet

  /**
   * Close wallet
   */
  case object CloseWallet

  /**
   * Rescan wallet
   */
  case class RescanWallet(fromHeight: Int)

  /**
   * Get wallet status
   */
  case object GetWalletStatus

  /**
   * Wallet status. To be sent in response to GetWalletStatus
   *
   * @param initialized   - whether wallet is initialized or not
   * @param unlocked      - whether wallet is unlocked or not
   * @param changeAddress - address used for change (optional)
   * @param height        - last height scanned
   */
  case class WalletStatus(initialized: Boolean,
                          unlocked: Boolean,
                          changeAddress: Option[P2PKAddress],
                          height: ErgoHistory.Height,
                          error: Option[String])

  /**
   * Get root secret key (used in miner)
   */
  case object GetFirstSecret

  /**
   * Response with root secret key (used in miner)
   */
  case class FirstSecretResponse(secret: Try[DLogProverInput])

  /**
   * Get mining public key
   */
  case object GetMiningPubKey

  /**
   * Response with mining public key
   */
  case class MiningPubKeyResponse(miningPubKeyOpt: Option[ProveDlog])

  /**
   * Get registered scans list
   */
  case object ReadScans

  /**
   * Get registered scans list
   */
  case class ReadScansResponse(apps: Seq[Scan])

  /**
   * Remove association between a scan and a box (remove a box if its the only one which belongs to the
   * scan)
   *
   * @param scanId
   * @param boxId
   */
  case class StopTracking(scanId: ScanId, boxId: BoxId)

  /**
   * Wrapper for a result of StopTracking processing
   *
   * @param status
   */
  case class StopTrackingResponse(status: Try[Unit])

  /**
   * A request to extract hints from given transaction
   *
   * @param tx            - transaction to extract hints from
   * @param real          - public keys corresponing to the secrets known
   * @param simulated     - public keys to simulate
   * @param inputsOpt     - optionally, externally provided inputs
   * @param dataInputsOpt - optionally, externally provided inputs
   */
  case class ExtractHints(tx: ErgoTransaction,
                          real: Seq[SigmaBoolean],
                          simulated: Seq[SigmaBoolean],
                          inputsOpt: Option[Seq[ErgoBox]],
                          dataInputsOpt: Option[Seq[ErgoBox]])

  /**
   * Result of hints generation operation
   *
   * @param transactionHintsBag - hints for transaction
   */
  case class ExtractHintsResult(transactionHintsBag: TransactionHintsBag)


  /**
   * Add association between a scan and a box (and add the box to the database if it is not there)
   *
   * @param box
   * @param scanIds
   *
   */
  case class AddBox(box: ErgoBox, scanIds: Set[ScanId])

  /**
   * Wrapper for a result of AddBox processing
   *
   * @param status
   */
  case class AddBoxResponse(status: Try[Unit])

}
