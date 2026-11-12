package org.ergoplatform.nodeView.wallet

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.requests.{BoxesRequest, ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.ScanRequest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.ChainStatus
import org.ergoplatform.wallet.boxes.ChainStatus.{OffChain, OnChain}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform.{ErgoBox, NodeViewComponent, P2PKAddress}
import scorex.util.ModifierId
import sigma.data.SigmaBoolean

import java.util.concurrent.TimeUnit
import scala.concurrent.Future

trait ErgoWalletReader extends NodeViewComponent {

  val walletActor: ActorRef

  private implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  /** Returns the Future generated mnemonic phrase.
    * @param pass   storage encription password
    * @param mnemonicPassOpt  mnemonic encription password
    * @return  menmonic phrase for the new wallet
    */
  def initWallet(pass: SecretString, mnemonicPassOpt: Option[SecretString]): Future[InitWalletResponse] =
    (walletActor ? InitWallet(pass, mnemonicPassOpt)).mapTo[InitWalletResponse]

  def restoreWallet(encryptionPass: SecretString, mnemonic: SecretString,
                    mnemonicPassOpt: Option[SecretString] = None, usePre1627KeyDerivation: Boolean): Future[RestoreWalletResponse] =
    (walletActor ? RestoreWallet(mnemonic, mnemonicPassOpt, encryptionPass, usePre1627KeyDerivation)).mapTo[RestoreWalletResponse]

  def unlockWallet(pass: SecretString): Future[UnlockWalletResponse] =
    (walletActor ? UnlockWallet(pass)).mapTo[UnlockWalletResponse]

  def lockWallet(): Unit = walletActor ! LockWallet

  def rescanWallet(fromHeight: Height): Future[RescanWalletResponse] =
    (walletActor ? RescanWallet(fromHeight)).mapTo[RescanWalletResponse]

  def getWalletStatus: Future[WalletStatus] =
    (walletActor ? GetWalletStatus).mapTo[WalletStatus]

  def checkSeed(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString] = None): Future[CheckSeedResponse] =
    (walletActor ? CheckSeed(mnemonic, mnemonicPassOpt)).mapTo[CheckSeedResponse]

  def deriveKey(path: DerivationPath): Future[DeriveKeyResponse] =
    (walletActor ? DeriveKey(path)).mapTo[DeriveKeyResponse]

  def deriveNextKey: Future[DeriveNextKeyResult] =
    (walletActor ? DeriveNextKey).mapTo[DeriveNextKeyResult]

  def balances(chainStatus: ChainStatus): Future[BalancesResponse] =
    (walletActor ? ReadBalances(chainStatus)).mapTo[BalancesResponse]

  def confirmedBalances: Future[BalancesResponse] = balances(OnChain)

  def balancesWithUnconfirmed: Future[BalancesResponse] = balances(OffChain)

  def publicKeys(from: Int, to: Int): Future[PublicKeysResponse] =
    (walletActor ? ReadPublicKeys(from, to)).mapTo[PublicKeysResponse]

  def allExtendedPublicKeys(): Future[ExtendedPublicKeysResponse] =
    (walletActor ? ReadExtendedPublicKeys()).mapTo[ExtendedPublicKeysResponse]

  def getPrivateKeyFromPath(path: DerivationPath): Future[PrivateKeyFromPathResponse] =
    (walletActor ? GetPrivateKeyFromPath(path)).mapTo[PrivateKeyFromPathResponse]

  def walletBoxes(unspentOnly: Boolean, considerUnconfirmed: Boolean): Future[WalletBoxesResponse] =
    (walletActor ? GetWalletBoxes(unspentOnly, considerUnconfirmed)).mapTo[WalletBoxesResponse]

  def scanUnspentBoxes(scanId: ScanId, considerUnconfirmed: Boolean, minHeight: Height, maxHeight: Height): Future[ScanBoxesResponse] =
    (walletActor ? GetScanUnspentBoxes(scanId, considerUnconfirmed, minHeight, maxHeight)).mapTo[ScanBoxesResponse]

  def scanSpentBoxes(scanId: ScanId): Future[ScanBoxesResponse] =
    (walletActor ? GetScanSpentBoxes(scanId)).mapTo[ScanBoxesResponse]

  def updateChangeAddress(address: P2PKAddress): Future[Unit] =
    walletActor.askWithStatus(UpdateChangeAddress(address)).mapTo[Unit]

  def transactions: Future[WalletTransactionsResponse] =
    (walletActor ? GetTransactions).mapTo[WalletTransactionsResponse]

  def transactionById(id: ModifierId): Future[WalletTransactionResponse] =
    (walletActor ? GetTransaction(id)).mapTo[WalletTransactionResponse]

  def generateTransaction(requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String] = Seq.empty,
                          dataInputsRaw: Seq[String] = Seq.empty): Future[SignedTransactionResponse] =
    (walletActor ? GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign = true))
      .mapTo[SignedTransactionResponse]

  def generateCommitmentsFor(unsignedErgoTransaction: UnsignedErgoTransaction,
                             externalSecretsOpt: Option[Seq[ExternalSecret]],
                             boxesToSpend: Option[Seq[ErgoBox]],
                             dataBoxes: Option[Seq[ErgoBox]]): Future[GenerateCommitmentsResponse] =
    (walletActor ? GenerateCommitmentsFor(unsignedErgoTransaction, externalSecretsOpt, boxesToSpend, dataBoxes))
      .mapTo[GenerateCommitmentsResponse]


  def generateUnsignedTransaction(requests: Seq[TransactionGenerationRequest],
                          inputsRaw: Seq[String] = Seq.empty,
                          dataInputsRaw: Seq[String] = Seq.empty): Future[UnsignedTransactionResponse] =
    (walletActor ? GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign = false)).mapTo[UnsignedTransactionResponse]


  def signTransaction(tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpend: Option[Seq[ErgoBox]],
                      dataBoxes: Option[Seq[ErgoBox]]): Future[SignedTransactionResponse] =
    (walletActor ? SignTransaction(tx, secrets, hints, boxesToSpend, dataBoxes)).mapTo[SignedTransactionResponse]

  def extractHints(tx: ErgoTransaction,
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean],
                   boxesToSpend: Option[Seq[ErgoBox]],
                   dataBoxes: Option[Seq[ErgoBox]]): Future[ExtractHintsResult] =
    (walletActor ? ExtractHints(tx, real, simulated, boxesToSpend, dataBoxes)).mapTo[ExtractHintsResult]

  def addScan(appRequest: ScanRequest): Future[AddScanResponse] =
    (walletActor ? AddScan(appRequest)).mapTo[AddScanResponse]

  def removeScan(scanId: ScanId): Future[RemoveScanResponse] =
    (walletActor ? RemoveScan(scanId)).mapTo[RemoveScanResponse]

  def readScans(): Future[ReadScansResponse] =
    (walletActor ? ReadScans).mapTo[ReadScansResponse]

  def stopTracking(scanId: ScanId, boxId: BoxId): Future[StopTrackingResponse] =
    (walletActor ? StopTracking(scanId, boxId)).mapTo[StopTrackingResponse]

  def addBox(box: ErgoBox, scanIds: Set[ScanId]): Future[AddBoxResponse] =
    (walletActor ? AddBox(box, scanIds)).mapTo[AddBoxResponse]

  def collectBoxes(request: BoxesRequest): Future[ReqBoxesResponse] =
    (walletActor ? CollectWalletBoxes(request.targetBalance, request.targetAssets)).mapTo[ReqBoxesResponse]

  def transactionsByScanId(scanId: ScanId, includeUnconfirmed: Boolean): Future[ScanRelatedTxsResponse] =
    (walletActor ? GetScanTransactions(scanId, includeUnconfirmed)).mapTo[ScanRelatedTxsResponse]

  /**
    * Get filtered scan-related txs
    * @param scanIds - scan identifiers
    * @param minHeight - minimal tx inclusion height
    * @param maxHeight - maximal tx inclusion height
    * @param minConfNum - minimal confirmations number
    * @param maxConfNum - maximal confirmations number
    * @param includeUnconfirmed - whether to include transactions from mempool that match given scanId
    */
  def filteredScanTransactions(scanIds: List[ScanId],
                               minHeight: Height,
                               maxHeight: Height,
                               minConfNum: Int,
                               maxConfNum: Int,
                               includeUnconfirmed: Boolean): Future[WalletTransactionsResponse] =
    (walletActor ? GetFilteredScanTxs(scanIds, minHeight, maxHeight, minConfNum, maxConfNum, includeUnconfirmed)).mapTo[WalletTransactionsResponse]

}
