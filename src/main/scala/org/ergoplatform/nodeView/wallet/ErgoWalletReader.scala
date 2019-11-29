package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.persistence.RegistryDigest
import org.ergoplatform.nodeView.wallet.requests.TransactionRequest
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplication}
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.wallet.boxes.ChainStatus
import org.ergoplatform.wallet.boxes.ChainStatus.{OffChain, OnChain}
import org.ergoplatform.wallet.secrets.DerivationPath
import org.ergoplatform.{ErgoAddress, P2PKAddress}
import scorex.core.transaction.wallet.VaultReader
import scorex.util.ModifierId
import sigmastate.basics.DLogProtocol.DLogProverInput

import scala.concurrent.Future
import scala.util.Try

trait ErgoWalletReader extends VaultReader {

  val walletActor: ActorRef

  private implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  /** Returns the Future generated mnemonic phrase.
    * @param pass   storage encription password
    * @param mnemonicPassOpt  mnemonic encription password
    * @return  menmonic phrase for the new wallet
    */
  def initWallet(pass: String, mnemonicPassOpt: Option[String]): Future[Try[String]] =
    (walletActor ? InitWallet(pass, mnemonicPassOpt)).mapTo[Try[String]]

  def restoreWallet(encryptionPass: String, mnemonic: String,
                    mnemonicPassOpt: Option[String] = None): Future[Try[Unit]] =
    (walletActor ? RestoreWallet(mnemonic, mnemonicPassOpt, encryptionPass)).mapTo[Try[Unit]]

  def unlockWallet(pass: String): Future[Try[Unit]] =
    (walletActor ? UnlockWallet(pass)).mapTo[Try[Unit]]

  def lockWallet(): Unit = walletActor ! LockWallet

  def getLockStatus: Future[(Boolean, Boolean)] =
    (walletActor ? GetLockStatus).mapTo[(Boolean, Boolean)]

  def deriveKey(path: String): Future[Try[P2PKAddress]] =
    (walletActor ? DeriveKey(path)).mapTo[Try[P2PKAddress]]

  def deriveNextKey: Future[Try[(DerivationPath, P2PKAddress)]] =
    (walletActor ? DeriveNextKey).mapTo[Try[(DerivationPath, P2PKAddress)]]

  def balances(chainStatus: ChainStatus): Future[RegistryDigest] =
    (walletActor ? ReadBalances(chainStatus)).mapTo[RegistryDigest]

  def confirmedBalances: Future[RegistryDigest] = balances(OnChain)

  def balancesWithUnconfirmed: Future[RegistryDigest] = balances(OffChain)

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] =
    (walletActor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]

  def firstSecret: Future[Try[DLogProverInput]] =
    (walletActor ? GetFirstSecret).mapTo[Try[DLogProverInput]]

  def walletBoxes(unspentOnly: Boolean = false): Future[Seq[WalletBox]] =
    (walletActor ? GetWalletBoxes(unspentOnly)).mapTo[Seq[WalletBox]]

  def appBoxes(appId: AppId, unspentOnly: Boolean = false): Future[Seq[WalletBox]] =
    (walletActor ? GetAppBoxes(appId, unspentOnly)).mapTo[Seq[WalletBox]]

  def uncertainBoxes(appId: AppId): Future[Seq[WalletBox]] =
    (walletActor ? GetUncertainBoxes(appId)).mapTo[Seq[WalletBox]]

  def updateChangeAddress(address: P2PKAddress): Unit =
    walletActor ! UpdateChangeAddress(address)

  def transactions: Future[Seq[AugWalletTransaction]] =
    (walletActor ? GetTransactions).mapTo[Seq[AugWalletTransaction]]

  def transactionById(id: ModifierId): Future[Option[AugWalletTransaction]] =
    (walletActor ? GetTransaction(id)).mapTo[Option[AugWalletTransaction]]

  def randomPublicKey: Future[P2PKAddress] =
    (walletActor ? ReadRandomPublicKey).mapTo[P2PKAddress]

  def generateTransaction(requests: Seq[TransactionRequest],
                          inputsRaw: Seq[String] = Seq.empty): Future[Try[ErgoTransaction]] =
    (walletActor ? GenerateTransaction(requests, inputsRaw)).mapTo[Try[ErgoTransaction]]

  def addApplication(appRequest: ExternalAppRequest): Future[Try[ExternalApplication]] =
    (walletActor ? AddApplication(appRequest)).mapTo[Try[ExternalApplication]]

  def removeApplication(appId: AppId): Future[Try[Unit]] =
    (walletActor ? RemoveApplication(appId)).mapTo[Try[Unit]]

  def readApplications(): Future[Seq[ExternalApplication]] =
    (walletActor ? ReadApplications).mapTo[Seq[ExternalApplication]]

  def makeCertain(appId: AppId, boxId: BoxId): Future[Try[Unit]] =
    (walletActor ? MakeCertain(appId, boxId)).mapTo[Try[Unit]]

  def stopTracking(appId: AppId, boxId: BoxId): Future[Try[Unit]] =
    (walletActor ? StopTracking(appId, boxId)).mapTo[Try[Unit]]

}
