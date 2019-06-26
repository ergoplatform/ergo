package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.nodeView.wallet.requests.TransactionRequest
import org.ergoplatform.wallet.boxes.ChainStatus.{OffChain, OnChain}
import org.ergoplatform.wallet.boxes.{ChainStatus, TrackedBox}
import org.ergoplatform.wallet.secrets.DerivationPath
import org.ergoplatform.{ErgoAddress, P2PKAddress}
import scorex.core.transaction.wallet.VaultReader
import sigmastate.basics.DLogProtocol.DLogProverInput

import scala.concurrent.Future
import scala.util.Try

trait ErgoWalletReader extends VaultReader {

  val walletActor: ActorRef

  private implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  def initWallet(pass: String, mnemonicPassOpt: Option[String]): Future[Try[String]] =
    (walletActor ? InitWallet(pass, mnemonicPassOpt)).mapTo[Try[String]]

  def restoreWallet(encryptionPass: String, mnemonic: String,
                    mnemonicPassOpt: Option[String] = None): Future[Try[Unit]] =
    (walletActor ? RestoreWallet(mnemonic, mnemonicPassOpt, encryptionPass)).mapTo[Try[Unit]]

  def unlockWallet(pass: String): Future[Try[Unit]] =
    (walletActor ? UnlockWallet(pass)).mapTo[Try[Unit]]

  def lockWallet(): Unit = walletActor ! LockWallet

  def deriveKey(path: String): Future[Try[P2PKAddress]] =
    (walletActor ? DeriveKey(path)).mapTo[Try[P2PKAddress]]

  def deriveNextKey: Future[Try[(DerivationPath, P2PKAddress)]] =
    (walletActor ? DeriveNextKey).mapTo[Try[(DerivationPath, P2PKAddress)]]

  def balances(chainStatus: ChainStatus): Future[RegistryIndex] =
    (walletActor ? ReadBalances(chainStatus)).mapTo[RegistryIndex]

  def confirmedBalances: Future[RegistryIndex] = balances(OnChain)

  def balancesWithUnconfirmed: Future[RegistryIndex] = balances(OffChain)

  def publicKeys(from: Int, to: Int): Future[Seq[P2PKAddress]] =
    (walletActor ? ReadPublicKeys(from, to)).mapTo[Seq[P2PKAddress]]

  def firstSecret: Future[Try[DLogProverInput]] =
    (walletActor ? GetFirstSecret).mapTo[Try[DLogProverInput]]

  def boxes(unspentOnly: Boolean = false): Future[Seq[WalletBox]] =
    (walletActor ? GetBoxes(unspentOnly)).mapTo[Seq[WalletBox]]

  def transactions: Future[Seq[ErgoTransaction]] =
    (walletActor ? GetTransactions).mapTo[Seq[ErgoTransaction]]

  def randomPublicKey: Future[P2PKAddress] =
    (walletActor ? ReadRandomPublicKey).mapTo[P2PKAddress]

  def trackedAddresses: Future[Seq[ErgoAddress]] =
    (walletActor ? ReadTrackedAddresses).mapTo[Seq[ErgoAddress]]

  def generateTransaction(requests: Seq[TransactionRequest]): Future[Try[ErgoTransaction]] =
    (walletActor ? GenerateTransaction(requests)).mapTo[Try[ErgoTransaction]]

}
