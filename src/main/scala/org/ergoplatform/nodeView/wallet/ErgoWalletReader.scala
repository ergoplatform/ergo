package org.ergoplatform.nodeView.wallet

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.{ErgoAddress, ErgoBox, ErgoLikeTransactionTemplate, P2PKAddress}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.wallet.ErgoWalletActor._
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.nodeView.wallet.requests.{ExternalSecret, TransactionGenerationRequest}
import org.ergoplatform.wallet.boxes.ChainStatus
import org.ergoplatform.wallet.boxes.ChainStatus.{OffChain, OnChain}
import org.ergoplatform.wallet.secrets.DerivationPath
import scorex.core.transaction.wallet.VaultReader
import scorex.util.ModifierId
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.HintsBag

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

  def updateChangeAddress(address: P2PKAddress): Unit =
    walletActor ! UpdateChangeAddress(address)

  def transactions: Future[Seq[AugWalletTransaction]] =
    (walletActor ? GetTransactions).mapTo[Seq[AugWalletTransaction]]

  def transactionById(id: ModifierId): Future[Option[AugWalletTransaction]] =
    (walletActor ? GetTransaction(id)).mapTo[Option[AugWalletTransaction]]

  def randomPublicKey: Future[P2PKAddress] =
    (walletActor ? ReadRandomPublicKey).mapTo[P2PKAddress]

  def trackedAddresses: Future[Seq[ErgoAddress]] =
    (walletActor ? ReadTrackedAddresses).mapTo[Seq[ErgoAddress]]

  def generateTransaction(requests: Seq[TransactionGenerationRequest],
                          sign: Boolean,
                          inputsRaw: Seq[String] = Seq.empty,
                          dataInputsRaw: Seq[String] = Seq.empty): Future[Try[ErgoLikeTransactionTemplate[_]]] =
    (walletActor ? GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign)).mapTo[Try[ErgoLikeTransactionTemplate[_]]]

  def signTransaction(tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: HintsBag,
                      boxesToSpend: Seq[ErgoBox],
                      dataBoxes: Seq[ErgoBox]): Future[Try[ErgoTransaction]] =
    (walletActor ? SignTransaction(tx, secrets, hints, boxesToSpend, dataBoxes)).mapTo[Try[ErgoTransaction]]

  def extractHints(tx: ErgoTransaction,
                   boxesToSpend: IndexedSeq[ErgoBox],
                   dataBoxes: IndexedSeq[ErgoBox],
                   real: Seq[SigmaBoolean],
                   simulated: Seq[SigmaBoolean]): Future[HintsBag] =
    (walletActor ? ExtractHints(tx, boxesToSpend, dataBoxes, real, simulated)).mapTo[HintsBag]

}
