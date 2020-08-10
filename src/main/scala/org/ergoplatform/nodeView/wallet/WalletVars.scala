package org.ergoplatform.nodeView.wallet

import com.google.common.hash.BloomFilter
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.secrets.{ExtendedPublicKey, ExtendedSecretKey}
import scorex.util.ScorexLogging
import sigmastate.Values

import scala.util.Try

/**
  * Inner class of the wallet actor which it encapsulating its mutable state (aside of the databases
  * the actor modifies). The main intention behind the class is to make modifications of this part of the internal
  * state explicit and unit-testable.
  *
  * @param proverOpt
  * @param externalScans
  * @param stateCacheProvided
  * @param settings
  */
final case class WalletVars(proverOpt: Option[ErgoProvingInterpreter],
                            externalScans: Seq[Scan],
                            stateCacheProvided: Option[WalletCache] = None)
                           (implicit val settings: ErgoSettings) extends ScorexLogging {

  private[wallet] implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  val stateCacheOpt: Option[WalletCache] =
    stateCacheProvided.orElse(proverOpt.map(p => WalletCache(p.hdPubKeys, settings)))

  val trackedPubKeys: Seq[ExtendedPublicKey] = stateCacheOpt.map(_.trackedPubKeys).getOrElse(Seq.empty)

  val publicKeyAddresses: Seq[P2PKAddress] = stateCacheOpt.map(_.publicKeyAddresses).getOrElse(Seq.empty)

  val trackedBytes: Seq[Array[Byte]] = stateCacheOpt.map(_.trackedBytes).getOrElse(Seq.empty)

  val miningScripts: Seq[Values.ErgoTree] = stateCacheOpt.map(_.miningScripts).getOrElse(Seq.empty)

  val miningScriptsBytes: Seq[Array[Byte]] = stateCacheOpt.map(_.miningScriptsBytes).getOrElse(Seq.empty)

  val filter: BloomFilter[Array[Byte]] =
    stateCacheOpt.map(_.filter).getOrElse(WalletCache.emptyFilter(settings))

  def removeScan(scanId: ScanId): WalletVars = {
    this.copy(externalScans = this.externalScans.filter(_.scanId != scanId))
  }

  def addScan(app: Scan): WalletVars = {
    this.copy(externalScans = this.externalScans :+ app)
  }

  /**
    * Clear the prover along with its secrets.
    *
    * Public keys and scans still live in the new instance.
    *
    * @return updated WalletVars instance
    **/
  def resetProver(): WalletVars = this.copy(proverOpt = None, stateCacheProvided = stateCacheOpt)

  def withProver(prover: ErgoProvingInterpreter): WalletVars = {
    this.copy(proverOpt = Some(prover), stateCacheProvided = None)
  }

  /**
    * Add new secret to the prover
    *
    * @param secret - secret to add to existing ones
    * @return
    */
  def withExtendedKey(secret: ExtendedSecretKey): Try[WalletVars] = Try {
    proverOpt match {
      case Some(prover) =>
        val (updProver, newPk) = prover.withNewExtendedSecret(secret)
        val updCache = stateCacheOpt.get.withNewPubkey(newPk).get
        this.copy(proverOpt = Some(updProver), stateCacheProvided = Some(updCache))
      case None =>
        log.warn(s"Trying to add new secret, but prover is not initialized")
        this
    }
  }

  /**
    * Updates parameters of the prover
    *
    * @param parameters - new of the prover parameters
    * @return
    */
  def withParameters(parameters: Parameters): Try[WalletVars] = Try {
    proverOpt match {
      case Some(prover) =>
        val updProver = prover.withNewParameters(parameters)
        this.copy(proverOpt = Some(updProver))
      case None =>
        log.warn(s"Trying to add new secret, but prover is not initialized")
        this
    }
  }

}

object WalletVars {

  def apply(storage: WalletStorage, settings: ErgoSettings): WalletVars = {
    val keysRead = storage.readAllKeys()
    val cacheOpt = if (keysRead.nonEmpty) {
      Some(WalletCache(keysRead, settings))
    } else {
      None
    }
    WalletVars(None, storage.allScans, cacheOpt)(settings)
  }

}
