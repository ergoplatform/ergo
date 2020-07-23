package org.ergoplatform.nodeView.wallet

import com.github.oskin1.scakoo.immutable.CuckooFilter
import org.ergoplatform.{ErgoAddressEncoder, ErgoScriptPredef, P2PKAddress}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.secrets.ExtendedPublicKey
import sigmastate.Values
import sigmastate.eval._
import scala.util.Try

/**
  * Fields of WalletVars which are potentially costly to compute if there are many keys in the wallet
  */
final case class WalletCache(publicKeyAddresses: Seq[P2PKAddress],
                             trackedPubKeys: Seq[ExtendedPublicKey],
                             trackedBytes: Seq[Array[Byte]],
                             filter: CuckooFilter[Array[Byte]])(implicit val settings: ErgoSettings) {

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  val miningScripts: Seq[Values.ErgoTree] = WalletCache.miningScripts(trackedPubKeys, settings)

  val miningScriptsBytes: Seq[Array[Byte]] = miningScripts.map(_.bytes)

  def withNewPubkey(newPk: ExtendedPublicKey): Try[WalletCache] = Try {
    val updAddresses: Seq[P2PKAddress] = publicKeyAddresses :+ P2PKAddress(newPk.key)
    val updTrackedPubKeys: Seq[ExtendedPublicKey] = trackedPubKeys :+ newPk
    val newPkBytes = newPk.key.propBytes.toArray
    val updTrackedBytes: Seq[Array[Byte]] = trackedBytes :+ newPkBytes
    val updFilter: CuckooFilter[Array[Byte]] = filter.insert(newPkBytes).get

    WalletCache(updAddresses, updTrackedPubKeys, updTrackedBytes, updFilter)
  }

}

object WalletCache {
  //strategy for Cuckoo filter
  import com.github.oskin1.scakoo.TaggingStrategy.MurmurHash3Strategy

  // currently only one mining key supported
  def miningScripts(trackedPubKeys: Seq[ExtendedPublicKey],
                    settings: ErgoSettings): Seq[Values.ErgoTree] = {
    trackedPubKeys.headOption.map { pk =>
      ErgoScriptPredef.rewardOutputScript(settings.miningRewardDelay, pk.key)
    }.toSeq
  }

  def emptyFilter(settings: ErgoSettings): com.github.oskin1.scakoo.mutable.CuckooFilter[Array[Byte]] = {
    val entriesPerBucket = settings.walletSettings.keysFilter.entriesPerBucket
    val bucketsQty = settings.walletSettings.keysFilter.bucketsQty
    com.github.oskin1.scakoo.mutable.CuckooFilter[Array[Byte]](entriesPerBucket, bucketsQty)
  }

  /**
    * Construction a Cuckoo filter for scanning the boxes efficiently
    */
  def filter(trackedBytes: Seq[Array[Byte]],
             miningScriptsBytes: Seq[Array[Byte]],
             settings: ErgoSettings): CuckooFilter[Array[Byte]] = {
    val f = emptyFilter(settings)
    trackedBytes.foreach(bs => f.insert(bs))
    miningScriptsBytes.foreach(msb => f.insert(msb))
    CuckooFilter.recover(f.memTable, f.entriesCount, f.entriesPerBucket)
  }

  def trackedBytes(trackedPubKeys: Seq[ExtendedPublicKey]): Seq[Array[Byte]] =
    trackedPubKeys.map(_.key.propBytes.toArray)

  def trackedAddresses(trackedPubKeys: Seq[ExtendedPublicKey],
                       addressEncoder: ErgoAddressEncoder): Seq[P2PKAddress] =
    trackedPubKeys.map(pk => P2PKAddress(pk.key)(addressEncoder))

  def apply(trackedPubKeys: Seq[ExtendedPublicKey], settings: ErgoSettings): WalletCache = {
    val tbs = trackedBytes(trackedPubKeys)
    val msBytes = miningScripts(trackedPubKeys, settings).map(_.bytes)
    val filter = filter(tbs, msBytes, settings)
    val tas = trackedAddresses(trackedPubKeys, settings.addressEncoder)

    WalletCache(tas, trackedPubKeys, tbs, filter)(settings)
  }

}
