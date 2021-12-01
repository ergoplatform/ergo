package org.ergoplatform.nodeView.wallet

import com.google.common.hash.{BloomFilter, Funnels}
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
                             filter: BloomFilter[Array[Byte]])(implicit val settings: ErgoSettings) {

  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  // Mining rewards are being tracked only if mining = true in config
  private val miningScripts: Seq[Values.ErgoTree] = {
    if (settings.nodeSettings.mining) {
      WalletCache.miningScripts(trackedPubKeys, settings)
    } else {
      Seq.empty
    }
  }

  val miningScriptsBytes: Seq[Array[Byte]] = miningScripts.map(_.bytes)

  def withNewPubkey(newPk: ExtendedPublicKey): Try[WalletCache] = Try {
    val updAddresses: Seq[P2PKAddress] = publicKeyAddresses :+ P2PKAddress(newPk.key)
    val updTrackedPubKeys: Seq[ExtendedPublicKey] = trackedPubKeys :+ newPk
    val newPkBytes = newPk.key.propBytes.toArray
    val updTrackedBytes: Seq[Array[Byte]] = trackedBytes :+ newPkBytes

    // update filter
    filter.put(newPkBytes)

    WalletCache(updAddresses, updTrackedPubKeys, updTrackedBytes, filter)
  }

}

object WalletCache {

  def miningScripts(trackedPubKeys: Seq[ExtendedPublicKey],
                    settings: ErgoSettings): Seq[Values.ErgoTree] = {
    trackedPubKeys.map { pk =>
      ErgoScriptPredef.rewardOutputScript(settings.miningRewardDelay, pk.key)
    }
  }

  /**
    * Create empty bloom filter
    * @return a Bloom filter instance
    */
  def emptyFilter(expectedKeys: Int = 100000): BloomFilter[Array[Byte]] = {
    val falsePositiveRate = 0.01
    BloomFilter.create[Array[Byte]](Funnels.byteArrayFunnel(), expectedKeys, falsePositiveRate)
  }

  /**
    * Constructing a Bloom filter for scanning the boxes efficiently
    */
  def createFilter(trackedBytes: Seq[Array[Byte]],
                   miningScriptsBytes: Seq[Array[Byte]]): BloomFilter[Array[Byte]] = {
    val f = emptyFilter()
    trackedBytes.foreach(bs => f.put(bs))
    miningScriptsBytes.foreach(msb => f.put(msb))
    f
  }

  private def trackedBytes(trackedPubKeys: Seq[ExtendedPublicKey]): Seq[Array[Byte]] =
    trackedPubKeys.map(_.key.propBytes.toArray)

  private def publicKeyAddresses(trackedPubKeys: Seq[ExtendedPublicKey],
                                 addressEncoder: ErgoAddressEncoder): Seq[P2PKAddress] = {
    // Remove master key from visible addresses if following-up keys are from EIP-3 range
    val pks = if (trackedPubKeys.size > 1
      && trackedPubKeys.head.path.isMaster
      && trackedPubKeys(1).path.isEip3) {
      // Skip master key
      trackedPubKeys.tail
    } else {
      trackedPubKeys
    }
    pks.map(pk => P2PKAddress(pk.key)(addressEncoder))
  }

  def apply(trackedPubKeys: Seq[ExtendedPublicKey], settings: ErgoSettings): WalletCache = {
    val tbs = trackedBytes(trackedPubKeys)
    val msBytes = miningScripts(trackedPubKeys, settings).map(_.bytes)
    val f = createFilter(tbs, msBytes)
    val pka = publicKeyAddresses(trackedPubKeys, settings.addressEncoder)

    WalletCache(pka, trackedPubKeys, tbs, f)(settings)
  }

}
