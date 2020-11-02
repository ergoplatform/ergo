package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedTokenId, _}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

import scala.collection.mutable
import scorex.util.Extensions._

/**
  * Holds aggregate wallet data (including off-chain) with no need fo re-processing it on each request.
  *
  * @param height - height corresponding to the wallet state digest
  * @param walletBalance - wallet balance (in nanoErgs)
  * @param walletAssetBalances - asset balances
  */
final case class WalletDigest(height: Int,
                              walletBalance: Long,
                              walletAssetBalances: mutable.LinkedHashMap[EncodedTokenId, Long]) {

  /**
    * Generate a new wallet digest with contents of a given box included into the digest
    *
    * @param box - box to include into the digest
    * @return updated digest
    */
  def putBox(box: ErgoBox): WalletDigest = {
    val mutMap = walletAssetBalances

    box.additionalTokens.toArray.foreach { tokenRec =>
      val key = EncodedTokenId @@ scorex.util.encode.Base16.encode(tokenRec._1)
      val value = mutMap.getOrElse(key, 0L) + tokenRec._2
      mutMap.updated(key, value)
    }
    this.copy(walletBalance = this.walletBalance + box.value, walletAssetBalances = mutMap)
  }

  /**
    * Exclude box from the digest
    *
    * @param box - box to exclude from the digest
    * @return updated digest
    */
  def removeBox(box: ErgoBox): WalletDigest = {
    val mutMap = walletAssetBalances

    box.additionalTokens.toArray.foreach { tokenRec =>
      val key = EncodedTokenId @@ scorex.util.encode.Base16.encode(tokenRec._1)
      val value = mutMap.getOrElse(key, 0L) - tokenRec._2
      mutMap.updated(key, value)
    }
    this.copy(walletBalance = this.walletBalance - box.value, walletAssetBalances = mutMap)
  }

}

object WalletDigest {

  def empty: WalletDigest =
    WalletDigest(ErgoHistory.EmptyHistoryHeight, 0, mutable.LinkedHashMap.empty)

}

object WalletDigestSerializer extends ScorexSerializer[WalletDigest] {

  override def serialize(obj: WalletDigest, w: Writer): Unit = {
    w.putUInt(obj.height)
    w.putULong(obj.walletBalance)

    w.putUInt(obj.walletAssetBalances.size)
    obj.walletAssetBalances.foreach { case (id, amt) =>
      w.putBytes(decodedTokenId(id))
      w.putULong(amt)
    }
  }

  override def parse(r: Reader): WalletDigest = {
    val height = r.getUInt().toIntExact
    val balance = r.getULong()

    val walletAssetBalancesSize = r.getUInt().toIntExact

    val walletAssetBalances = mutable.LinkedHashMap.empty[EncodedTokenId, Long]
    (0 until walletAssetBalancesSize).foreach { _ =>
      val kv = encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getULong()
      walletAssetBalances += kv
    }

    WalletDigest(height, balance, walletAssetBalances)
  }

}
