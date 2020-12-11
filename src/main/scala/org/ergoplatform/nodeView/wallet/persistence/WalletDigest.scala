package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils._
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
                              walletAssetBalances: Seq[(EncodedTokenId, Long)])

object WalletDigest {

  val empty: WalletDigest =
    WalletDigest(ErgoHistory.EmptyHistoryHeight, 0, mutable.WrappedArray.empty)

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

    WalletDigest(height, balance, walletAssetBalances.toSeq)
  }

}
