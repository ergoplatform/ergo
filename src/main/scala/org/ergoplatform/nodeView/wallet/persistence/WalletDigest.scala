package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

/**
  * Holds aggregate wallet data (including off-chain) with no need fo re-processing it on each request.
  *
  * @param height - height corresponding to the wallet state digest
  * @param walletBalance - wallet balance (in nanoErgs)
  * @param walletAssetBalances - asset balances
  */
final case class WalletDigest(height: Int,
                              walletBalance: Long,
                              walletAssetBalances: Map[EncodedTokenId, Long])
object WalletDigest {

  def empty: WalletDigest =
    WalletDigest(ErgoHistory.EmptyHistoryHeight, 0, Map.empty)

}

object WalletDigestSerializer extends ScorexSerializer[WalletDigest] {

  override def serialize(obj: WalletDigest, w: Writer): Unit = {
    w.putInt(obj.height)
    w.putLong(obj.walletBalance)

    w.putInt(obj.walletAssetBalances.size)
    obj.walletAssetBalances.foreach { case (id, amt) =>
      w.putBytes(decodedTokenId(id))
      w.putLong(amt)
    }
  }

  override def parse(r: Reader): WalletDigest = {
    val height = r.getInt()
    val balance = r.getLong()

    val walletAssetBalancesSize = r.getInt()
    val walletAssetBalances = (0 until walletAssetBalancesSize).map { _ =>
      encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getLong()
    }.toMap

    WalletDigest(height, balance, walletAssetBalances)
  }

}
