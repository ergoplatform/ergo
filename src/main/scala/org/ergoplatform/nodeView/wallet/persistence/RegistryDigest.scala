package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

final case class RegistryDigest(height: Int,
                                walletBalance: Long,
                                walletAssetBalances: Map[EncodedTokenId, Long])
object RegistryDigest {

  def empty: RegistryDigest =
    RegistryDigest(ErgoHistory.EmptyHistoryHeight, 0, Map.empty)

}

object RegistryDigestSerializer extends ScorexSerializer[RegistryDigest] {

  override def serialize(obj: RegistryDigest, w: Writer): Unit = {
    w.putInt(obj.height)
    w.putLong(obj.walletBalance)

    w.putInt(obj.walletAssetBalances.size)
    obj.walletAssetBalances.foreach { case (id, amt) =>
      w.putBytes(decodedTokenId(id))
      w.putLong(amt)
    }
  }

  override def parse(r: Reader): RegistryDigest = {
    val height = r.getInt()
    val balance = r.getLong()

    val walletAssetBalancesSize = r.getInt()
    val walletAssetBalances = (0 until walletAssetBalancesSize).map { _ =>
      encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getLong()
    }.toMap

    RegistryDigest(height, balance, walletAssetBalances)
  }

}
