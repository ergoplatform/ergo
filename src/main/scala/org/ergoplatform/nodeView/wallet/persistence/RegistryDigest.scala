package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}

final case class RegistryDigest(height: Int,
                                walletBalance: Long,
                                walletAssetBalances: Map[EncodedTokenId, Long],
                                appBalances: Map[AppId, Long],
                                appAssetBalances: Map[AppId, Map[EncodedTokenId, Long]],
                                uncertainAppBoxes: Map[AppId, Seq[EncodedBoxId]])
object RegistryDigest {

  def empty: RegistryDigest =
    RegistryDigest(ErgoHistory.EmptyHistoryHeight, 0, Map.empty, Map.empty, Map.empty, Map.empty)

}

object RegistrySummarySerializer extends ScorexSerializer[RegistryDigest] {

  override def serialize(obj: RegistryDigest, w: Writer): Unit = {
    w.putInt(obj.height)
    w.putLong(obj.walletBalance)

    w.putInt(obj.walletAssetBalances.size)
    obj.walletAssetBalances.foreach { case (id, amt) =>
      w.putBytes(decodedTokenId(id))
      w.putLong(amt)
    }

    w.putInt(obj.appBalances.size)
    obj.appBalances.foreach{ case (appId, balance) =>
      w.putShort(appId)
      w.putLong(balance)
    }

    w.putInt(obj.appAssetBalances.size)
    obj.appAssetBalances.foreach{ case (appId, assetsMap) =>
      w.putShort(appId)
      w.putInt(assetsMap.size)
      assetsMap.foreach { case (id, amt) =>
        w.putBytes(decodedTokenId(id))
        w.putLong(amt)
      }
    }

    w.putInt(obj.uncertainAppBoxes.size)
    obj.uncertainAppBoxes.foreach{ case (appId, boxIds) =>
      w.putShort(appId)
      w.putInt(boxIds.size)
      boxIds.foreach(x => w.putBytes(decodedBoxId(x)))
    }
  }

  override def parse(r: Reader): RegistryDigest = {
    val height = r.getInt()
    val balance = r.getLong()

    val walletAssetBalancesSize = r.getInt()
    val walletAssetBalances = (0 until walletAssetBalancesSize).map { _ =>
      encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getLong()
    }

    val appBalancesSize = r.getInt()
    val appBalances = (0 until appBalancesSize).map { _ =>
      r.getShort() -> r.getLong()
    }

    val appAssetsSize = r.getInt()
    val appAssets = (0 until appAssetsSize).map {_ =>
      val appId = r.getShort()
      val assetsQty = r.getInt()
      val assets = (0 until assetsQty).map{_ =>
        encodedTokenId(Digest32 @@ r.getBytes(Constants.ModifierIdSize)) -> r.getLong()
      }
      appId -> assets.toMap
    }

    val uncertainAppBoxesQty = r.getInt()
    val uncertainIds = (0 until uncertainAppBoxesQty).map { _ =>
      val appId = r.getShort()
      val boxesQty = r.getInt()

      val boxIds = (0 until boxesQty).map { _ =>
        encodedBoxId(ADKey @@ r.getBytes(Constants.ModifierIdSize))
      }
      appId -> boxIds
    }

    RegistryDigest(height, balance, walletAssetBalances.toMap, appBalances.toMap, appAssets.toMap, uncertainIds.toMap)
  }

}
