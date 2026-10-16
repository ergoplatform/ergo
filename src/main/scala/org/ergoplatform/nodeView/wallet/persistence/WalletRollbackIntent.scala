package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

/** Durable evidence that the wallet registry must reach one exact rollback target. */
final case class WalletRollbackIntent(targetVersionId: ModifierId, expectedHeight: Int)

object WalletRollbackIntentSerializer extends ErgoSerializer[WalletRollbackIntent] {
  private val FormatPrefix: Array[Byte] = Array(
    0x80.toByte, 0x00.toByte, 0x57.toByte, 0x52.toByte,
    0x49.toByte, 0x01.toByte)

  override def serialize(intent: WalletRollbackIntent, w: Writer): Unit = {
    w.putBytes(FormatPrefix)
    w.putInt(intent.expectedHeight)
    w.putBytes(idToBytes(intent.targetVersionId))
  }

  override def parse(r: Reader): WalletRollbackIntent = {
    val prefix = r.getBytes(FormatPrefix.length)
    require(java.util.Arrays.equals(prefix, FormatPrefix),
      "Unsupported wallet rollback intent format")
    val expectedHeight = r.getInt()
    WalletRollbackIntent(
      targetVersionId = bytesToId(r.getBytes(Constants.ModifierIdSize)),
      expectedHeight = expectedHeight)
  }
}
