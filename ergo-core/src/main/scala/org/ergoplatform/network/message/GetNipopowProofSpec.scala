package org.ergoplatform.network.message

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.sdk.wallet.Constants.ModifierIdLength
import org.ergoplatform.settings.Algos
import scorex.util.ModifierId
import scorex.util.serialization.{Reader, Writer}

/**
 * The `GetNipopowProof` message requests a `NipopowProof` message from the receiving node
 */
object GetNipopowProofSpec extends MessageSpecV1[NipopowProofData] {

  val SizeLimit = 1000

  val messageCode: MessageCode = 90: Byte
  val messageName: String = "GetNipopowProof"

  override def serialize(data: NipopowProofData, w: Writer): Unit = {
    w.putInt(data.m)
    w.putInt(data.k)
    data.headerIdBytesOpt match {
      case Some(idBytes) =>
        w.put(1)
        w.putBytes(idBytes)
      case None =>
        w.put(0)
    }
    w.putUShort(data.pad.length) // length of additional data, which a reader that does not know it skips
    w.putBytes(data.pad)
  }

  override def parse(r: Reader): NipopowProofData = {
    require(r.remaining <= SizeLimit, s"Too big GetNipopowProofSpec message(size: ${r.remaining})")

    val m = r.getInt()
    val k = r.getInt()

    val headerIdPresents = r.getByte() == 1
    val headerIdOpt = if (headerIdPresents) {
      Some(ModifierId @@ Algos.encode(r.getBytes(ModifierIdLength)))
    } else {
      None
    }
    val remainingBytes = r.getUShort()
    val pad = if (remainingBytes > 0 && remainingBytes < SizeLimit) {
      r.getBytes(remainingBytes)
    } else {
      Array.emptyByteArray
    }
    NipopowProofData(m, k, headerIdOpt, pad)
  }
}
