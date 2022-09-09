package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import spire.implicits.cfor

import java.nio.ByteBuffer
import scala.collection.mutable

class BalanceInfo(var nanoErgs: Long,
                  val tokens: mutable.HashMap[TokenId,Long])

object BalanceInfoSerializer extends ScorexSerializer[BalanceInfo] {

  override def serialize(bI: BalanceInfo, w: Writer): Unit = {
    w.putLong(bI.nanoErgs)
    w.putInt(bI.tokens.size)
    val b: ByteBuffer = ByteBuffer.allocate(8)
    val arr: Array[(TokenId,Long)] = bI.tokens.toArray
    cfor(0)(_ < arr.length, _ + 1) { i =>
      w.putBytes(arr(i)._1 ++ b.putLong(arr(i)._2).array)
      b.clear()
    }
  }

  override def parse(r: Reader): BalanceInfo = {
    val nanoErgs: Long = r.getLong()
    val tokensLen: Int = r.getInt()
    val tokens: mutable.HashMap[TokenId,Long] = mutable.HashMap.empty[TokenId,Long]
    val b: ByteBuffer = ByteBuffer.allocate(8)
    cfor(0)(_ < tokensLen, _ + 1) { _ =>
      tokens.put(Digest32 @@ r.getBytes(32), b.put(r.getBytes(8)).getLong)
      b.clear()
    }
    new BalanceInfo(nanoErgs, tokens)
  }

}

object BalanceInfo {
  def fromBox(box: ErgoBox): BalanceInfo = {
    new BalanceInfo(box.value, mutable.HashMap.empty[TokenId,Long] ++= box.additionalTokens.toMap)
  }
}
