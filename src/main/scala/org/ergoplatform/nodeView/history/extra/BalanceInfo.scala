package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.hash.Digest32
import scorex.util.bytesToId
import scorex.util.serialization.{Reader, Writer}
import spire.implicits.cfor

import scala.collection.mutable
import scala.collection.JavaConverters.mapAsScalaMap

class BalanceInfo(var nanoErgs: Long,
                  val tokens: mutable.Map[TokenId,Long]) {

  val additionalTokenInfo: mutable.Map[TokenId,(String,Int)] = mutable.Map.empty[TokenId,(String,Int)]

  def retreiveAdditionalTokenInfo(history: ErgoHistoryReader): BalanceInfo = {
    additionalTokenInfo ++= tokens.map(token => {
      val iT: IndexedToken = history.typedModifierById[IndexedToken](IndexedTokenSerializer.uniqueId(bytesToId(token._1))).get
      (token._1,(iT.name,iT.decimals))
    })
    this
  }

  def add(box: ErgoBox): BalanceInfo = {
    nanoErgs += box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      tokens.put(box.additionalTokens(i)._1, tokens.getOrElse(box.additionalTokens(i)._1, 0L) + box.additionalTokens(i)._2)
    }
    this
  }

  def subtract(box: ErgoBox): BalanceInfo = {
    nanoErgs -= box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val newVal: Long = tokens.getOrElse(box.additionalTokens(i)._1, 0L) - box.additionalTokens(i)._2
      if(newVal == 0)
        tokens.remove(box.additionalTokens(i)._1)
      else
        tokens.put(box.additionalTokens(i)._1, newVal)
    }
    this
  }

}

object BalanceInfoSerializer extends ScorexSerializer[BalanceInfo] {

  override def serialize(bI: BalanceInfo, w: Writer): Unit = {
    w.putLong(bI.nanoErgs)
    w.putInt(bI.tokens.size)
    val arr: Array[(TokenId,Long)] = bI.tokens.toArray
    cfor(0)(_ < arr.length, _ + 1) { i =>
      w.putBytes(arr(i)._1)
      w.putLong(arr(i)._2)
    }
  }

  override def parse(r: Reader): BalanceInfo = {
    val nanoErgs: Long = r.getLong()
    val tokensLen: Int = r.getInt()
    val tokens: java.util.HashMap[TokenId,Long] = new java.util.HashMap[TokenId,Long]
    cfor(0)(_ < tokensLen, _ + 1) { _ =>
      tokens.put(Digest32 @@ r.getBytes(32), r.getLong())
    }
    new BalanceInfo(nanoErgs, mapAsScalaMap(tokens))
  }

}

object BalanceInfo {

  def empty: BalanceInfo = new BalanceInfo(0, mutable.Map.empty[TokenId,Long])

}
