package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import spire.implicits.cfor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class BalanceInfo(var nanoErgs: Long = 0L,
                  val tokens: ArrayBuffer[(ModifierId,Long)] = ArrayBuffer.empty[(ModifierId,Long)]) {

  val additionalTokenInfo: mutable.HashMap[ModifierId,(String,Int)] = mutable.HashMap.empty[ModifierId,(String,Int)]

  def retreiveAdditionalTokenInfo(history: ErgoHistoryReader): BalanceInfo = {
    additionalTokenInfo ++= tokens.map(token => {
      val iT: IndexedToken = history.typedModifierById[IndexedToken](IndexedTokenSerializer.uniqueId(token._1)).get
      (token._1,(iT.name,iT.decimals))
    })
    this
  }

  private def index(id: ModifierId): Int = {
    cfor(0)(_ < tokens.length, _ + 1) { i =>
      if(tokens(i)._1 == id) return i
    }
    -1
  }

  def add(box: ErgoBox): Unit = {
    nanoErgs += box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val id: ModifierId = bytesToId(box.additionalTokens(i)._1)
      val n: Int = index(id)
      if(n >= 0)
        tokens(n) = Tuple2(id, tokens(n)._2 + box.additionalTokens(i)._2)
      else
        tokens += Tuple2(id, box.additionalTokens(i)._2)
    }
  }

  def subtract(box: ErgoBox): Unit = {
    nanoErgs -= box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val id: ModifierId = bytesToId(box.additionalTokens(i)._1)
      val n: Int = index(id)
      val newVal: Long = tokens(n)._2 - box.additionalTokens(i)._2
      if(newVal == 0)
        tokens.remove(n)
      else
        tokens(n) = (id, newVal)
    }
  }

}

object BalanceInfoSerializer extends ScorexSerializer[BalanceInfo] {

  override def serialize(bI: BalanceInfo, w: Writer): Unit = {
    w.putLong(bI.nanoErgs)
    w.putInt(bI.tokens.length)
    cfor(0)(_ < bI.tokens.length, _ + 1) { i =>
      w.putBytes(fastIdToBytes(bI.tokens(i)._1))
      w.putLong(bI.tokens(i)._2)
    }
  }

  override def parse(r: Reader): BalanceInfo = {
    val bI: BalanceInfo = new BalanceInfo(r.getLong())
    val tokensLen: Int = r.getInt()
    cfor(0)(_ < tokensLen, _ + 1) { _ =>
      bI.tokens += Tuple2(bytesToId(r.getBytes(32)), r.getLong())
    }
    bI
  }

}
