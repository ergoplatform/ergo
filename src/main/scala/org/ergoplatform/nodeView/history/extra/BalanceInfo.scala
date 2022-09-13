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

class BalanceInfo(var nanoErgs: Long = 0L,
                  val tokens: mutable.HashMap[TokenId,Long] = mutable.HashMap.empty[TokenId,Long]) {

  val additionalTokenInfo: mutable.HashMap[TokenId,(String,Int)] = mutable.HashMap.empty[TokenId,(String,Int)]

  def retreiveAdditionalTokenInfo(history: ErgoHistoryReader): BalanceInfo = {
    additionalTokenInfo ++= tokens.map(token => {
      val iT: IndexedToken = history.typedModifierById[IndexedToken](IndexedTokenSerializer.uniqueId(bytesToId(token._1))).get
      (token._1,(iT.name,iT.decimals))
    })
    this
  }

  def add(box: ErgoBox): Unit = {
    nanoErgs += box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      tokens.put(box.additionalTokens(i)._1, tokens.getOrElse(box.additionalTokens(i)._1, 0L) + box.additionalTokens(i)._2)
    }
  }

  def subtract(box: ErgoBox): Unit = {
    nanoErgs -= box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val newVal: Long = tokens(box.additionalTokens(i)._1) - box.additionalTokens(i)._2
      if(newVal == 0)
        tokens.remove(box.additionalTokens(i)._1)
      else
        tokens.put(box.additionalTokens(i)._1, newVal)
    }
  }

}

object BalanceInfoSerializer extends ScorexSerializer[BalanceInfo] {

  override def serialize(bI: BalanceInfo, w: Writer): Unit = {
    w.putLong(bI.nanoErgs)
    w.putInt(bI.tokens.size)
    bI.tokens.foreach { case (id,amount) =>
      w.putBytes(id)
      w.putLong(amount)
    }
  }

  override def parse(r: Reader): BalanceInfo = {
    val bI: BalanceInfo = new BalanceInfo(r.getLong())
    val tokensLen: Int = r.getInt()
    cfor(0)(_ < tokensLen, _ + 1) { _ =>
      bI.tokens.put(Digest32 @@ r.getBytes(32), r.getLong())
    }
    bI
  }

}
