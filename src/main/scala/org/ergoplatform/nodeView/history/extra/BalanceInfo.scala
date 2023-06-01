package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.fastIdToBytes
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import scorex.core.serialization.ErgoSerializer
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import spire.implicits.cfor
import special.collection.Extensions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Class that tracks the ERG and token balances in [[IndexedErgoAddress]]
  */
case class BalanceInfo() extends ScorexLogging {

  var nanoErgs: Long = 0L
  val tokens: ArrayBuffer[(ModifierId,Long)] = ArrayBuffer.empty[(ModifierId,Long)]

  val additionalTokenInfo: mutable.HashMap[ModifierId,(String,Int)] = mutable.HashMap.empty[ModifierId,(String,Int)]

  /**
    * Get full token information from database.
    * @param history - database handle
    * @return this object
    */
  def retreiveAdditionalTokenInfo(history: ErgoHistoryReader): BalanceInfo = {
    additionalTokenInfo ++= tokens.map(token => {
      history.typedExtraIndexById[IndexedToken](uniqueId(token._1)) match {
        case Some(iT) => (token._1,(iT.name,iT.decimals))
        case None => (token._1,("", 0))
      }
    })
    this
  }

  /**
    * Find index of token with given id in tokens
    * @param id - id to look for
    * @return index of id, if any
    */
  private def index(id: ModifierId): Option[Int] = {
    cfor(0)(_ < tokens.length, _ + 1) { i =>
      if(tokens(i)._1 == id) return Some(i)
    }
    None
  }

  /**
    * Record an address receiving a box.
    * @param box - box received
    */
  def add(box: ErgoBox): Unit = {
    nanoErgs += box.value
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val id: ModifierId = box.additionalTokens(i)._1.toModifierId
      index(id) match {
        case Some(n) => tokens(n) = Tuple2(id, tokens(n)._2 + box.additionalTokens(i)._2)
        case None    => tokens += Tuple2(id, box.additionalTokens(i)._2)
      }
    }
  }

  /**
    * Record an address spending a box.
    * @param box - box spent
    * @param ae - address encoder to use in case of an error message
    */
  def subtract(box: ErgoBox)(implicit ae: ErgoAddressEncoder): Unit = {
    nanoErgs = math.max(nanoErgs - box.value, 0)
    cfor(0)(_ < box.additionalTokens.length, _ + 1) { i =>
      val id: ModifierId = box.additionalTokens(i)._1.toModifierId
      index(id) match {
        case Some(n) =>
          val newVal: Long = tokens(n)._2 - box.additionalTokens(i)._2
          if (newVal == 0)
            tokens.remove(n)
          else
            tokens(n) = (id, newVal)
        case None => log.warn(s"Failed to subtract token $id from address ${ae.fromProposition(box.ergoTree).map(ae.toString).getOrElse(box.ergoTree.bytesHex)}")
      }

    }
  }

}

object BalanceInfoSerializer extends ErgoSerializer[BalanceInfo] {

  override def serialize(bI: BalanceInfo, w: Writer): Unit = {
    w.putLong(bI.nanoErgs)
    w.putInt(bI.tokens.length)
    cfor(0)(_ < bI.tokens.length, _ + 1) { i =>
      w.putBytes(fastIdToBytes(bI.tokens(i)._1))
      w.putLong(bI.tokens(i)._2)
    }
  }

  override def parse(r: Reader): BalanceInfo = {
    val bI: BalanceInfo = BalanceInfo()
    bI.nanoErgs = r.getLong()
    val tokensLen: Int = r.getInt()
    cfor(0)(_ < tokensLen, _ + 1) { _ =>
      bI.tokens += Tuple2(bytesToId(r.getBytes(32)), r.getLong())
    }
    bI
  }

}
