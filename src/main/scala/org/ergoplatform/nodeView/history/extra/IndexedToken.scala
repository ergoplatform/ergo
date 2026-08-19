package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.{ByteColl, uniqueId}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Algos
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ByteArrayOps, ModifierId, bytesToId}
import sigma.Extensions._
import sigma.ast.SByte
import sigma.ast.syntax.CollectionConstant

import scala.collection.mutable.ArrayBuffer

/**
  * Index of a token containing creation information.
  * @param tokenId     - id of this token
  * @param boxId       - id of the box that created this token
  * @param amount      - emission amount
  * @param name        - name of this token (UTF-8)
  * @param description - description of this token (UTF-8)
  * @param decimals    - number of decimal places
  * @param boxes       - list of numberic box indexes, negative values indicate the box is spent
  */
case class IndexedToken(tokenId: ModifierId,
                        boxId: Option[ModifierId] = None,
                        amount: Option[Long] = None,
                        name: Option[String] = None,
                        description: Option[String] = None,
                        decimals: Option[Int] = None,
                        override val boxes: ArrayBuffer[Long] = new ArrayBuffer[Long])
  extends Segment[IndexedToken](id => IndexedToken(id), new ArrayBuffer[Long], boxes) with ExtraIndex {

  override lazy val id: ModifierId = uniqueId(tokenId)
  override def serializedId: Array[Byte] = fastIdToBytes(id)

  /**
   * Rollback the state of segments in memory and in db
   *
   * @param txTarget  - remove transaction numbers above this number
   * @param boxTarget - remove box numbers above this number
   * @param history   - history handle to update segment(s) in database
   * @return modifier ids to remove
   */
  override private[extra] def rollback(txTarget: Long, boxTarget: Long, history: ErgoHistory)(implicit segmentTreshold: Int): Array[ModifierId] = {

    val toRemove: ArrayBuffer[ModifierId] = rollbackState(txTarget, boxTarget, history.getReader)

    if (boxCount == 0) {
      toRemove += id // all segments empty after rollback, delete parent
      log.info(s"Removing token $tokenId because no more boxes are associated with it")
    } else
      history.historyStorage.insertExtra(Array.empty, Array(this)) // save the changes made to this address

    toRemove.toArray
  }

  /**
   * Add transaction index
   *
   * @param tx - numeric transaction index
   * @return this
   */
  @deprecated("Indexed tokens do not track transactions", "")
  override private[extra] def addTx(tx: Long): IndexedToken = this

  /**
   * Add box index
   *
   * @param iEb    - box to use
   * @param record - whether to add box to boxes list, used in rollbacks (true by default)
   * @return this
   */
  override private[extra] def addBox(iEb: IndexedErgoBox, record: Boolean = true): IndexedToken = {
    if(record) boxes += iEb.globalIndex
    this
  }

  /**
   * Update segments in memory or in database by spending a box
   *
   * @param iEb        - box to spend
   * @param historyOpt - history handle to update segment in db if spent box is old
   * @return this
   */
  override private[extra] def spendBox(iEb: IndexedErgoBox, historyOpt: Option[ErgoHistoryReader])(implicit ae: ErgoAddressEncoder): IndexedToken = {
    if(historyOpt.isDefined)
      findAndModBox(iEb.globalIndex, historyOpt.get)
    this
  }

  /**
   * Filter mempool boxes if API call requires it
   *
   * @param boxes - all boxes in mempool
   * @return associated boxes
   */
  override private[extra] def filterMempool(boxes: Seq[ErgoBox]): Seq[ErgoBox] =
    boxes.filter(_.additionalTokens.exists(_._1.toModifierId == tokenId))

  /**
   * Increase emission amount of this token. Sometimes tokens get created in multiple boxes.
   * @param plus - emission amount to add
   * @return updated token
   */
  private[extra] def addEmissionAmount(plus: Long): IndexedToken = {
    val updated = IndexedToken(tokenId, boxId, Some(amount.getOrElse(0L) + plus), name, description, decimals, boxes)
    updated.buffer ++= buffer
    updated
  }

}

object IndexedTokenSerializer extends ErgoSerializer[IndexedToken] {

  type ByteColl = CollectionConstant[SByte.type]

  /**
    * Calculate a unique identifier for this a token.
    * Necessary, because token ids are sometimes identical to box ids, which causes overwrites.
    * @param tokenId - id of the token
    * @return unique id for token
    */
  def uniqueId(tokenId: ModifierId): ModifierId = bytesToId(Algos.hash(tokenId + "token"))

  override def serialize(iT: IndexedToken, w: Writer): Unit = {
    w.putBytes(fastIdToBytes(iT.tokenId))
    w.putOption[ModifierId](iT.boxId)((ww,boxId) => ww.putBytes(fastIdToBytes(boxId)))
    w.putOption[Long](iT.amount)((ww, amount) => ww.putULong(amount))
    w.putOption[String](iT.name) { case (ww, name) =>
      val bytes = name.getBytes
      ww.putUShort(bytes.length)
      ww.putBytes(bytes)
    }
    w.putOption[String](iT.description) { case (ww, description) =>
      val bytes = description.getBytes
      ww.putUShort(bytes.length)
      ww.putBytes(bytes)
    }
    w.putOption[Int](iT.decimals)((ww, decimals) => ww.putInt(decimals))
    SegmentSerializer.serialize(iT, w)
  }

  override def parse(r: Reader): IndexedToken = {
    val tokenId: ModifierId = bytesToId(r.getBytes(32))
    val boxId: Option[ModifierId] = r.getOption[ModifierId](r.getBytes(32).toModifierId)
    val amount: Option[Long] = r.getOption[Long](r.getULong())
    val name: Option[String] = r.getOption[String] {
      val len = r.getUShort()
      new String(r.getBytes(len))
    }
    val description: Option[String] = r.getOption[String] {
      val len = r.getUShort()
      new String(r.getBytes(len))
    }
    val decimals: Option[Int] = r.getOption[Int](r.getInt())
    val iT = IndexedToken(tokenId, boxId, amount, name, description, decimals)
    SegmentSerializer.parse(r, iT)
    iT
  }
}

object IndexedToken {

  val extraIndexTypeId: ExtraIndexTypeId = 35.toByte

  /**
    * Construct an indexed token from a box.
    * Tokens can be created without setting registers or something other than token information can be in them,
    * so they are checked with Try-catches.
    *
    * @param iEb - box to use
    * @param tokenIndex - token index to check in box `additionalTokens`
    * @return token index
    */
  def fromBox(iEb: IndexedErgoBox, tokenIndex: Int): IndexedToken = {
    val name: String =
      iEb.box.additionalRegisters.get(R4) match {
        case Some(reg) =>
          try {
            new String(reg.asInstanceOf[ByteColl].value.toArray, "UTF-8")
          } catch {
            case _: Throwable => ""
          }
        case None => ""
      }

    val description: String =
      iEb.box.additionalRegisters.get(R5) match {
        case Some(reg) =>
          try {
            new String(reg.asInstanceOf[ByteColl].value.toArray, "UTF-8")
          } catch {
            case _: Throwable => ""
          }
        case None => ""
      }


    val decimals: Int =
      iEb.box.additionalRegisters.get(R6) match {
        case Some(reg) =>
          try {
            new String(reg.asInstanceOf[ByteColl].value.toArray, "UTF-8").toInt
          } catch {
            case _: Throwable =>
              try{
                reg.value.asInstanceOf[Int]
              }catch {
                case _: Throwable => 0
              }
          }
        case None => 0
      }

    IndexedToken(iEb.box.additionalTokens(tokenIndex)._1.toModifierId,
                 Some(iEb.id),
                 Some(iEb.box.additionalTokens(tokenIndex)._2),
                 Some(name),
                 Some(description),
                 Some(decimals))
  }
}
