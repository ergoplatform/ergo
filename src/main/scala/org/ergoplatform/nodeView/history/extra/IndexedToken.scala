package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.{ByteColl, uniqueId}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.CollectionConstant
import sigmastate.SByte
import special.collection.Extensions._

import scala.collection.mutable.ArrayBuffer

/**
  * Index of a token containing creation information.
  * @param tokenId     - id of this token
  * @param boxId       - id of the box that created th is token
  * @param amount      - emission amount
  * @param name        - name of this token (UTF-8)
  * @param description - description of this token (UTF-8)
  * @param decimals    - number of decimal places
  * @param boxes       - list of numberic box indexes, negative values indicate the box is spent
  */
case class IndexedToken(tokenId: ModifierId,
                        boxId: ModifierId = ModifierId @@ "",
                        amount: Long = 0L,
                        name: String = "",
                        description: String = "",
                        decimals: Int = 0,
                        override val boxes: ArrayBuffer[Long] = new ArrayBuffer[Long])
  extends Segment[IndexedToken](uniqueId(tokenId), id => IndexedToken(id), new ArrayBuffer[Long], boxes) with ExtraIndex {

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

    if (txCount == 0 && boxCount == 0)
      toRemove += id // all segments empty after rollback, delete parent
    else
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
    w.putBytes(fastIdToBytes(iT.boxId))
    w.putULong(iT.amount)
    val name: Array[Byte] = iT.name.getBytes("UTF-8")
    w.putUShort(name.length)
    w.putBytes(name)
    val description: Array[Byte] = iT.description.getBytes("UTF-8")
    w.putUShort(description.length)
    w.putBytes(description)
    w.putInt(iT.decimals)
    SegmentSerializer.serialize(iT, w)
  }

  override def parse(r: Reader): IndexedToken = {
    val tokenId: ModifierId = bytesToId(r.getBytes(32))
    val boxId: ModifierId = bytesToId(r.getBytes(32))
    val amount: Long = r.getULong()
    val nameLen: Int = r.getUShort()
    val name: String = new String(r.getBytes(nameLen), "UTF-8")
    val descLen: Int = r.getUShort()
    val description: String = new String(r.getBytes(descLen), "UTF-8")
    val decimals: Int = r.getInt()
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
    * @param tokenIndex - token index to check in [[ErgoBox.additionalTokens]]
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
                 iEb.id,
                 iEb.box.additionalTokens(tokenIndex)._2,
                 name,
                 description,
                 decimals)
      .addBox(iEb)
  }
}
