package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.{ByteColl, getDecimals, uniqueId}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.{CollectionConstant, EvaluatedValue}
import sigmastate.{SByte, SType}

/**
  * Index of a token containing creation information.
  * @param tokenId     - id of this token
  * @param boxId       - id of the box that created th is token
  * @param amount      - emission amount
  * @param name        - name of this token (UTF-8)
  * @param description - description of this token (UTF-8)
  * @param decimals    - number of decimal places
  */
case class IndexedToken(tokenId: ModifierId,
                        boxId: ModifierId,
                        amount: Long,
                        name: String,
                        description: String,
                        decimals: Int) extends ExtraIndex {

  override lazy val id: ModifierId = uniqueId(tokenId)
  override def serializedId: Array[Byte] = fastIdToBytes(id)

}

object IndexedTokenSerializer extends ScorexSerializer[IndexedToken] {

  type ByteColl = CollectionConstant[SByte.type]

  /**
    * Calculate a unique identifier for this a token.
    * Necessary, because token ids are sometimes identical to box ids, which causes overwrites.
    * @param tokenId - id of the token
    * @return unique id for token
    */
  def uniqueId(tokenId: ModifierId): ModifierId = bytesToId(Algos.hash(tokenId + "token"))

  /**
    * Check if a box is creating a token.
    * @param box - box to check
    * @return true if the box is creation a token, false otherwise
    */
  def tokenRegistersSet(box: ErgoBox): Boolean = {

    // box has tokens
    if(box.additionalTokens.length == 0) return false

    // registers exist
    if(!box.additionalRegisters.contains(R4) ||
       !box.additionalRegisters.contains(R5) ||
       !box.additionalRegisters.contains(R6))
      return false

    // registers correct type
    try {
      box.additionalRegisters(R4).asInstanceOf[ByteColl]
      box.additionalRegisters(R5).asInstanceOf[ByteColl]
      getDecimals(box.additionalRegisters(R6))
    }catch {
      case _: Throwable => return false
    }

    // ok
    true
  }

  /**
    * Get the number of decimals places from a register.
    * Try-catch, because some old tokens used Int to store the decimals, rather than Byte Coll
    * @param reg - register to extract decimals from
    * @return number of decimals places
    */
  def getDecimals(reg: EvaluatedValue[_ <: SType]): Int = {
    try {
      new String(reg.asInstanceOf[ByteColl].value.toArray, "UTF-8").toInt
    }catch {
      case _: Throwable => reg.value.asInstanceOf[Int]
    }
  }

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
    IndexedToken(tokenId, boxId, amount, name, description, decimals)
  }
}

object IndexedToken {

  val extraIndexTypeId: ExtraIndexTypeId = 35.toByte

  /**
    * Construct a token index from a box. Used after checking box with "tokenRegistersSet".
    *
    * @param box - box to use
    * @return token index
    */
  def fromBox(box: ErgoBox, tokenIndex: Int): IndexedToken =
    IndexedToken(bytesToId(box.additionalTokens(tokenIndex)._1),
                 bytesToId(box.id),
                 box.additionalTokens(tokenIndex)._2,
                 new String(box.additionalRegisters(R4).asInstanceOf[ByteColl].value.toArray, "UTF-8"),
                 new String(box.additionalRegisters(R5).asInstanceOf[ByteColl].value.toArray, "UTF-8"),
                 getDecimals(box.additionalRegisters(R6)))
}
