package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.{SByte, SType}
import sigmastate.Values.CollectionConstant
import special.collection.Coll

case class IndexedToken(tokenId: ModifierId,
                        boxId: ModifierId,
                        amount: Long,
                        name: String,
                        description: String,
                        decimals: Int) extends BlockSection {
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedToken.modifierTypeId
  override type M = IndexedToken
  override def serializer: ScorexSerializer[IndexedToken] = IndexedTokenSerializer
  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = fastIdToBytes(tokenId)
}

object IndexedTokenSerializer extends ScorexSerializer[IndexedToken] with ScorexLogging {

  def tokenRegistersSet(box: ErgoBox): Boolean =
    box.additionalRegisters.contains(R4) && box.additionalRegisters.contains(R5) && box.additionalRegisters.contains(R6)

  def getDecimals(x: SType#WrappedType): Int = {
    try {
      x.asInstanceOf[Coll[Byte]].toArray(0)
    }catch {
      case _: Throwable => x.asInstanceOf[Int]
    }
  }

  def fromBox(box: ErgoBox): IndexedToken =
    IndexedToken(bytesToId(box.additionalTokens(0)._1),
                 bytesToId(box.id),
                 box.additionalTokens(0)._2,
                 new String(box.additionalRegisters(R4).asInstanceOf[CollectionConstant[SByte.type]].value.toArray, "UTF-8"),
                 new String(box.additionalRegisters(R5).asInstanceOf[CollectionConstant[SByte.type]].value.toArray, "UTF-8"),
                 getDecimals(box.additionalRegisters(R6).value))

  override def serialize(iT: IndexedToken, w: Writer): Unit = {
    w.putBytes(iT.serializedId)
    w.putBytes(fastIdToBytes(iT.boxId))
    w.putULong(iT.amount)
    w.putUShort(iT.name.length)
    w.putBytes(iT.name.getBytes("UTF-8"))
    w.putUShort(iT.description.length)
    w.putBytes(iT.description.getBytes("UTF-8"))
    w.putInt(iT.decimals)
  }

  override def parse(r: Reader): IndexedToken = {
    val tokenId: ModifierId = bytesToId(r.getBytes(32))
    val boxId: ModifierId = bytesToId(r.getBytes(32))
    val amount: Long = r.getULong()
    val name: String = new String(r.getBytes(r.getUShort), "UTF-8")
    val description: String = new String(r.getBytes(r.getUShort), "UTF-8")
    val decimals: Int = r.getInt()
    IndexedToken(tokenId, boxId, amount, name, description, decimals)
  }
}

object IndexedToken {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 35.toByte
}
