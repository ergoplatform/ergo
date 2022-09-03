package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.fastIdToBytes
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.Values.{CollectionConstant, EvaluatedValue}
import sigmastate.{SByte, SType}

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
  override def serializedId: Array[Byte] = fastIdToBytes(uniqueId(tokenId))
}

object IndexedTokenSerializer extends ScorexSerializer[IndexedToken] {

  // necessary, because token ids are sometimes identical to box ids, which causes overwrites
  def uniqueId(tokenId: ModifierId): ModifierId = bytesToId(Algos.hash(tokenId + "token"))

  def tokenRegistersSet(box: ErgoBox): Boolean = {

    // registers exist
    if(!box.additionalRegisters.contains(R4) ||
       !box.additionalRegisters.contains(R5) ||
       !box.additionalRegisters.contains(R6))
      return false

    // registers correct type
    try {
      box.additionalRegisters(R4).asInstanceOf[CollectionConstant[SByte.type]]
      box.additionalRegisters(R5).asInstanceOf[CollectionConstant[SByte.type]]
      getDecimals(box.additionalRegisters(R6))
    }catch {
      case _: Throwable => return false
    }

    // ok
    true
  }

  def getDecimals(reg: EvaluatedValue[_ <: SType]): Int = {
    try {
      new String(reg.asInstanceOf[CollectionConstant[SByte.type]].value.toArray, "UTF-8").toInt
    }catch {
      case _: Throwable => reg.value.asInstanceOf[Int]
    }
  }

  def fromBox(box: ErgoBox): IndexedToken =
    IndexedToken(bytesToId(box.additionalTokens(0)._1),
                 bytesToId(box.id),
                 box.additionalTokens(0)._2,
                 new String(box.additionalRegisters(R4).asInstanceOf[CollectionConstant[SByte.type]].value.toArray, "UTF-8"),
                 new String(box.additionalRegisters(R5).asInstanceOf[CollectionConstant[SByte.type]].value.toArray, "UTF-8"),
                 getDecimals(box.additionalRegisters(R6)))

  override def serialize(iT: IndexedToken, w: Writer): Unit = {
    w.putBytes(fastIdToBytes(iT.tokenId))
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
