package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.ErgoAddress
import org.ergoplatform.ErgoAddressEncoder.{ChecksumLength, hash256}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADKey
import scorex.util.{ByteArrayOps, ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

case class IndexedErgoAddress(addressHash: ModifierId,
                              txIds: Seq[ModifierId],
                              boxIds: Seq[BoxId]) extends BlockSection {

  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = idToBytes(addressHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoAddress.modifierTypeId
  override type M = IndexedErgoAddress
  override def serializer: ScorexSerializer[IndexedErgoAddress] = IndexedErgoAddressSerializer

  private var _transactions: Seq[IndexedErgoTransaction] = Seq.empty[IndexedErgoTransaction]
  private var _boxes: Seq[IndexedErgoBox] = Seq.empty[IndexedErgoBox]
  private var _utxos: Seq[IndexedErgoBox] = _

  def transactions(lastN: Long = 20L): Seq[IndexedErgoTransaction] =
    if(lastN > 0)
      _transactions.slice(math.max((_transactions.size - lastN).toInt, 0), _transactions.size)
    else
      _transactions

  def boxes(): Seq[IndexedErgoBox] = _boxes

  def utxos(): Seq[IndexedErgoBox] = _utxos

  def retrieveBody(history: ErgoHistoryReader): IndexedErgoAddress = {
    retrieveTxs(history, -1)
    retrieveBoxes(history, -1)
    this
  }

  def retrieveTxs(history: ErgoHistoryReader, lastN: Long): IndexedErgoAddress = {
    _transactions = (
      if(lastN > 0)
        txIds.slice(math.max((txIds.size - lastN).toInt, 0), txIds.size)
      else
        txIds
      ).map(history.typedModifierById[IndexedErgoTransaction](_).get.retrieveBody(history))
    this
  }

  def retrieveBoxes(history: ErgoHistoryReader, lastN: Long): IndexedErgoAddress = {
    _boxes = (
      if(lastN > 0)
        boxIds.slice(math.max((boxIds.size - lastN).toInt, 0), boxIds.size)
      else
        boxIds
      ).map(x => history.typedModifierById[IndexedErgoBox](bytesToId(x)).get)
    _utxos = _boxes.filter(!_.trackedBox.isSpent)
    this
  }
}

object IndexedErgoAddressSerializer extends ScorexSerializer[IndexedErgoAddress] {

  def addressToBytes(address: ErgoAddress): Array[Byte] = {
    val withNetworkByte = (ExtraIndexerRef.getAddressEncoder.networkPrefix + address.addressTypePrefix).toByte +: address.contentBytes
    withNetworkByte ++ hash256(withNetworkByte).take(ChecksumLength)
  }

  def addressToModifierId(address: ErgoAddress): ModifierId = bytesToId(Algos.hash(addressToBytes(address)))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(idToBytes(iEa.addressHash))
    w.putUInt(iEa.txIds.length)
    iEa.txIds.foreach(m => w.putBytes(m.toBytes))
    w.putUInt(iEa.boxIds.length)
    iEa.boxIds.foreach(w.putBytes(_))
  }

  override def parse(r: Reader): IndexedErgoAddress = {
    val addressHash: ModifierId = bytesToId(r.getBytes(32))
    val txnsLen: Long = r.getUInt()
    var txns: Seq[ModifierId] = Seq.empty[ModifierId]
    for(n <- 1L to txnsLen) txns = txns :+ r.getBytes(32).toModifierId
    val boxesLen: Long = r.getUInt()
    var boxes: Seq[BoxId] = Seq.empty[BoxId]
    for(n <- 1L to boxesLen) boxes = boxes :+ ADKey @@ r.getBytes(32)
    new IndexedErgoAddress(addressHash, txns, boxes)
  }
}

object IndexedErgoAddress {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 15.toByte
}
