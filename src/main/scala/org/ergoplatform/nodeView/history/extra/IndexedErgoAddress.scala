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

import scala.collection.mutable.ListBuffer
import spire.syntax.all.cfor

case class IndexedErgoAddress(addressHash: ModifierId,
                              txIds: ListBuffer[ModifierId],
                              boxIds: ListBuffer[BoxId]) extends BlockSection {

  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = idToBytes(addressHash)
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoAddress.modifierTypeId
  override type M = IndexedErgoAddress
  override def serializer: ScorexSerializer[IndexedErgoAddress] = IndexedErgoAddressSerializer

  private var _transactions: ListBuffer[IndexedErgoTransaction] = ListBuffer.empty[IndexedErgoTransaction]
  private var _boxes: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]
  private var _utxos: ListBuffer[IndexedErgoBox] = ListBuffer.empty[IndexedErgoBox]

  def transactions(lastN: Long = 20L): ListBuffer[IndexedErgoTransaction] =
    if(lastN > 0)
      _transactions.slice(math.max((_transactions.size - lastN).toInt, 0), _transactions.size)
    else
      _transactions

  def boxes(): ListBuffer[IndexedErgoBox] = _boxes

  def utxos(): ListBuffer[IndexedErgoBox] = _utxos

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

  def addTx(id: ModifierId): IndexedErgoAddress = {
    txIds += id
    this
  }

  def addBox(box: BoxId): IndexedErgoAddress = {
    boxIds += box
    this
  }
}

object IndexedErgoAddressSerializer extends ScorexSerializer[IndexedErgoAddress] {

  def addressToBytes(address: ErgoAddress): Array[Byte] = {
    val withNetworkByte = (ExtraIndexerRef.getAddressEncoder.networkPrefix + address.addressTypePrefix).toByte +: address.contentBytes
    withNetworkByte ++ hash256(withNetworkByte).take(ChecksumLength)
  }

  def hashAddress(address: ErgoAddress): Array[Byte] = Algos.hash(addressToBytes(address))

  def addressToModifierId(address: ErgoAddress): ModifierId = bytesToId(hashAddress(address))

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putBytes(idToBytes(iEa.addressHash))
    w.putUInt(iEa.txIds.length)
    cfor(0)(_ < iEa.txIds.length, _ + 1) { i => w.putBytes(iEa.txIds(i).toBytes)}
    w.putUInt(iEa.boxIds.length)
    cfor(0)(_ < iEa.boxIds.length, _ + 1) { i => w.putBytes(iEa.boxIds(i))}
  }

  override def parse(r: Reader): IndexedErgoAddress = {
    val addressHash: ModifierId = bytesToId(r.getBytes(32))
    val txnsLen: Long = r.getUInt()
    val txns: ListBuffer[ModifierId] = ListBuffer.empty[ModifierId]
    cfor(0)(_ < txnsLen, _ + 1) {_ => txns += r.getBytes(32).toModifierId}
    val boxesLen: Long = r.getUInt()
    val boxes: ListBuffer[BoxId] = ListBuffer.empty[BoxId]
    cfor(0)(_ < boxesLen, _ + 1) { _ => boxes += ADKey @@ r.getBytes(32)}
    new IndexedErgoAddress(addressHash, txns, boxes)
  }
}

object IndexedErgoAddress {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 15.toByte
}
