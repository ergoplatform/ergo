package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, P2PKAddress, Pay2SAddress, Pay2SHAddress}
import org.ergoplatform.ErgoAddressEncoder.{ChecksumLength, hash256}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.ADKey
import scorex.util.{ByteArrayOps, ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.serialization.{ErgoTreeSerializer, GroupElementSerializer, SigmaSerializer}

class IndexedErgoAddress(val address: ErgoAddress,
                         val txIds: Seq[ModifierId],
                         val boxIds: Seq[BoxId]) extends BlockSection {

  override val sizeOpt: Option[Int] = None
  override def serializedId: Array[Byte] = Algos.hash(IndexedErgoAddressSerializer.addressToBytes(address))
  override def parentId: ModifierId = null
  override val modifierTypeId: ModifierTypeId = IndexedErgoAddress.modifierTypeId
  override type M = IndexedErgoAddress
  override def serializer: ScorexSerializer[IndexedErgoAddress] = IndexedErgoAddressSerializer

  def getBytes: Array[Byte] = serializer.toBytes(this)

  private var _transactions: Seq[IndexedErgoTransaction] = Seq.empty[IndexedErgoTransaction]
  private var _boxes: Seq[IndexedErgoBox] = Seq.empty[IndexedErgoBox]
  private var _utxos: Seq[IndexedErgoBox] = _

  def transactions(lastN: Long = 20L): Seq[IndexedErgoTransaction] =
    if(lastN > 0)
      _transactions.slice(math.max((_transactions.size - lastN).toInt, 0), _transactions.size)
    else
      _transactions

  def boxes(lastN: Long = 20L): Seq[IndexedErgoBox] =
    if(lastN > 0)
      _boxes.slice(math.max((_boxes.size - lastN).toInt, 0), _boxes.size)
    else
      _boxes

  def utxos(lastN: Long = 20L): Seq[IndexedErgoBox] = {
    if(lastN > 0)
      _utxos.slice(math.max((_utxos.size - lastN).toInt, 0), _utxos.size)
    else
      _utxos
  }

  def retrieveBody(history: ErgoHistoryReader): IndexedErgoAddress = {
    _transactions = txIds.map(history.typedModifierById[IndexedErgoTransaction](_).get.retrieveBody(history))
    _boxes = boxIds.map(x => history.typedModifierById[IndexedErgoBox](bytesToId(x)).get)
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

  def bytesToAddress(bytes: Array[Byte]): ErgoAddress = {
    val contentBytes: Array[Byte] = bytes.slice(1, bytes.length - ChecksumLength)
    implicit val encoder: ErgoAddressEncoder = ExtraIndexerRef.getAddressEncoder
    (bytes(0) & 0x0F).toByte match {
      case P2PKAddress.  addressTypePrefix => new P2PKAddress(ProveDlog(GroupElementSerializer.parseTry(SigmaSerializer.startReader(contentBytes)).get), contentBytes)
      case Pay2SHAddress.addressTypePrefix => new Pay2SHAddress(contentBytes)
      case Pay2SAddress. addressTypePrefix => new Pay2SAddress(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(contentBytes), contentBytes)
    }
  }

  override def serialize(iEa: IndexedErgoAddress, w: Writer): Unit = {
    w.putUByte(IndexedErgoAddress.modifierTypeId)
    val addressBytes: Array[Byte] = addressToBytes(iEa.address)
    w.putUShort(addressBytes.length)
    w.putBytes(addressBytes)
    w.putUInt(iEa.txIds.length)
    iEa.txIds.foreach(m => w.putBytes(m.toBytes))
    w.putUInt(iEa.boxIds.length)
    iEa.boxIds.foreach(w.putBytes(_))
  }

  override def parse(r: Reader): IndexedErgoAddress = {
    val addressLen: Int = r.getUShort()
    val address: ErgoAddress = bytesToAddress(r.getBytes(addressLen))
    val txnsLen: Long = r.getUInt()
    var txns: Seq[ModifierId] = Seq.empty[ModifierId]
    for(n <- 1L to txnsLen) txns = txns :+ r.getBytes(32).toModifierId
    val boxesLen: Long = r.getUInt()
    var boxes: Seq[BoxId] = Seq.empty[BoxId]
    for(n <- 1L to boxesLen) boxes = boxes :+ ADKey @@ r.getBytes(32)
    new IndexedErgoAddress(address, txns, boxes)
  }
}

object IndexedErgoAddress {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 15.toByte
}
