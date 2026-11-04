package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Algos
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import scorex.util.serialization.{Reader, Writer}
import sigma.VersionContext
import sigma.ast.ErgoTree

import scala.collection.mutable.ArrayBuffer

case class IndexedContractTemplate(templateHash: ModifierId,
                                   override val boxes: ArrayBuffer[Long] = new ArrayBuffer[Long])
  extends Segment[IndexedContractTemplate](id => IndexedContractTemplate(id), new ArrayBuffer[Long], boxes)
    with ExtraIndex {

  override lazy val id: ModifierId = templateHash
  override def serializedId: Array[Byte] = fastIdToBytes(templateHash)

  override private[extra] def rollback(txTarget: Long, boxTarget: Long, history: ErgoHistory)(implicit segmentTreshold: Int): Array[ModifierId] = {
    val toRemove: ArrayBuffer[ModifierId] = rollbackState(txTarget, boxTarget, history.getReader)

    if (boxCount == 0)
      toRemove += templateHash
    else
      history.historyStorage.insertExtraTry(Array.empty, Array(this)).get

    toRemove.toArray
  }

  @deprecated("Indexed templates do not track transactions", "")
  override private[extra] def addTx(tx: Long) = this

  override private[extra] def addBox(iEb: IndexedErgoBox, record: Boolean): IndexedContractTemplate = {
    if (record) boxes += iEb.globalIndex
    this
  }

  override private[extra] def spendBox(iEb: IndexedErgoBox, historyOpt: Option[ErgoHistoryReader])(implicit ae: ErgoAddressEncoder): IndexedContractTemplate = {
    if (historyOpt.isDefined)
      findAndModBox(iEb.globalIndex, historyOpt.get)
    this
  }

  override private[extra] def filterMempool(boxes: Seq[ErgoBox]): Seq[ErgoBox] =
    boxes.filter(box => hashTreeTemplate(box.ergoTree) == templateHash)
}

object IndexedContractTemplateSerializer extends ErgoSerializer[IndexedContractTemplate] with ScorexLogging {

  def hashTreeTemplate(tree: ErgoTree): ModifierId = {
    try {
      VersionContext.withVersions(3, 3) {
        bytesToId(Algos.hash(tree.template))
      }
    } catch {
      case _: Throwable =>
        log.error("Can't parse template for: " + tree.bytesHex)
        // tree bytes hash is the id of the address index, so hash it with a suffix (as for tokens) to get a fake template hash.
        // The key is internal: no documented route derives it. If a later sigma version can parse such a tree, its key
        // moves to the normal path above and the row under this key stays.
        bytesToId(Algos.hash(IndexedErgoAddressSerializer.hashErgoTree(tree) + "template"))
    }
  }

  override def serialize(iCt: IndexedContractTemplate, w: Writer): Unit = {
    w.putBytes(iCt.serializedId)
    SegmentSerializer.serialize(iCt, w)
  }

  override def parse(r: Reader): IndexedContractTemplate = {
    val templateHash: ModifierId = bytesToId(r.getBytes(32))
    val iCt: IndexedContractTemplate = new IndexedContractTemplate(templateHash)
    SegmentSerializer.parse(r, iCt)
    iCt
  }
}

object IndexedContractTemplate {
  val extraIndexTypeId: ExtraIndexTypeId = 20.toByte
}
