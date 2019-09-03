package org.ergoplatform.nodeView.history

import com.google.common.primitives.Doubles
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.PoPowParams
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}

final case class ErgoSyncInfo(lastHeaderIds: Seq[ModifierId],
                              poPowParamsOpt: Option[PoPowParams] = None) extends SyncInfo {

  override type M = ErgoSyncInfo

  override lazy val serializer: ScorexSerializer[M] = ErgoSyncInfoSerializer

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    lastHeaderIds.map(Header.modifierTypeId -> _)
}

object ErgoSyncInfo {
  // TODO move to config?
  val MaxBlockIds = 1000
}

object ErgoSyncInfoSerializer extends ScorexSerializer[ErgoSyncInfo] {

  override def serialize(obj: ErgoSyncInfo, w: Writer): Unit = {
    w.putUShort(obj.lastHeaderIds.size)
    obj.lastHeaderIds.foreach( id => w.putBytes(idToBytes(id)))
    obj.poPowParamsOpt.foreach { p =>
      w.putInt(p.m)
      w.putInt(p.k)
      w.putInt(p.k1)
      w.putShortString(p.d.toString)
    }
  }

  override def parse(r: Reader): ErgoSyncInfo = {
    val length = r.getUShort()
    val ids = (1 to length).map(_ => bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize)))
    val poPowParamsOpt = if (r.remaining > 0) {
      val (m, k, k1) = (r.getInt(), r.getInt(), r.getInt())
      val d = Doubles.tryParse(r.getShortString())
      Some(PoPowParams(m, k, k1, d))
    } else {
      None
    }
    ErgoSyncInfo(ids, poPowParamsOpt)
  }

}

object ErgoSyncInfoMessageSpec extends SyncInfoMessageSpec[ErgoSyncInfo](ErgoSyncInfoSerializer)
