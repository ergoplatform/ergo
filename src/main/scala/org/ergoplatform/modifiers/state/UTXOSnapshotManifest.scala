package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverManifest
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class UTXOSnapshotManifest(
                                height: Height,
                                blockId: ModifierId,
                                utxoSetDigest: ADDigest, //33 bytes! extra byte with tree height here!
                                manifest: BatchAVLProverManifest[Digest32]
                               ) extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = UTXOSnapshotManifest.modifierTypeId

  override lazy val serializedId: Array[Byte] = Algos.hash(idToBytes(blockId) ++ manifest.id)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = UTXOSnapshotManifest

  override def serializer: ScorexSerializer[UTXOSnapshotManifest] = ???

  override def parentId: ModifierId = ???

  override val sizeOpt: Option[Int] = None
  
}

object UTXOSnapshotManifest {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)

  def validate(manifest: UTXOSnapshotManifest, header: Header): Try[Unit] = Try {
    require(manifest.blockId == header.id)
    ???
  }

}

