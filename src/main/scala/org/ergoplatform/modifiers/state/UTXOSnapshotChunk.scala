package org.ergoplatform.modifiers.state

import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.BlockSection
import scorex.core.ModifierTypeId
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSubtree
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}

/**
* Container for a chunk of sliced AVL+ tree
*/
case class UTXOSnapshotChunk(subTree: Either[BatchAVLProverSubtree[Digest32], Array[Byte]])
  extends BlockSection {

  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.modifierTypeId

  lazy val subtreeDeserialized: BatchAVLProverSubtree[Digest32] = subTree match {
    case Left(st) => st
    case Right(_) => ??? //todo: exception may happen here if bytes
  }

  override lazy val id: ModifierId = bytesToId(subtreeDeserialized.id)

  override def parentId: ModifierId = ???

  //todo: provide id from outside
  override def serializedId: Array[Byte] = subtreeDeserialized.subtreeTop.label

  override type M = UTXOSnapshotChunk

  override def serializer: ScorexSerializer[UTXOSnapshotChunk] = ???

  override val sizeOpt: Option[Int] = None

}

object UTXOSnapshotChunk {
  type StateElement = ErgoBox

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)
}
