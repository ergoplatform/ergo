package org.ergoplatform.modifiers.state

import io.circe.Json
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendNoncedBox
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk.StateElement
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.utils.Random

import scala.util.Try

case class UTXOSnapshotChunk(stateElements: Seq[StateElement], index: Short) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.ModifierTypeId

  //TODO implement correctly
  override lazy val id: ModifierId = Random.randomBytes(32)

  override type M = UTXOSnapshotChunk

  override lazy val serializer: Serializer[UTXOSnapshotChunk] = UTXOSnapshotChunkSerializer

  override lazy val json: Json = ???

  lazy val rootHash: Array[Byte] = Algos.merkleTreeRoot(stateElements.map(_.bytes))
}

object UTXOSnapshotChunk {
  type StateElement = AnyoneCanSpendNoncedBox

  val ModifierTypeId: Byte = 107: Byte
}

object UTXOSnapshotChunkSerializer extends Serializer[UTXOSnapshotChunk] {
  override def toBytes(obj: UTXOSnapshotChunk): Array[ModifierTypeId] = ???

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[UTXOSnapshotChunk] = ???
}
