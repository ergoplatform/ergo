package org.ergoplatform.modifiers.state

import cats.Traverse
import com.google.common.primitives.{Bytes, Ints, Shorts}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoBoxSerializer
import org.ergoplatform.modifiers.state.AUtxoSnapshotChunk.StateElement
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.util.Try

case class AUtxoSnapshotChunk(stateElements: IndexedSeq[StateElement], index: Short)
  extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = AUtxoSnapshotChunk.modifierTypeId

  override lazy val id: ModifierId = AUtxoSnapshot.rootHashToId(rootHash)

  override def serializedId: Array[Byte] = AUtxoSnapshot.rootHashToSerializedId(rootHash)

  override type M = AUtxoSnapshotChunk

  override lazy val serializer: Serializer[AUtxoSnapshotChunk] = AUtxoSnapshotChunkSerializer

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(stateElements.map(LeafData @@ _.bytes))

  override def parentId: ModifierId = ???

  override val sizeOpt: Option[Int] = None

  def correspondsTo(manifest: AUtxoSnapshotManifest): Boolean = {
    manifest.chunkRootHashes.exists(java.util.Arrays.equals(_, rootHash))
  }

}

object AUtxoSnapshotChunk {

  type StateElement = ErgoBox

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)

}

object AUtxoSnapshotChunkSerializer extends Serializer[AUtxoSnapshotChunk] {

  import cats.instances.try_._
  import cats.instances.list._

  override def toBytes(obj: AUtxoSnapshotChunk): Array[Byte] = {
    Shorts.toByteArray(obj.index) ++
      Ints.toByteArray(obj.stateElements.size) ++
      Bytes.concat(obj.stateElements.map(elt => Ints.toByteArray(elt.bytes.length) ++ elt.bytes): _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AUtxoSnapshotChunk] = Try {
    val index = Shorts.fromByteArray(bytes.take(2))
    val elementsQty = Ints.fromByteArray(bytes.slice(2, 6))
    val stateElementsTry = (0 to elementsQty).tail.foldLeft((List.empty[Try[StateElement]], bytes.drop(6))) {
      case ((acc, leftBytes), _) =>
        val eltSize = Ints.fromByteArray(leftBytes.take(4))
        val boxTry = ErgoBoxSerializer.parseBytes(leftBytes.slice(4, 4 + eltSize))
        (acc :+ boxTry, leftBytes.drop(4 + eltSize))
    }._1
    Traverse[List].sequence(stateElementsTry).map(stateElems => AUtxoSnapshotChunk(stateElems.toIndexedSeq, index))
  }.flatten

}
