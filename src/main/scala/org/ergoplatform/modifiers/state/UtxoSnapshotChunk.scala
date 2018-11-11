package org.ergoplatform.modifiers.state

import cats.Traverse
import com.google.common.primitives.{Bytes, Ints, Shorts}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.util.Try

case class UtxoSnapshotChunk(subtrees: IndexedSeq[BatchAVLProverSubtree[Digest32, Algos.HF]], index: Short)
  extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotChunk.modifierTypeId

  override lazy val id: ModifierId = UtxoSnapshot.rootHashToId(rootHash)

  override def serializedId: Array[Byte] = UtxoSnapshot.rootHashToSerializedId(rootHash)

  override type M = UtxoSnapshotChunk

  override lazy val serializer: Serializer[UtxoSnapshotChunk] = UtxoSnapshotChunkSerializer

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(subtrees.map(LeafData !@@ _.subtreeTop.key))

  override def parentId: ModifierId = ???

  override val sizeOpt: Option[Int] = None

  def correspondsTo(manifest: UtxoSnapshotManifest): Boolean = {
    manifest.chunkRootHashes.exists(java.util.Arrays.equals(_, rootHash))
  }

}

object UtxoSnapshotChunk {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)
}

object UtxoSnapshotChunkSerializer extends Serializer[UtxoSnapshotChunk] {

  import cats.instances.list._
  import cats.instances.try_._

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotChunk): Array[Byte] = {
    val serializedSubtrees = obj.subtrees.map(serializer.subtreeToBytes)
    Shorts.toByteArray(obj.index) ++
      Ints.toByteArray(obj.subtrees.size) ++
      Bytes.concat(serializedSubtrees.map(st => Ints.toByteArray(st.length) ++ st): _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotChunk] = Try {
    val index = Shorts.fromByteArray(bytes.take(2))
    val elementsQty = Ints.fromByteArray(bytes.slice(2, 6))
    val stateElementsTry = (0 to elementsQty).tail
      .foldLeft((List.empty[Try[BatchAVLProverSubtree[Digest32, Algos.HF]]], bytes.drop(6))) {
        case ((acc, leftBytes), _) =>
          val eltSize = Ints.fromByteArray(leftBytes.take(4))
          val subtreeTry = serializer.subtreeFromBytes(leftBytes.slice(4, 4 + eltSize), Constants.HashLength)
          (acc :+ subtreeTry, leftBytes.drop(4 + eltSize))
      }._1
    Traverse[List].sequence(stateElementsTry).map(stateElems => UtxoSnapshotChunk(stateElems.toIndexedSeq, index))
  }.flatten

}
