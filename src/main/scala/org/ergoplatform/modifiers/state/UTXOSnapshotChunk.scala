package org.ergoplatform.modifiers.state

import cats.Traverse
import com.google.common.primitives.{Bytes, Ints, Shorts}
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoBoxSerializer
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk.StateElement
import org.ergoplatform.settings.Algos
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

import scala.util.Try

case class UTXOSnapshotChunk(stateElements: IndexedSeq[StateElement], index: Short)
  extends ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = UTXOSnapshotChunk.modifierTypeId

  override lazy val id: ModifierId = UTXOSnapshot.rootHashToId(rootHash)

  override def serializedId: Array[Byte] = UTXOSnapshot.rootHashToSerializedId(rootHash)

  override type M = UTXOSnapshotChunk

  override lazy val serializer: Serializer[UTXOSnapshotChunk] = UTXOSnapshotChunkSerializer

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(stateElements.map(LeafData @@ _.bytes))

  override def parentId: ModifierId = ???

  override val sizeOpt: Option[Int] = None

  def correspondsTo(manifest: UTXOSnapshotManifest): Boolean = {
    manifest.chunkRootHashes.exists(java.util.Arrays.equals(_, rootHash))
  }

}

object UTXOSnapshotChunk {

  type StateElement = ErgoBox

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)

}

object UTXOSnapshotChunkSerializer extends Serializer[UTXOSnapshotChunk] {

  import cats.instances.try_._
  import cats.instances.list._

  override def toBytes(obj: UTXOSnapshotChunk): Array[Byte] = {
    Shorts.toByteArray(obj.index) ++
      Ints.toByteArray(obj.stateElements.size) ++
      Bytes.concat(obj.stateElements.map(elt => Ints.toByteArray(elt.bytes.length) ++ elt.bytes): _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UTXOSnapshotChunk] = {
    val indexTry = Try(Shorts.fromByteArray(bytes.take(2)))
    val elementsQtyTry = Try(Ints.fromByteArray(bytes.slice(2, 6)))
    val stateElementsTry = elementsQtyTry.flatMap { qty =>
      val elementsTry = (1 to qty).foldLeft((List.empty[Try[StateElement]], bytes.drop(6))) {
        case ((acc, leftBytes), _) =>
          val eltSize = Ints.fromByteArray(leftBytes.take(4))
          val boxTry = ErgoBoxSerializer.parseBytes(leftBytes.slice(4, eltSize))
          (acc :+ boxTry, leftBytes.drop(4 + eltSize))
      }._1
      Traverse[List].sequence(elementsTry)
    }
    indexTry.flatMap { idx =>
      stateElementsTry.map(stateElems => UTXOSnapshotChunk(stateElems.toIndexedSeq, idx))
    }
  }

}
