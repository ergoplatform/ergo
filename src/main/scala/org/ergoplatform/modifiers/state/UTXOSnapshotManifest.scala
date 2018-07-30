package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core._
import scorex.core.utils.concatBytes
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32

import scala.util.Try

case class UTXOSnapshotManifest(chunkRootHashes: Seq[Array[Byte]], blockId: ModifierId) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = UTXOSnapshotManifest.modifierTypeId

  override def serializedId: Array[Byte] = Algos.hash(concatBytes(chunkRootHashes :+ idToBytes(blockId)))

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = UTXOSnapshotManifest

  override lazy val serializer: Serializer[UTXOSnapshotManifest] = ???

  override def parentId: ModifierId = ???

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(LeafData @@ chunkRootHashes)
}

object UTXOSnapshotManifest {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)

  def validate(manifest: UTXOSnapshotManifest, header: Header): Try[Unit] = Try {
    require(manifest.blockId == header.id)
    require(java.util.Arrays.equals(manifest.rootHash, header.stateRoot))
    ???
  }
}

