package org.ergoplatform.modifiers.state

import com.google.common.primitives.Bytes
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class AUtxoSnapshotManifest(chunkRootHashes: IndexedSeq[Array[Byte]], blockId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = AUtxoSnapshotManifest

  override val modifierTypeId: ModifierTypeId = AUtxoSnapshotManifest.modifierTypeId

  override def serializedId: Array[Byte] = AUtxoSnapshot.rootHashToSerializedId(rootHash)

  override lazy val id: ModifierId = AUtxoSnapshot.rootHashToId(rootHash)

  override lazy val serializer: Serializer[AUtxoSnapshotManifest] = AUtxoSnapshotManifestSerializer

  override def parentId: ModifierId = ???

  lazy val rootHash: Digest32 = Algos.merkleTreeRoot(LeafData @@ chunkRootHashes)

  override val sizeOpt: Option[Int] = None

  def validate(header: Header): Try[Unit] = {
    failFast
      .demandEqualIds(blockId, header.id, s"`blockId` does not correspond to $header")
      .demandEqualArrays(rootHash, header.stateRoot, "`rootHash` does not correspond to header's `stateRoot`")
      .result
      .toTry
  }
}

object AUtxoSnapshotManifest {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)
}

object AUtxoSnapshotManifestSerializer extends Serializer[AUtxoSnapshotManifest] {

  val rootHashSize: Int = 33

  override def toBytes(obj: AUtxoSnapshotManifest): Array[Byte] = {
    idToBytes(obj.blockId) ++ Bytes.concat(obj.chunkRootHashes: _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AUtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val chunkRootHashes = bytes.drop(Constants.ModifierIdSize).grouped(rootHashSize).toIndexedSeq
    AUtxoSnapshotManifest(chunkRootHashes, blockId)
  }

}
