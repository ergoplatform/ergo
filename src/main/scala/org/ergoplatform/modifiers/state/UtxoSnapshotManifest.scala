package org.ergoplatform.modifiers.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class UtxoSnapshotManifest(chunkRootHashes: IndexedSeq[Array[Byte]],
                                blockId: ModifierId,
                                proverManifest: BatchAVLProverManifest[Digest32, Algos.HF])
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = UtxoSnapshotManifest

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotManifest.modifierTypeId

  override def serializedId: Array[Byte] = UtxoSnapshot.rootHashToSerializedId(rootHash)

  override lazy val id: ModifierId = UtxoSnapshot.rootHashToId(rootHash)

  override lazy val serializer: Serializer[UtxoSnapshotManifest] = UtxoSnapshotManifestSerializer

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

object UtxoSnapshotManifest {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)
}

object UtxoSnapshotManifestSerializer extends Serializer[UtxoSnapshotManifest] {

  val rootHashSize: Int = 33

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotManifest): Array[Byte] = {
    val serializedProverManifest = serializer.manifestToBytes(obj.proverManifest)
    idToBytes(obj.blockId) ++
      Ints.toByteArray(serializedProverManifest.length) ++
      serializedProverManifest ++
      Bytes.concat(obj.chunkRootHashes: _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val proverManifestLen = Ints.fromByteArray(bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    val proverManifestTry = serializer.manifestFromBytes(
      bytes.slice(Constants.ModifierIdSize + 4, Constants.ModifierIdSize + 4 + proverManifestLen))
    val chunkRootHashes = bytes.drop(Constants.ModifierIdSize + 4 + proverManifestLen).grouped(rootHashSize).toIndexedSeq
    proverManifestTry.map(proverManifest => UtxoSnapshotManifest(chunkRootHashes, blockId, proverManifest))
  }.flatten

}
