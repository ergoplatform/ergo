package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.hash.Digest32
import scorex.util._

import scala.util.Try

case class UtxoSnapshotChunk(subtree: BatchAVLProverSubtree[Digest32, Algos.HF], latestManifestId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotChunk.modifierTypeId

  override def serializedId: Array[Byte] = UtxoSnapshot.rootDigestToSerializedId(rootDigest)

  override type M = UtxoSnapshotChunk

  override lazy val serializer: Serializer[UtxoSnapshotChunk] = UtxoSnapshotChunkSerializer

  lazy val rootDigest: ADDigest = ADDigest !@@ subtree.subtreeTop.label

  override def parentId: ModifierId = latestManifestId

  override lazy val sizeOpt: Option[Int] = Some(bytes.length)

  def validate(manifest: UtxoSnapshotManifest): Try[Unit] = {
    failFast
      .demand(manifest.chunkRoots.exists(java.util.Arrays.equals(_, rootDigest)),
        "Chunk does not correspond to declared manifest")
      .result
      .toTry
  }

}

object UtxoSnapshotChunk {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (107: Byte)
}

object UtxoSnapshotChunkSerializer extends Serializer[UtxoSnapshotChunk] {

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotChunk): Array[Byte] = {
    val serializedSubtree = serializer.subtreeToBytes(obj.subtree)
    idToBytes(obj.latestManifestId) ++ serializedSubtree
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotChunk] = Try {
    val manifestId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val subtreeTry = serializer.subtreeFromBytes(bytes.drop(Constants.ModifierIdSize), Constants.HashLength)
    subtreeTry.map(subtree => UtxoSnapshotChunk(subtree, manifestId))
  }.flatten

}
