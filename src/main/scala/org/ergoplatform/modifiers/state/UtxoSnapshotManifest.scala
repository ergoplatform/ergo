package org.ergoplatform.modifiers.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class UtxoSnapshotManifest(proverManifest: BatchAVLProverManifest[Digest32, Algos.HF],
                                chunkRoots: Seq[ADDigest],
                                blockId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = UtxoSnapshotManifest

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotManifest.modifierTypeId

  override lazy val serializer: Serializer[UtxoSnapshotManifest] = UtxoSnapshotManifestSerializer

  override lazy val sizeOpt: Option[Int] = Some(bytes.length)

  lazy val rootDigest: ADDigest = {
    val (root, height) = proverManifest.oldRootAndHeight
    ADDigest @@ (root.label :+ height.toByte)
  }

  override def serializedId: Array[Byte] = UtxoSnapshot.rootDigestToSerializedId(rootDigest)

  override def parentId: ModifierId = blockId

  def validate(header: Header): Try[Unit] = {
    failFast
      .demandEqualIds(blockId, header.id, s"`blockId` does not correspond to $header")
      .demandEqualArrays(rootDigest, header.stateRoot, "`rootHash` does not correspond to header's `stateRoot`")
      .result
      .toTry
  }

}

object UtxoSnapshotManifest {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)

  def blockIdToSerializedManifestId(blockId: ModifierId): Array[Byte] = Algos.hash(modifierTypeId +: idToBytes(blockId))

  def blockIdToManifestId(blockId: ModifierId): ModifierId = bytesToId(blockIdToSerializedManifestId(blockId))

}

object UtxoSnapshotManifestSerializer extends Serializer[UtxoSnapshotManifest] {

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotManifest): Array[Byte] = {
    val serializedProverManifest = serializer.manifestToBytes(obj.proverManifest)
    idToBytes(obj.blockId) ++
      Ints.toByteArray(serializedProverManifest.length) ++
      serializedProverManifest ++
      Bytes.concat(obj.chunkRoots: _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val proverManifestLen = Ints.fromByteArray(
      bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    val proverManifestTry = serializer.manifestFromBytes(
      bytes.slice(Constants.ModifierIdSize + 4, Constants.ModifierIdSize + 4 + proverManifestLen))
    val chunkRootHashes = bytes.drop(Constants.ModifierIdSize + 4 + proverManifestLen)
      .grouped(Constants.ModifierIdSize)
      .map(ADDigest @@ _)
      .toSeq
    proverManifestTry.map(UtxoSnapshotManifest(_, chunkRootHashes, blockId))
  }.flatten

}
