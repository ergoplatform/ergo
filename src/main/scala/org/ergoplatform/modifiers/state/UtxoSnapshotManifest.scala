package org.ergoplatform.modifiers.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.ADDigest
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class UtxoSnapshotManifest(serializedProverManifest: Array[Byte],
                                chunkRoots: Seq[ADDigest],
                                blockId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override type M = UtxoSnapshotManifest

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotManifest.modifierTypeId

  override def serializedId: Array[Byte] = UtxoSnapshotManifest.blockIdToSerializedManifestId(blockId)

  override lazy val serializer: Serializer[UtxoSnapshotManifest] = UtxoSnapshotManifestSerializer

  override val sizeOpt: Option[Int] = None

  override def parentId: ModifierId = ???

}

object UtxoSnapshotManifest {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (106: Byte)

  def blockIdToSerializedManifestId(blockId: ModifierId): Array[Byte] = Algos.hash(modifierTypeId +: idToBytes(blockId))

  def blockIdToManifestId(blockId: ModifierId): ModifierId = bytesToId(blockIdToSerializedManifestId(blockId))

}

object UtxoSnapshotManifestSerializer extends Serializer[UtxoSnapshotManifest] {

  override def toBytes(obj: UtxoSnapshotManifest): Array[Byte] = {
    idToBytes(obj.blockId) ++
      Ints.toByteArray(obj.serializedProverManifest.length) ++
      obj.serializedProverManifest ++
      Bytes.concat(obj.chunkRoots: _*)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val proverManifestLen = Ints.fromByteArray(
      bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    val proverManifest = bytes.slice(Constants.ModifierIdSize + 4, Constants.ModifierIdSize + 4 + proverManifestLen)
    val chunkRootHashes = bytes.drop(Constants.ModifierIdSize + 4 + proverManifestLen)
      .grouped(Constants.ModifierIdSize)
      .map(ADDigest @@ _)
      .toSeq
    UtxoSnapshotManifest(proverManifest, chunkRootHashes, blockId)
  }

}
