package org.ergoplatform.modifiers.state

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSerializer, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverNodes}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.Try

case class UtxoSnapshotManifest(proverManifest: BatchAVLProverManifest[Digest32, Algos.HF],
                                blockId: ModifierId,
                                emissionBoxIdOpt: Option[ADKey])
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

  def chunkRoots: Seq[ADDigest] = {
    def loop(node: ProverNodes[Digest32]): Seq[ADDigest] = node match {
      case n: ProxyInternalNode[Digest32] =>
        Seq(n.leftLabel, n.rightLabel).map(ADDigest !@@ _)
      case n: InternalProverNode[Digest32] =>
        loop(n.left) ++ loop(n.right)
      case _ =>
        Seq.empty
    }
    loop(proverManifest.oldRootAndHeight._1)
  }

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
}

object UtxoSnapshotManifestSerializer extends Serializer[UtxoSnapshotManifest] {

  private implicit val hf: Algos.HF = Algos.hash
  private val serializer = new BatchAVLProverSerializer[Digest32, Algos.HF]

  override def toBytes(obj: UtxoSnapshotManifest): Array[Byte] = {
    val serializedProverManifest = serializer.manifestToBytes(obj.proverManifest)
    idToBytes(obj.blockId) ++
      Ints.toByteArray(serializedProverManifest.length) ++
      serializedProverManifest ++
      obj.emissionBoxIdOpt.getOrElse(Array.empty)
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotManifest] = Try {
    val blockId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val proverManifestLen = Ints.fromByteArray(
      bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    val requiredBytesLen = Constants.ModifierIdSize + 4 + proverManifestLen
    val proverManifestTry = serializer.manifestFromBytes(
      bytes.slice(Constants.ModifierIdSize + 4, requiredBytesLen))
    val emissionBoxIdOpt: Option[ADKey] = if (bytes.length - requiredBytesLen == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else {
      None
    }
    proverManifestTry.map(UtxoSnapshotManifest(_, blockId, emissionBoxIdOpt))
  }.flatten

}
