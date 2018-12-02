package org.ergoplatform.modifiers.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.ModifierTypeId
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSerializer, BatchAVLProverSubtree}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.crypto.hash.Digest32
import scorex.util._

import scala.annotation.tailrec
import scala.util.Try

/** Holds single subtree of sliced state tree.
  */
case class UtxoSnapshotChunk(subtree: BatchAVLProverSubtree[Digest32, Algos.HF], manifestId: ModifierId)
  extends ErgoPersistentModifier with ModifierValidator {

  override val modifierTypeId: ModifierTypeId = UtxoSnapshotChunk.modifierTypeId

  override def serializedId: Array[Byte] = UtxoSnapshot.digestToSerializedId(rootDigest)

  override type M = UtxoSnapshotChunk

  override lazy val serializer: Serializer[UtxoSnapshotChunk] = UtxoSnapshotChunkSerializer

  lazy val rootDigest: Digest32 = subtree.subtreeTop.label

  override def parentId: ModifierId = manifestId

  override lazy val sizeOpt: Option[Int] = Some(bytes.length)

  def validate(manifest: UtxoSnapshotManifest): Try[Unit] = {
    failFast
      .demand(manifest.chunkRoots.exists(java.util.Arrays.equals(_, rootDigest)),
        "Chunk does not correspond to manifest")
      .demand(validSubtree, "Invalid subtree")
      .result
      .toTry
  }

  /** Checks that each tree branch ends with ProverLeaf.
    */
  private def validSubtree: Boolean = {
    def isLeaf(n: ProverNodes[Digest32]): Boolean = n match {
      case _: ProverLeaf[Digest32] => true
      case _ => false
    }
    def nonEmpty(n: InternalProverNode[Digest32]): Boolean = Option(n.left).flatMap(_ => Option(n.right)).nonEmpty
    @tailrec
    def validTree(nodes: Seq[ProverNodes[Digest32]], validity: Boolean = true): Boolean = {
      nodes match {
        case (n: InternalProverNode[Digest32]) +: tail if nonEmpty(n) =>
          validTree(n.left +: n.right +: tail, validity)
        case (n: InternalProverNode[Digest32]) +: tail if Option(n.left).nonEmpty && isLeaf(n.left) =>
          validTree(n.left +: tail, validity)
        case (n: InternalProverNode[Digest32]) +: tail if Option(n.right).nonEmpty && isLeaf(n.right) =>
          validTree(n.right +: tail, validity)
        case (_: InternalProverNode[Digest32]) +: _ =>
          validTree(Seq.empty, validity = false)
        case (_: ProverLeaf[Digest32]) +: tail =>
          validTree(tail, validity)
        case seq if seq.isEmpty =>
          validity
      }
    }
    validTree(Seq(subtree.subtreeTop))
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
    idToBytes(obj.manifestId) ++ serializedSubtree
  }

  override def parseBytes(bytes: Array[Byte]): Try[UtxoSnapshotChunk] = Try {
    val manifestId = bytesToId(bytes.take(Constants.ModifierIdSize))
    val subtreeTry = serializer.subtreeFromBytes(bytes.drop(Constants.ModifierIdSize), Constants.HashLength)
    subtreeTry.map(subtree => UtxoSnapshotChunk(subtree, manifestId))
  }.flatten

}
