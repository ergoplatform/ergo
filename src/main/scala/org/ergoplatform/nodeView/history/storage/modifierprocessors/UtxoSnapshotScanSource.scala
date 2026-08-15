package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.serialization.{ErgoSerializer, ManifestSerializer, SubtreeSerializer}
import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/** Narrow read-only authority for persisted immutable UTXO snapshot scan parts. */
trait UtxoSnapshotScanSourceReader {
  /** Read and validate the source identified by the expected snapshot block. */
  def readUtxoSnapshotScanSource(expectedBlockId: ModifierId): Try[UtxoSnapshotScanSource]

  /** Read and verify one deterministic part from an already validated immutable source. */
  def readUtxoSnapshotScanPart(source: UtxoSnapshotScanSource,
                               index: Int): Try[BatchAVLProverSubtree[DigestType]]
}

/** Immutable identity and exact manifest bytes retained for a UTXO snapshot scan. */
final case class UtxoSnapshotScanSource(snapshotHeight: Height,
                                        snapshotBlockId: ModifierId,
                                        manifestDepth: Byte,
                                        private val exactManifestBytes: Array[Byte],
                                        private val parts: IndexedSeq[UtxoSnapshotScanSource.Part]) {

  /** Defensive copy of the exact serialized manifest retained by this source. */
  def manifestBytes: Array[Byte] = exactManifestBytes.clone()

  /** Exact number of deterministic DFS scan parts described by the manifest. */
  def partCount: Int = parts.size

  /** Number of retained ordinal chunk rows referenced by this manifest. */
  def chunkCount: Int = parts.count(_.isInstanceOf[UtxoSnapshotScanSource.ChunkPart])

  /** Read and verify one bounded scan part by its deterministic index. */
  def readPart(index: Int,
               readChunk: Int => Try[Array[Byte]]): Try[BatchAVLProverSubtree[DigestType]] = Try {
    require(index >= 0 && index < parts.size, s"Snapshot scan part index $index is out of bounds")
    parts(index)
  }.flatMap {
    case UtxoSnapshotScanSource.EmbeddedPart(leaf) =>
      Success(new BatchAVLProverSubtree[DigestType](leaf))
    case UtxoSnapshotScanSource.ChunkPart(ordinal, expectedId) =>
      readChunk(ordinal)
        .flatMap(SubtreeSerializer.parseBytesTry)
        .flatMap { subtree =>
          if (subtree.verify(expectedId)) Success(subtree)
          else Failure(new IllegalArgumentException(
            s"Snapshot chunk $ordinal does not match its manifest identifier"))
        }
  }
}

object UtxoSnapshotScanSource {
  private[modifierprocessors] sealed trait Part
  private final case class EmbeddedPart(leaf: ProverLeaf[DigestType]) extends Part
  private final case class ChunkPart(ordinal: Int, expectedId: Digest32) extends Part

  /** Parse exact manifest bytes and derive deterministic DFS left-before-right scan parts. */
  def create(snapshotHeight: Height,
             snapshotBlockId: ModifierId,
             manifestDepth: Byte,
             manifestBytes: Array[Byte]): Try[UtxoSnapshotScanSource] = {
    new ManifestSerializer(manifestDepth).parseBytesTry(manifestBytes).map { manifest =>
      val parts = ArrayBuffer.empty[Part]
      var chunkOrdinal = 0

      def loop(node: ProverNodes[DigestType]): Unit = node match {
        case leaf: ProverLeaf[DigestType] =>
          parts += EmbeddedPart(leaf)
        case proxy: ProxyInternalNode[DigestType] if proxy.isEmpty =>
          parts += ChunkPart(chunkOrdinal, proxy.leftLabel)
          chunkOrdinal += 1
          parts += ChunkPart(chunkOrdinal, proxy.rightLabel)
          chunkOrdinal += 1
        case internal: InternalProverNode[DigestType] =>
          loop(internal.left)
          loop(internal.right)
      }

      loop(manifest.root)
      UtxoSnapshotScanSource(
        snapshotHeight,
        snapshotBlockId,
        manifestDepth,
        manifestBytes.clone(),
        parts.toIndexedSeq
      )
    }
  }
}

/** Stable serializer for the immutable UTXO snapshot scan descriptor. */
object UtxoSnapshotScanSourceSerializer extends ErgoSerializer[UtxoSnapshotScanSource] {
  private val FormatVersion: Byte = 1
  private val MaxManifestBytes: Int = 4000000

  override def serialize(source: UtxoSnapshotScanSource, w: Writer): Unit = {
    val manifestBytes = source.manifestBytes
    require(manifestBytes.nonEmpty && manifestBytes.length <= MaxManifestBytes,
      s"Snapshot manifest length ${manifestBytes.length} is out of bounds")
    w.put(FormatVersion)
    w.putInt(source.snapshotHeight)
    w.putBytes(idToBytes(source.snapshotBlockId))
    w.put(source.manifestDepth)
    w.putUInt(manifestBytes.length.toLong)
    w.putBytes(manifestBytes)
  }

  override def parse(r: Reader): UtxoSnapshotScanSource = {
    val version = r.getByte()
    require(version == FormatVersion, s"Unsupported snapshot scan source version $version")
    val height = r.getInt()
    val blockId = bytesToId(r.getBytes(32))
    val manifestDepth = r.getByte()
    val manifestLengthLong = r.getUInt()
    require(manifestLengthLong <= Int.MaxValue, "Snapshot manifest bytes are too large")
    val manifestLength = manifestLengthLong.toInt
    require(manifestLength > 0 && manifestLength <= MaxManifestBytes,
      s"Snapshot manifest length $manifestLength is out of bounds")
    val source = UtxoSnapshotScanSource
      .create(height, blockId, manifestDepth, r.getBytes(manifestLength))
      .get
    require(r.remaining == 0, s"Unexpected trailing snapshot scan source bytes: ${r.remaining}")
    source
  }
}
