package scorex.core.serialization

import com.google.common.primitives.Ints
import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes, VersionedLDBAVLStorage}
import scorex.util.serialization.{Reader, Writer}

/**
  * Serializer of manifest, a tree which is cut at some `manifestDepth` from root
  */
class ManifestSerializer(manifestDepth: Byte) extends ErgoSerializer[BatchAVLProverManifest[DigestType]] {
  private val nodeSerializer = VersionedLDBAVLStorage.noStoreSerializer

  override def serialize(manifest: BatchAVLProverManifest[DigestType], w: Writer): Unit = {
    val height = manifest.rootHeight
    w.putBytes(Ints.toByteArray(height))
    w.put(manifestDepth)

    def loop(node: ProverNodes[DigestType], level: Int): Unit = {
      nodeSerializer.serialize(node, w)
      node match {
        case _: ProverLeaf[DigestType] =>
        case n: ProxyInternalNode[DigestType] if n.isEmpty =>
        case i: InternalProverNode[DigestType] if level < manifestDepth =>
          loop(i.left, level + 1)
          loop(i.right, level + 1)
      }
    }

    loop(manifest.root, level = 1)
  }

  override def parse(r: Reader): BatchAVLProverManifest[DigestType] = {
    val rootHeight = Ints.fromByteArray(r.getBytes(4))
    val manifestDepth = r.getByte()

    require(manifestDepth == this.manifestDepth, "Wrong manifest depth")

    def loop(level: Int): ProverNodes[DigestType] = {
      val node = nodeSerializer.parse(r)
      node match {
        case _: ProverLeaf[DigestType] => node
        case i: InternalProverNode[DigestType] if level < manifestDepth =>
          val left = loop(level + 1)
          val right = loop(level + 1)
          i.getNew(newLeft = left, newRight = right)
        case i: InternalProverNode[DigestType] if level == manifestDepth =>
          i
      }
    }
    val tree = loop(level = 1)
    new BatchAVLProverManifest[DigestType](tree, rootHeight)
  }

}

object ManifestSerializer {
  val ManifestDepth: Byte = 14
  lazy val defaultSerializer = new ManifestSerializer(ManifestDepth)
}

