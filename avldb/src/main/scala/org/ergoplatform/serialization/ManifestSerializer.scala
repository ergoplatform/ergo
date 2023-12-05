package org.ergoplatform.serialization

import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, ProxyInternalNode}
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes, VersionedLDBAVLStorage}
import scorex.util.serialization.{Reader, Writer}

/**
  * Serializer of manifest, a tree which is cut at some `manifestDepth` from root
  */
class ManifestSerializer(manifestDepth: Byte) extends ErgoSerializer[BatchAVLProverManifest[DigestType]] {
  private val nodeSerializer = VersionedLDBAVLStorage.noStoreSerializer

  /**
    * Serialize manifest provided as top subtree and height separately. Used in tests.
    */
  def serialize(rootNode: ProverNodes[DigestType], rootNodeHeight: Byte, w: Writer): Unit = {
    w.put(rootNodeHeight)
    w.put(manifestDepth)

    def loop(node: ProverNodes[DigestType], level: Int): Unit = {
      nodeSerializer.serialize(node, w)
      node match {
        case n: ProxyInternalNode[DigestType] if n.isEmpty =>
        case i: InternalProverNode[DigestType] if level < manifestDepth =>
          loop(i.left, level + 1)
          loop(i.right, level + 1)
        case _: InternalProverNode[DigestType] | _: ProverLeaf[DigestType] =>
      }
    }

    loop(rootNode, level = 1)
  }

  override def serialize(manifest: BatchAVLProverManifest[DigestType], w: Writer): Unit = {
    serialize(manifest.root, manifest.rootHeight.toByte, w)
  }

  override def parse(r: Reader): BatchAVLProverManifest[DigestType] = {
    val rootHeight = r.getByte()
    val manifestDepth = r.getByte()

    require(manifestDepth == this.manifestDepth,
            s"Wrong manifest depth, found: $manifestDepth, expected: ${this.manifestDepth}")

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
  /**
    * Current manifest depth in the Ergo mainnet
    */
  val MainnetManifestDepth: Byte = 14

  /**
    * Manifest serializer used in the Ergo mainnet
    */
  val defaultSerializer = new ManifestSerializer(MainnetManifestDepth)
}

