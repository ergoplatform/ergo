package org.ergoplatform.serialization

import scorex.crypto.authds.avltree.batch.Constants.DigestType
import scorex.crypto.authds.avltree.batch.{InternalProverNode, ProverLeaf, ProverNodes, VersionedLDBAVLStorage}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverSubtree, ProxyInternalNode}
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}

/**
  * Serializer for subtree
  */
object SubtreeSerializer extends ErgoSerializer[BatchAVLProverSubtree[DigestType]] with ScorexLogging {
  private val nodeSerializer = VersionedLDBAVLStorage.noStoreSerializer
  private val MaxSubtreeNodeLevels = 127 // max AVL+ tree height

  override def serialize(subtree: BatchAVLProverSubtree[DigestType], w: Writer): Unit = {
    def loop(node: ProverNodes[DigestType]): Unit = {
      nodeSerializer.serialize(node, w)
      node match {
        case _: ProverLeaf[DigestType] =>
        case n: ProxyInternalNode[DigestType] if n.isEmpty =>
          log.warn("Proxy node in subtree serialization")
        case i: InternalProverNode[DigestType] =>
          loop(i.left)
          loop(i.right)
      }
    }

    loop(subtree.subtreeTop)
  }

  override def parse(r: Reader): BatchAVLProverSubtree[DigestType] = {
    def loop(level: Int): ProverNodes[DigestType] = {
      require(level <= MaxSubtreeNodeLevels, "Snapshot subtree exceeds maximum AVL height")
      val node = nodeSerializer.parse(r)
      node match {
        case _: ProverLeaf[DigestType] => node
        case i: InternalProverNode[DigestType] =>
          val left = loop(level + 1)
          val right = loop(level + 1)
          i.getNew(newLeft = left, newRight = right)
      }
    }
    val tree = loop(level = 0)
    new BatchAVLProverSubtree[DigestType](tree)
  }

}
