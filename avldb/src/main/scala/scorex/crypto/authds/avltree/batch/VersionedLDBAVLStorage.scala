package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage.{InternalNodePrefix, LeafPrefix}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, Balance}
import scorex.util.encode.Base58
import scorex.crypto.hash
import scorex.crypto.hash.{CryptographicHash, Digest}
import scorex.db.LDBVersionedStore
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.util.{Failure, Try}

/**
  * Persistent versioned authenticated AVL+ tree implementation on top of versioned LevelDB storage
  *
  * @param store - level db storage to save the tree in
  * @param nodeParameters - parameters of the tree node (key size, optional value size, label size)
  * @param hf - hash function used to construct the tree
  * @tparam D - type of hash function digest
  */
class VersionedLDBAVLStorage[D <: Digest, HF <: CryptographicHash[D]](store: LDBVersionedStore,
                                          nodeParameters: NodeParameters)
                                         (implicit val hf: HF) extends VersionedAVLStorage[D] with ScorexLogging {

  private lazy val labelSize = nodeParameters.labelSize

  private val TopNodeKey: Array[Byte] = Array.fill(labelSize)(123: Byte)
  private val TopNodeHeight: Array[Byte] = Array.fill(labelSize)(124: Byte)

  private val fixedSizeValueMode = nodeParameters.valueSize.isDefined

  def restorePrunedProver(): Try[BatchAVLProver[D, HF]] = {
    restorePrunedTopNode().map {recoveredTop =>
      new BatchAVLProver(nodeParameters.keySize, nodeParameters.valueSize, Some(recoveredTop))(hf)
    }
  }

  private def restorePrunedTopNode(): Try[(ProverNodes[D], Int)] = Try {
    val top = VersionedLDBAVLStorage.fetch[D](ADKey @@ store.get(TopNodeKey).get)(hf, store, nodeParameters)
    val topHeight = Ints.fromByteArray(store.get(TopNodeHeight).get)

    top -> topHeight
  }

  override def rollback(version: ADDigest): Try[(ProverNodes[D], Int)] = Try {
    if (!this.version.contains(version)) { // do not rollback to self
      store.rollbackTo(version)
    }
  }.flatMap(_ => restorePrunedTopNode())
    .recoverWith { case e =>
      log.warn(s"Failed to recover tree for digest ${Base58.encode(version)}:", e)
      Failure(e)
    }

  override def version: Option[ADDigest] = store.lastVersionID.map(d => ADDigest @@ d)

  def rollbackVersions: Iterable[ADDigest] = store.rollbackVersions().map(d => ADDigest @@ d)

  def leafsIterator(): Iterator[(Array[Byte], Array[Byte])] =
    store.getWithFilter{ case (_, v) => v.head == LeafPrefix }

  override def update[K <: Array[Byte], V <: Array[Byte]](prover: BatchAVLProver[D, _],
                                                          additionalData: Seq[(K, V)]): Try[Unit] = {
    val digestWrapper = prover.digest
    val indexes = Seq(TopNodeKey -> nodeKey(prover.topNode), TopNodeHeight -> Ints.toByteArray(prover.rootNodeHeight))
    val toInsert = serializedVisitedNodes(prover.topNode, isTop = true)
    val toRemove = prover.removedNodes().map(rn => rn.label)
    val toUpdate = indexes ++ toInsert
    val toUpdateWithWrapped = toUpdate ++ additionalData

    store.update(digestWrapper, toRemove, toUpdateWithWrapped)
  }

  def update(manifest: BatchAVLProverManifest[D],
             chunks: Iterator[BatchAVLProverSubtree[D]],
             additionalData: Iterator[(Array[Byte], Array[Byte])]): Try[Unit] = {
    //todo: the function below copy-pasted from BatchAVLProver, eliminate boilerplate
    def digest(rootNode: Node[D], rootNodeHeight: Int): ADDigest = {
      assert(rootNodeHeight >= 0 && rootNodeHeight < 256)
      // rootNodeHeight should never be more than 255, so the toByte conversion is safe (though it may cause an incorrect
      // sign on the signed byte if rootHeight>127, but we handle that case correctly on decoding the byte back to int in the
      // verifier, by adding 256 if it's negative).
      // The reason rootNodeHeight should never be more than 255 is that if height is more than 255,
      // then the AVL tree has at least  2^{255/1.4405} = 2^177 leaves, which is more than the number of atoms on planet Earth.
      ADDigest @@ (rootNode.label :+ rootNodeHeight.toByte)
    }

    val rootNode = manifest.root
    val rootNodeHeight = manifest.rootHeight
    val digestWrapper = digest(rootNode, rootNodeHeight)
    val indexes = Iterator(TopNodeKey -> nodeKey(rootNode), TopNodeHeight -> Ints.toByteArray(rootNodeHeight))
    val nodesIterator = visitedNodesSerializer(manifest, chunks)

    store.update(digestWrapper, Nil, toUpdate = indexes ++ nodesIterator ++ additionalData)
  }

  private def visitedNodesSerializer(manifest: BatchAVLProverManifest[D],
                                     chunks: Iterator[BatchAVLProverSubtree[D]]) = {
    def idCollector(node: ProverNodes[D],
                    acc: Iterator[(Array[Byte], Array[Byte])]): Iterator[(Array[Byte], Array[Byte])] = {
      val pair: (Array[Byte], Array[Byte]) = (nodeKey(node), toBytes(node))
      node match {
        case n: ProxyInternalNode[D] if n.isEmpty =>
          acc ++ Iterator(pair)
        case i : InternalProverNode[D] =>
          acc ++ Iterator(pair) ++ idCollector(i.left, acc) ++ idCollector(i.right, acc)
        case _: ProverLeaf[D] =>
          acc ++ Iterator(pair)
      }
    }

    idCollector(manifest.root, Iterator.empty) ++ chunks.flatMap(subtree => idCollector(subtree.subtreeTop, Iterator.empty))
  }

  private def serializedVisitedNodes(node: ProverNodes[D],
                                     isTop: Boolean): Array[(Array[Byte], Array[Byte])] = {
    // Should always serialize top node. It may not be new if it is the creation of the tree
    if (node.isNew || isTop) {
      val pair: (Array[Byte], Array[Byte]) = (nodeKey(node), toBytes(node))
      node match {
        case n: InternalProverNode[D] =>
          val leftSubtree = serializedVisitedNodes(n.left, isTop = false)
          val rightSubtree = serializedVisitedNodes(n.right, isTop = false)
          pair +: (leftSubtree ++ rightSubtree)
        case _: ProverLeaf[D] => Array(pair)
      }
    } else {
      Array.empty
    }
  }

  //TODO label or key???
  private def nodeKey(node: ProverNodes[D]): Array[Byte] = node.label

  private def toBytes(node: ProverNodes[D]): Array[Byte] = {
    val builder = new mutable.ArrayBuilder.ofByte;
    node match {
      case n: InternalProverNode[D] =>
        builder += InternalNodePrefix += n.balance ++= n.key ++= n.left.label ++= n.right.label
      case n: ProverLeaf[D] =>
        if (fixedSizeValueMode) {
          builder += LeafPrefix ++= n.key ++= n.value ++= n.nextLeafKey
        } else {
          builder += LeafPrefix ++= n.key ++= Ints.toByteArray(n.value.length) ++= n.value ++= n.nextLeafKey
        }
    }
    builder.result()
  }

  //todo: this method is not used, should be removed on next scrypto update?
  override def update(prover: BatchAVLProver[D, _]): Try[Unit] = update(prover, Nil)
}


object VersionedLDBAVLStorage {
  val InternalNodePrefix: Byte = 0: Byte
  val LeafPrefix: Byte = 1: Byte

  def fetch[D <: hash.Digest](key: ADKey)(implicit hf: CryptographicHash[D],
                                          store: LDBVersionedStore,
                                          nodeParameters: NodeParameters): ProverNodes[D] = {
    val bytes = store(key)
    lazy val keySize = nodeParameters.keySize
    lazy val labelSize = nodeParameters.labelSize

    bytes.head match {
      case InternalNodePrefix =>
        val balance = Balance @@ bytes.slice(1, 2).head
        val key = ADKey @@ bytes.slice(2, 2 + keySize)
        val leftKey = ADKey @@ bytes.slice(2 + keySize, 2 + keySize + labelSize)
        val rightKey = ADKey @@ bytes.slice(2 + keySize + labelSize, 2 + keySize + (2 * labelSize))

        val n = new ProxyInternalProverNode[D](key, leftKey, rightKey, balance)
        n.isNew = false
        n
      case LeafPrefix =>
        val key = ADKey @@ bytes.slice(1, 1 + keySize)
        val (value, nextLeafKey) = if (nodeParameters.valueSize.isDefined) {
          val valueSize = nodeParameters.valueSize.get
          val value = ADValue @@ bytes.slice(1 + keySize, 1 + keySize + valueSize)
          val nextLeafKey = ADKey @@ bytes.slice(1 + keySize + valueSize, 1 + (2 * keySize) + valueSize)
          value -> nextLeafKey
        } else {
          val valueSize = Ints.fromByteArray(bytes.slice(1 + keySize, 1 + keySize + 4))
          val value = ADValue @@ bytes.slice(1 + keySize + 4, 1 + keySize + 4 + valueSize)
          val nextLeafKey = ADKey @@ bytes.slice(1 + keySize + 4 + valueSize, 1 + (2 * keySize) + 4 + valueSize)
          value -> nextLeafKey
        }
        val l = new ProverLeaf[D](key, value, nextLeafKey)
        l.isNew = false
        l
    }
  }
}
