package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import scorex.core.serialization.{ManifestSerializer, ScorexSerializer}
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, HashFnType, hashFn}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, Balance}
import scorex.util.encode.Base58
import scorex.crypto.hash
import scorex.crypto.hash.Digest32
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}

import scala.collection.mutable
import scala.util.{Failure, Try}

/**
  * Persistent versioned authenticated AVL+ tree implementation on top of versioned LevelDB storage
  *
  * @param store - level db storage to save the tree in
  */
class VersionedLDBAVLStorage(store: LDBVersionedStore)
  extends VersionedAVLStorage[DigestType] with ScorexLogging {

  import VersionedLDBAVLStorage.{nodeLabel, topNodeKeys}

  private val topKeys = topNodeKeys()
  private val topNodeHashKey: Array[Byte] = topKeys._1
  private val topNodeHeightKey: Array[Byte] = topKeys._2

  private def restorePrunedTopNode(): Try[(ProverNodes[DigestType], Int)] = Try {
    val top = VersionedLDBAVLStorage.fetch(ADKey @@ store.get(topNodeHashKey).get)(store)
    val topHeight = Ints.fromByteArray(store.get(topNodeHeightKey).get)

    top -> topHeight
  }

  /**
   * Restore prover's tree from database with just pruned top node (so only one node is fetched)
   */
  def restorePrunedProver(): Try[BatchAVLProver[DigestType, HashFnType]] = {
    restorePrunedTopNode().map { recoveredTop =>
      new BatchAVLProver(StateTreeParameters.keySize, StateTreeParameters.valueSize, Some(recoveredTop))(hashFn)
    }
  }

  override def rollback(version: ADDigest): Try[(ProverNodes[DigestType], Int)] = Try {
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

  override def update[K <: Array[Byte], V <: Array[Byte]](prover: BatchAVLProver[DigestType, _],
                                                          additionalData: Seq[(K, V)]): Try[Unit] = {
    val digestWrapper = prover.digest
    val indexes = Seq(topNodeHashKey -> nodeLabel(prover.topNode),
                      topNodeHeightKey -> Ints.toByteArray(prover.rootNodeHeight))
    val toInsert = serializedVisitedNodes(prover.topNode, isTop = true)
    val toRemove = prover.removedNodes().map(rn => rn.label)
    val toUpdate = indexes ++ toInsert
    val toUpdateWithWrapped = toUpdate ++ additionalData

    store.update(digestWrapper, toRemove, toUpdateWithWrapped)
  }

  //todo: optimize the method
  private def serializedVisitedNodes(node: ProverNodes[DigestType],
                                     isTop: Boolean): Array[(Array[Byte], Array[Byte])] = {
    // Should always serialize top node. It may not be new if it is the creation of the tree
    if (node.isNew || isTop) {
      val pair: (Array[Byte], Array[Byte]) = (nodeLabel(node), VersionedLDBAVLStorage.noStoreSerializer.toBytes(node))
      node match {
        case n: InternalProverNode[DigestType] =>
          val leftSubtree = serializedVisitedNodes(n.left, isTop = false)
          val rightSubtree = serializedVisitedNodes(n.right, isTop = false)
          pair +: (leftSubtree ++ rightSubtree)
        case _: ProverLeaf[DigestType] => Array(pair)
      }
    } else {
      Array.empty
    }
  }

  //todo: this method is not used, should be removed on next scrypto update?
  override def update(prover: BatchAVLProver[DigestType, _]): Try[Unit] = update(prover, Nil)

  def dumpSnapshot(dumpStorage: LDBKVStore,
                   manifestDepth: Int): Array[Byte] = {
    store.backup { ri =>

      def subtreeLoop(label: DigestType, builder: mutable.ArrayBuilder[Byte]): Unit = {
        val nodeBytes = ri.get(label)
        builder ++= nodeBytes
        val node = VersionedLDBAVLStorage.noStoreSerializer.parseBytes(nodeBytes)
        node match {
          case in: ProxyInternalNode[DigestType] =>
            subtreeLoop(Digest32 @@ in.leftLabel, builder)
            subtreeLoop(Digest32 @@ in.rightLabel, builder)
          case _ =>
        }
      }

      def dumpSubtree(sid: DigestType): Try[Unit] = {
        val builder = mutable.ArrayBuilder.make[Byte]()
        builder.sizeHint(200000)
        subtreeLoop(sid, builder)
        dumpStorage.insert(sid, builder.result())
      }

      def manifestLoop(nodeDbKey: Array[Byte], level: Int, manifestBuilder: mutable.ArrayBuilder[Byte]): Unit = {
        val nodeBytes = ri.get(nodeDbKey)
        manifestBuilder ++= nodeBytes
        val node = VersionedLDBAVLStorage.noStoreSerializer.parseBytes(nodeBytes)
        node match {
          case in: ProxyInternalNode[DigestType] if level == manifestDepth =>
            dumpSubtree(Digest32 @@ in.leftLabel)
            dumpSubtree(Digest32 @@ in.rightLabel)
          case in: ProxyInternalNode[DigestType] =>
            manifestLoop(in.leftLabel, level + 1, manifestBuilder)
            manifestLoop(in.rightLabel, level + 1, manifestBuilder)
          case _ =>
            //todo: support leafs
            println("!!!")
        }
      }

      val rootNodeLabel = ri.get(topNodeHashKey)
      val rootNodeHeight = Ints.fromByteArray(ri.get(topNodeHeightKey))

      val manifestBuilder = mutable.ArrayBuilder.make[Byte]()
      manifestBuilder.sizeHint(200000)
      manifestBuilder ++= Ints.toByteArray(rootNodeHeight)
      manifestBuilder += ManifestSerializer.ManifestDepth

      manifestLoop(rootNodeLabel, level = 1, manifestBuilder)
      val manifestBytes = manifestBuilder.result()
      dumpStorage.insert(rootNodeLabel, manifestBytes)

      rootNodeLabel
    }
  }
}


object VersionedLDBAVLStorage {
  // prefixes used to encode node type (internal or leaf) in database
  private[batch] val InternalNodePrefix: Byte = 0: Byte
  private[batch] val LeafPrefix: Byte = 1: Byte

  val noStoreSerializer = new ProverNodeSerializer(null)

  private[batch] def topNodeKeys(): (Array[Byte], Array[Byte]) = {
    val topNodeKey: Array[Byte] = Array.fill(StateTreeParameters.labelSize)(123: Byte)
    val topNodeHeightKey: Array[Byte] = Array.fill(StateTreeParameters.labelSize)(124: Byte)
    (topNodeKey, topNodeHeightKey)
  }

  /**
    * Fetch tree node from database by its database id (hash of node contents)
    *
    * @param dbKey - database key node of interest is stored under (hash of the node)
    * @param store - database
    * @return node read from the database (or throws exception if there is no such node), in case of internal node it
    *         returns pruned internal node, so a node where left and right children not stored, only their hashes
    */
  def fetch(dbKey: ADKey)(store: LDBVersionedStore): ProverNodes[DigestType] = {
    val bytes = store(dbKey)
    val node = new ProverNodeSerializer(store).parseBytes(bytes)
    node.isNew = false
    node
  }

  /**
    * Node label (hash). Used as database key for node contents
    */
  private[batch] def nodeLabel(node: ProverNodes[DigestType]): Array[Byte] = node.label

  def recreate(manifest: BatchAVLProverManifest[DigestType],
                               chunks: Iterator[BatchAVLProverSubtree[DigestType]],
                               additionalData: Iterator[(Array[Byte], Array[Byte])],
                               store: LDBVersionedStore): Try[VersionedLDBAVLStorage] = {
    //todo: the function below copy-pasted from BatchAVLProver, eliminate boilerplate

    val (topNodeHashKey, topNodeHeightKey) = topNodeKeys()

    def idCollector(node: ProverNodes[DigestType],
                    acc: Iterator[(Array[Byte], Array[Byte])]): Iterator[(Array[Byte], Array[Byte])] = {
      val pair: (Array[Byte], Array[Byte]) = (nodeLabel(node), noStoreSerializer.toBytes(node))
      node match {
        case n: ProxyInternalNode[DigestType] if n.isEmpty =>
          acc ++ Iterator(pair)
        case i: InternalProverNode[DigestType] =>
          acc ++ Iterator(pair) ++ idCollector(i.left, acc) ++ idCollector(i.right, acc)
        case _: ProverLeaf[DigestType] =>
          acc ++ Iterator(pair)
      }
    }

    val rootNode = manifest.root
    val rootNodeHeight = manifest.rootHeight
    val digestWrapper = VersionedLDBAVLStorage.digest(rootNode.label, rootNodeHeight)
    val indices = Iterator(topNodeHashKey -> nodeLabel(rootNode), topNodeHeightKey -> Ints.toByteArray(rootNodeHeight))
    val nodesIterator = idCollector(manifest.root, Iterator.empty) ++
      chunks.flatMap(subtree => idCollector(subtree.subtreeTop, Iterator.empty))
    store.update(digestWrapper, toRemove = Nil, toUpdate = indices ++ nodesIterator ++ additionalData).map{_ =>
      new VersionedLDBAVLStorage(store)
    }
  }

  /**
    * Calculate tree digest, given root node label(hash) and root node height, by appending height to the hash
    */
  def digest[D <: hash.Digest](rootNodeLabel: D, rootNodeHeight: Int): ADDigest = {
    assert(rootNodeHeight >= 0 && rootNodeHeight < 256)
    // rootNodeHeight should never be more than 255, so the toByte conversion is safe (though it may cause an incorrect
    // sign on the signed byte if rootHeight>127, but we handle that case correctly on decoding the byte back to int in the
    // verifier, by adding 256 if it's negative).
    // The reason rootNodeHeight should never be more than 255 is that if height is more than 255,
    // then the AVL tree has at least  2^{255/1.4405} = 2^177 leaves, which is more than the number of atoms on planet Earth.
    ADDigest @@ (rootNodeLabel :+ rootNodeHeight.toByte)
  }

}

class ProverNodeSerializer(store: LDBVersionedStore) extends ScorexSerializer[ProverNodes[DigestType]] {

  import VersionedLDBAVLStorage.{InternalNodePrefix,LeafPrefix}

  override def serialize(node: ProverNodes[DigestType], w: Writer): Unit = {
    node match {
      case n: ProxyInternalNode[DigestType] =>
        w.put(InternalNodePrefix)
        w.put(n.balance)
        w.putBytes(n.key)
        w.putBytes(n.leftLabel)
        w.putBytes(n.rightLabel)
      case n: InternalProverNode[DigestType] =>
        w.put(InternalNodePrefix)
        w.put(n.balance)
        w.putBytes(n.key)
        w.putBytes(n.left.label)
        w.putBytes(n.right.label)
      case n: ProverLeaf[DigestType] =>
        w.put(LeafPrefix)
        w.putBytes(n.key)
        w.putBytes(Ints.toByteArray(n.value.length))
        w.putBytes(n.value)
        w.putBytes(n.nextLeafKey)
    }
  }

  override def parse(r: Reader): ProverNodes[DigestType] = {
    val prefix = r.getByte()
    prefix match {
      case InternalNodePrefix =>
        val balance = Balance @@ r.getByte()
        val key = ADKey @@ r.getBytes(StateTreeParameters.keySize)
        val leftKey = ADKey @@ r.getBytes(StateTreeParameters.labelSize)
        val rightKey = ADKey @@ r.getBytes(StateTreeParameters.labelSize)

        if (store != null) {
          new ProxyInternalProverNode(key, leftKey, rightKey, balance)(store)
        } else {
          new ProxyInternalNode[DigestType](key, Digest32 @@ leftKey, Digest32 @@ rightKey, balance)(hashFn)
        }
      case LeafPrefix =>
        val key = ADKey @@ r.getBytes(StateTreeParameters.keySize)
        val (value, nextLeafKey) =  {
          val valueSize = Ints.fromByteArray(r.getBytes(4))
          val value = ADValue @@ r.getBytes(valueSize)
          val nextLeafKey = ADKey @@ r.getBytes(StateTreeParameters.keySize)
          value -> nextLeafKey
        }
        new ProverLeaf[DigestType](key, value, nextLeafKey)(hashFn)
    }
  }

}
