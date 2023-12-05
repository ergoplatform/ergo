package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import org.ergoplatform.serialization.ManifestSerializer.MainnetManifestDepth
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, HashFnType, hashFn}
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage.{noStoreSerializer, topNodeHashKey, topNodeHeightKey}
import scorex.crypto.authds.avltree.batch.serialization.{BatchAVLProverManifest, BatchAVLProverSubtree, ProxyInternalNode}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.util.encode.Base16
import scorex.crypto.hash
import scorex.crypto.hash.Digest32
import scorex.db.{LDBKVStore, LDBVersionedStore}
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.util.{Failure, Try}

/**
  * Persistent versioned authenticated AVL+ tree implementation on top of versioned LevelDB storage
  *
  * @param store - level db storage to save the tree in
  */
class VersionedLDBAVLStorage(store: LDBVersionedStore)
  extends VersionedAVLStorage[DigestType] with ScorexLogging {

  import VersionedLDBAVLStorage.nodeLabel

  private def restorePrunedRootNode(): Try[(ProverNodes[DigestType], Int)] = Try {
    val rootNode = VersionedLDBAVLStorage.fetch(ADKey @@ store.get(topNodeHashKey).get)(store)
    val rootHeight = Ints.fromByteArray(store.get(topNodeHeightKey).get)

    rootNode -> rootHeight
  }

  /**
    * Restore prover's tree from database with just pruned top node (so only one node is fetched)
    */
  def restorePrunedProver(): Try[BatchAVLProver[DigestType, HashFnType]] = {
    restorePrunedRootNode().map { recoveredTop =>
      new BatchAVLProver(StateTreeParameters.keySize, StateTreeParameters.valueSize, Some(recoveredTop))(hashFn)
    }
  }

  override def rollback(version: ADDigest): Try[(ProverNodes[DigestType], Int)] = Try {
    if (!this.version.contains(version)) { // do not rollback to self
      log.info(s"Doing rollback from ${this.version.map(Base16.encode)} to ${Base16.encode(version)}:")
      store.rollbackTo(version)
    }
  }.flatMap(_ => restorePrunedRootNode())
    .recoverWith { case e =>
      log.warn(s"Failed to recover tree for digest ${Base16.encode(version)}:", e)
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

  /**
    * Dump current state of AVL+ tree into another (non-versioned) storage. Tree is dumped in
    * (manifest, subtrees) format
    *
    * @param dumpStorage   - non-versioned storage to dump tree to
    * @param manifestDepth - depth of manifest tree
    * @param expectedRootHash - expected UTXO set authenticating tree root hash
    * @return - hash of root node of tree, or failure if an error (e.g. in database) happened
    */
  def dumpSnapshot(dumpStorage: LDBKVStore, manifestDepth: Byte, expectedRootHash: Array[Byte]): Try[Array[Byte]] = {
    store.processSnapshot { dbReader =>

      def subtreeLoop(label: DigestType, builder: mutable.ArrayBuilder[Byte]): Unit = {
        val nodeBytes = dbReader.get(label)
        builder ++= nodeBytes
        val node = noStoreSerializer.parseBytes(nodeBytes)
        node match {
          case in: ProxyInternalNode[DigestType] =>
            subtreeLoop(Digest32 @@@ in.leftLabel, builder)
            subtreeLoop(Digest32 @@@ in.rightLabel, builder)
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
        val nodeBytes = dbReader.get(nodeDbKey)
        manifestBuilder ++= nodeBytes
        val node = noStoreSerializer.parseBytes(nodeBytes)
        node match {
          case in: ProxyInternalNode[DigestType] if level == manifestDepth =>
            dumpSubtree(Digest32 @@@ in.leftLabel)
            dumpSubtree(Digest32 @@@ in.rightLabel)
          case in: ProxyInternalNode[DigestType] =>
            manifestLoop(in.leftLabel, level + 1, manifestBuilder)
            manifestLoop(in.rightLabel, level + 1, manifestBuilder)
          case _ =>
          // do nothing for a leaf
        }
      }

      val rootNodeLabel = dbReader.get(topNodeHashKey)
      val rootNodeHeight = Ints.fromByteArray(dbReader.get(topNodeHeightKey)).toByte

      require(rootNodeLabel.sameElements(expectedRootHash), "Root node hash changed")

      val manifestBuilder = mutable.ArrayBuilder.make[Byte]()
      manifestBuilder.sizeHint(200000)
      manifestBuilder += rootNodeHeight
      manifestBuilder += manifestDepth

      manifestLoop(rootNodeLabel, level = 1, manifestBuilder)
      val manifestBytes = manifestBuilder.result()
      dumpStorage.insert(rootNodeLabel, manifestBytes)

      rootNodeLabel
    }
  }

  def iterateAVLTree(fromIndex: Int)(handleSubtree: BatchAVLProverSubtree[DigestType] => Unit): Unit =
    store.processSnapshot { dbReader =>

      var current: Int = 0

      def subtree(sid: Array[Byte]): BatchAVLProverSubtree[DigestType] = {
        def loop(label: Array[Byte]): ProverNodes[DigestType] =
          (noStoreSerializer.parseBytes(dbReader.get(label)): @unchecked) match {
            case leaf: ProverLeaf[DigestType] => leaf
            case i: ProxyInternalNode[DigestType] =>
              i.getNew(loop(i.leftLabel), loop(i.rightLabel))
          }
        new BatchAVLProverSubtree[DigestType](loop(sid))
      }

      def proxyLoop(label: Array[Byte], level: Int): Unit =
        noStoreSerializer.parseBytes(dbReader.get(label)) match {
          case in: ProxyInternalNode[DigestType] if level == MainnetManifestDepth =>
            if(current >= fromIndex) handleSubtree(subtree(in.leftLabel))
            current += 1
            if(current >= fromIndex) handleSubtree(subtree(in.rightLabel))
            current += 1
          case in: ProxyInternalNode[DigestType] =>
            proxyLoop(in.leftLabel, level + 1)
            proxyLoop(in.rightLabel, level + 1)
          case _ =>
        }

      proxyLoop(dbReader.get(topNodeHashKey), 1)

    }
}


object VersionedLDBAVLStorage {

  private[batch] val topNodeHashKey: Array[Byte] = Array.fill(StateTreeParameters.labelSize)(123: Byte)
  private[batch] val topNodeHeightKey: Array[Byte] = Array.fill(StateTreeParameters.labelSize)(124: Byte)

  // prefixes used to encode node type (internal or leaf) in database
  private[batch] val InternalNodePrefix: Byte = 0: Byte
  private[batch] val LeafPrefix: Byte = 1: Byte

  /**
    * Node serializer which is restoring nodes with only children labels stored in, without possibility to restore
    * children automatically when requested
    */
  val noStoreSerializer = new ProverNodeSerializer(store = null)

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

  /**
    * Reconstruct versioned AVL+ tree storage from dump
    * @param manifest - tree started from root node cut at fixed depth
    * @param chunks - iterator (lazy sequence) of subtrees under manifest tree
    * @param additionalData - additional fields (metadata) to write into database
    * @param store - db to store restored tree to
    * @return db with reconstructed tree written to, or error
    */
  def recreate(manifest: BatchAVLProverManifest[DigestType],
               chunks: Iterator[BatchAVLProverSubtree[DigestType]],
               additionalData: Iterator[(Array[Byte], Array[Byte])],
               store: LDBVersionedStore): Try[VersionedLDBAVLStorage] = {
    //todo: the function below copy-pasted from BatchAVLProver, eliminate boilerplate?

    def idCollector(node: ProverNodes[DigestType]): Iterator[(Array[Byte], Array[Byte])] = {
      val pair: (Array[Byte], Array[Byte]) = (nodeLabel(node), noStoreSerializer.toBytes(node))
      node match {
        case n: ProxyInternalNode[DigestType] if n.isEmpty =>
          Iterator(pair)
        case i: InternalProverNode[DigestType] =>
          Iterator(pair) ++ idCollector(i.left) ++ idCollector(i.right)
        case _: ProverLeaf[DigestType] =>
          Iterator(pair)
      }
    }

    val rootNode = manifest.root
    val rootNodeHeight = manifest.rootHeight
    val digestWrapper = VersionedLDBAVLStorage.digest(rootNode.label, rootNodeHeight)
    val indices = Iterator(topNodeHashKey -> nodeLabel(rootNode), topNodeHeightKey -> Ints.toByteArray(rootNodeHeight))
    val nodesIterator = idCollector(manifest.root) ++
      chunks.flatMap(subtree => idCollector(subtree.subtreeTop))
    store.update(digestWrapper, toRemove = Nil, toUpdate = indices ++ nodesIterator ++ additionalData).map { _ =>
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

  /**
    * splits 33-byte digest into 32-bytes hash and 1-byte tree height
    */
  def splitDigest(digest: ADDigest): (Array[Byte], Byte) = {
    digest.dropRight(1) -> digest.last
  }

}
