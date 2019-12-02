package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.crypto.authds.avltree.batch.VersionedIODBAVLStorage.{InternalNodePrefix, LeafPrefix}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, Balance}
import scorex.util.encode.Base58
import scorex.crypto.hash
import scorex.crypto.hash.{CryptographicHash, Digest}
import scorex.util.ScorexLogging
import scorex.utils.ScorexEncoding

import scala.util.{Failure, Try}

class VersionedIODBAVLStorage[D <: Digest](store: Store, nodeParameters: NodeParameters)
                                          (implicit val hf: CryptographicHash[D]) extends VersionedAVLStorage[D] with ScorexLogging {

  private lazy val labelSize = nodeParameters.labelSize

  private val TopNodeKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(labelSize)(123: Byte))
  private val TopNodeHeight: ByteArrayWrapper = ByteArrayWrapper(Array.fill(labelSize)(124: Byte))
  private val DigestLength = 33
  private val InitialVersion = ADDigest @@ Array.fill(DigestLength)(11: Byte)
  private val fixedSizeValueMode = nodeParameters.valueSize.isDefined

  override def update(prover: BatchAVLProver[D, _]): Try[Unit] = update(prover, Seq())

  override def rollback(version: ADDigest): Try[(ProverNodes[D], Int)] = Try {
    store.rollback(ByteArrayWrapper(version))

    val top = VersionedIODBAVLStorage.fetch[D](ADKey @@ store.get(TopNodeKey).get.data)(hf, store, nodeParameters)
    val topHeight = Ints.fromByteArray(store.get(TopNodeHeight).get.data)

    top -> topHeight
  }.recoverWith { case e =>
    log.warn(s"Failed to recover tree for digest ${Base58.encode(version)}:", e)
    Failure(e)
  }

  override def version: Option[ADDigest] = store.lastVersionID.map(d => ADDigest @@ d.data)

  def rollbackVersions: Iterable[ADDigest] = store.rollbackVersions().map(d => ADDigest @@ d.data)

  def leafsIterator() = store.getAll().filter { case (_, v) => v.data.head == LeafPrefix }

  override def update[K <: Array[Byte], V <: Array[Byte]](prover: BatchAVLProver[D, _],
                                                          additionalData: Seq[(K, V)]): Try[Unit] = Try {
    val digestWrapper = ByteArrayWrapper(prover.digest)
    val indexes = Seq(TopNodeKey -> nodeKey(prover.topNode),
      TopNodeHeight -> ByteArrayWrapper(Ints.toByteArray(prover.rootNodeHeight)))
    val toInsert = serializedVisitedNodes(prover.topNode, isTop = true)
    val toRemove = prover.removedNodes().map(rn => ByteArrayWrapper(rn.label))
    val toUpdate = indexes ++ toInsert
    val toUpdateWrapped = additionalData.map { case (k, v) =>
      ByteArrayWrapper(k) -> ByteArrayWrapper(v)
    }
    val toUpdateWithWrapped = toUpdate ++ toUpdateWrapped

    //log.info(s"Update storage to version $digestWrapper: ${toUpdateWithWrapped.size} elements to insert," +
    //  s" ${toRemove.size} elements to remove")

    store.update(digestWrapper, toRemove, toUpdateWithWrapped)
  }.recoverWith { case e =>
    log.warn("Failed to update tree", e)
    Failure(e)
  }

  private def serializedVisitedNodes(node: ProverNodes[D],
                                     isTop: Boolean): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    // Should always serialize top node. It may not be new if it is the creation of the tree
    if (node.isNew || isTop) {
      val pair: (ByteArrayWrapper, ByteArrayWrapper) = (nodeKey(node), ByteArrayWrapper(toBytes(node)))
      node match {
        case n: InternalProverNode[D] =>
          val leftSubtree = serializedVisitedNodes(n.left, isTop = false)
          val rightSubtree = serializedVisitedNodes(n.right, isTop = false)
          pair +: (leftSubtree ++ rightSubtree)
        case _: ProverLeaf[D] => Seq(pair)
      }
    } else Seq()
  }

  //TODO label or key???
  private def nodeKey(node: ProverNodes[D]): ByteArrayWrapper = ByteArrayWrapper(node.label)

  private def toBytes(node: ProverNodes[D]): Array[Byte] = node match {
    case n: InternalProverNode[D] => InternalNodePrefix +: n.balance +: (n.key ++ n.left.label ++ n.right.label)
    case n: ProverLeaf[D] =>
      if (fixedSizeValueMode) LeafPrefix +: (n.key ++ n.value ++ n.nextLeafKey)
      else LeafPrefix +: (n.key ++ Ints.toByteArray(n.value.length) ++ n.value ++ n.nextLeafKey)
  }
}


object VersionedIODBAVLStorage {
  val InternalNodePrefix: Byte = 0: Byte
  val LeafPrefix: Byte = 1: Byte

  def fetch[D <: hash.Digest](key: ADKey)(implicit hf: CryptographicHash[D],
                                          store: Store,
                                          nodeParameters: NodeParameters): ProverNodes[D] = {
    val bytes = store(ByteArrayWrapper(key)).data
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