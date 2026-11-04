package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import org.ergoplatform.serialization.ErgoSerializer
import scorex.crypto.authds.{ADKey, ADValue, Balance}
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, hashFn}
import scorex.crypto.authds.avltree.batch.serialization.ProxyInternalNode
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.serialization.{Reader, Writer}

/**
  * ErgoSerializer based prover nodes serializer (alternative to one provided in scrypto which is doing much more
  * allocations)
  *
  * @param store - store which could be provided to fetch children of a node when a child is requested
  */
class ProverNodeSerializer(store: LDBVersionedStore) extends ErgoSerializer[ProverNodes[DigestType]] {

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
          new ProxyInternalNode[DigestType](key, Digest32 @@@ leftKey, Digest32 @@@ rightKey, balance)(hashFn)
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
