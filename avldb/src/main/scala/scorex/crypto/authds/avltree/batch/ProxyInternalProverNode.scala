package scorex.crypto.authds.avltree.batch

import scorex.crypto.authds.{ADKey, Balance}
import scorex.db.LDBVersionedStore
import InternalNode.InternalNodePrefix
import scorex.crypto.authds.avltree.batch.Constants.{DigestType, hashFn}

/**
  * Internal node where children are not provided during node construction, only pointers to them,
  * and then children nodes are read from database and constructed only when requested (and children internal nodes are
  * of the same type). It allows for lazy loading of a tree.
  *
  */
class ProxyInternalProverNode(protected var pk: ADKey,
                                           val leftLabel: ADKey,
                                           val rightLabel: ADKey,
                                           protected var pb: Balance = Balance @@ 0.toByte)
                                          (store: LDBVersionedStore)
  extends InternalProverNode(k = pk, l = null, r = null, b = pb)(hashFn) {

  override protected def computeLabel: DigestType = {
    hf.hash(Array(InternalNodePrefix, b), leftLabel, rightLabel)
  }

  override def left: ProverNodes[DigestType] = {
    if (l == null) {
      l = VersionedLDBAVLStorage.fetch(leftLabel)(store)
    }
    l
  }

  override def right: ProverNodes[DigestType] = {
    if (r == null) {
      r = VersionedLDBAVLStorage.fetch(rightLabel)(store)
    }
    r
  }

}
