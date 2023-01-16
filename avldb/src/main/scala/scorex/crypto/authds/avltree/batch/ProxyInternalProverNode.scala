package scorex.crypto.authds.avltree.batch

import scorex.crypto.authds.{ADKey, Balance}
import scorex.crypto.hash.{CryptographicHash, Digest}
import scorex.db.LDBVersionedStore
import InternalNode.InternalNodePrefix

/**
  * Internal node where children are not provided during node construction, only pointers to them,
  * and then children nodes are read from database and constructed only when requested (and children internal nodes are
  * of the same type). It allows for lazy loading of a tree.
  *
  */
class ProxyInternalProverNode[D <: Digest](protected var pk: ADKey,
                                           val leftLabel: ADKey,
                                           val rightLabel: ADKey,
                                           protected var pb: Balance = Balance @@ 0.toByte)
                                          (implicit val phf: CryptographicHash[D],
                                           store: LDBVersionedStore,
                                           nodeParameters: NodeParameters)
  extends InternalProverNode(k = pk, l = null, r = null, b = pb)(phf) {

  override protected def computeLabel: D = {
    hf.hash(Array(InternalNodePrefix, b), leftLabel, rightLabel)
  }

  override def left: ProverNodes[D] = {
    if (l == null) {
      l = VersionedLDBAVLStorage.fetch[D](leftLabel)
    }
    l
  }

  override def right: ProverNodes[D] = {
    if (r == null) {
      r = VersionedLDBAVLStorage.fetch[D](rightLabel)
    }
    r
  }

}
