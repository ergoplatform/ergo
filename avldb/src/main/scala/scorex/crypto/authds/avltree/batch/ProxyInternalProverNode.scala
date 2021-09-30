package scorex.crypto.authds.avltree.batch


import scorex.crypto.authds.{ADKey, Balance}
import scorex.crypto.hash.{CryptographicHash, Digest}
import scorex.db.SWDBVersionedStore


class ProxyInternalProverNode[D <: Digest](protected var pk: ADKey,
                                           val lkey: ADKey,
                                           val rkey: ADKey,
                                           protected var pb: Balance = Balance @@ 0.toByte)
                                          (implicit val phf: CryptographicHash[D],
                                           store: SWDBVersionedStore,
                                           nodeParameters: NodeParameters)
  extends InternalProverNode(k = pk, l = null, r = null, b = pb)(phf) {

  override def left: ProverNodes[D] = {
    if (l == null) l = VersionedSWDBAVLStorage.fetch[D](lkey)
    l
  }

  override def right: ProverNodes[D] = {
    if (r == null) r = VersionedSWDBAVLStorage.fetch[D](rkey)
    r
  }
}


