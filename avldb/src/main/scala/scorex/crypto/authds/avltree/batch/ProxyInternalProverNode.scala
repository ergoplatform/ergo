package scorex.crypto.authds.avltree.batch

import io.iohk.iodb.Store
import scorex.crypto.authds.{ADKey, Balance}
import scorex.crypto.hash.{CryptographicHash, Digest}


class ProxyInternalProverNode[D <: Digest](protected var pk: ADKey,
                                           val lkey: ADKey,
                                           val rkey: ADKey,
                                           protected var pb: Balance = Balance @@ 0.toByte)
                                          (implicit val phf: CryptographicHash[D],
                                           store: Store,
                                           nodeParameters: NodeParameters)
  extends InternalProverNode(k = pk, l = null, r = null, b = pb)(phf) {

  override def left: ProverNodes[D] = {
    if (l == null) l = VersionedIODBAVLStorage.fetch[D](lkey)
    l
  }

  override def right: ProverNodes[D] = {
    if (r == null) r = VersionedIODBAVLStorage.fetch[D](rkey)
    r
  }
}


