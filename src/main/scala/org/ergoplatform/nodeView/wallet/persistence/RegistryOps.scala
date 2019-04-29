package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.nodeView.wallet.persistence.RegistryOpA.RegistryOp

object RegistryOps {

  implicit def transact[A](ma: RegistryOp[A])
                          (version: Array[Byte], registry: WalletRegistry): A = {
    ma.foldMap(registry.interpreter).run((Seq.empty, Seq.empty)).value match {
      case ((toInsert, toRemove), out: A) if toInsert.nonEmpty || toRemove.nonEmpty =>
        registry.store.update(
          ByteArrayWrapper(version),
          toRemove.map(ByteArrayWrapper.apply),
          toInsert.map(x => ByteArrayWrapper(x._1) -> ByteArrayWrapper(x._2))
        )
        out
      case (_, out: A) =>
        out
    }
  }

}
