package org.ergoplatform.nodeView.wallet.persistence

import cats.data.State
import cats.~>
import io.iohk.iodb.Store
import org.ergoplatform.nodeView.wallet.persistence.KVTransactionA._
import org.ergoplatform.nodeView.wallet.persistence.TransactionalStorageBackend.BatchUpdate

final class IODBStorageBackend(store: Store) extends TransactionalStorageBackend {

  type KVStoreState[A] = State[BatchUpdate, A]

  override def transact[T](tn: KVTransactionA[T], versionOpt: Option[String] = None): Option[T] = ???

  private def interpreter: KVTransactionA ~> KVStoreState = new (KVTransactionA ~> KVStoreState) {
    override def apply[A](fa: KVTransactionA[A]): KVStoreState[A] = ???
  }

}
