package org.ergoplatform.nodeView.wallet.persistence

import cats.free.Free
import cats.free.Free.liftF
import org.ergoplatform.nodeView.wallet.persistence.KVTransactionA.{Get, KVTransaction, Put, Remove}

trait TransactionalStorageBackend {

  def transact[T](tn: KVTransactionA[T], versionOpt: Option[String]): Option[T]

  def put[T](key: String, value: T): KVTransaction[Unit] = liftF[KVTransactionA, Unit](Put(key, value))

  def get[T](key: String): KVTransaction[Option[T]] = liftF[KVTransactionA, Option[T]](Get(key))

  def remove(key: String): KVTransaction[Unit] = liftF[KVTransactionA, Unit](Remove(key))

  def update[T](key: String, updateF: T => T): KVTransaction[Unit] = get[T](key)
    .flatMap { _
      .map(v => put[T](key, updateF(v)))
      .getOrElse(Free.pure(()))
    }

}

object TransactionalStorageBackend {

  final case class BatchUpdate(remove: Iterable[String], insert: Iterable[(String, Array[Byte])])

}
