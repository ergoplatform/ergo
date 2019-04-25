package org.ergoplatform.nodeView.wallet.persistence

import cats.free.Free

sealed trait KVTransactionA[A]

object KVTransactionA {

  type KVTransaction[A] = Free[KVTransactionA, A]

  final case class Put[T](key: String, value: T) extends KVTransactionA[Unit]

  final case class Get[T](key: String) extends KVTransactionA[Option[T]]

  final case class Remove(key: String) extends KVTransactionA[Unit]

}
