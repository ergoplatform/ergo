package org.ergoplatform.db

import akka.util.ByteString

sealed trait Change {
  val key: ByteString
}

object Change {

  final case class Insert(key: ByteString) extends Change

  final case class Delete(key: ByteString, value: ByteString) extends Change

  final case class Alter(key: ByteString, oldValue: ByteString, newValue: ByteString) extends Change

}
