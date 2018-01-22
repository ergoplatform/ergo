package org.ergoplatform.nodeView.history.storage

import scorex.core.ModifierId

import scala.util.Try

trait ObjectsStore {

  def get(id: ModifierId): Option[Array[Byte]]

  def put(id: ModifierId, data: Array[Byte]): Try[Unit]

  def delete(id: ModifierId): Try[Unit]
}
