package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.ModifierId

import scala.util.Try

trait ObjectsStore {

  def get(id: ModifierId): Option[Array[Byte]]

  def put(id: ModifierId, data: Array[Byte]): Try[Unit]

  def delete(id: ModifierId): Try[Unit]

  def put(m: ErgoPersistentModifier): Try[Unit] = put(m.id, m.bytes)

}
