package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.ModifierId

import scala.util.Try

trait ObjectsStore {

  def get(id: ModifierId): Option[Array[Byte]]

  def delete(id: ModifierId): Unit

  def put(m: ErgoPersistentModifier): Unit

  def contains(id: ModifierId): Boolean

}
