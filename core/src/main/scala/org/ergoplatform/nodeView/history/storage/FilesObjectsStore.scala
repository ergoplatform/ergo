package org.ergoplatform.nodeView.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId

import scala.util.Try

class FilesObjectsStore(dir: String) extends ObjectsStore {

  override def get(id: ModifierId): Option[Array[Byte]] = Try {
    Files.readAllBytes(path(id))
  }.toOption

  override def put(m: ErgoPersistentModifier): Try[Unit] = Try {
    val p = path(m.id)
    p.toFile.createNewFile()
    Files.write(p, HistoryModifierSerializer.toBytes(m), StandardOpenOption.WRITE)
  }

  override def delete(id: ModifierId): Try[Unit] = Try {
    Files.delete(path(id))
  }

  private def path(id: ModifierId) = Paths.get(dir + "/" + Algos.encode(id))
}
