package org.ergoplatform.nodeView.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}

import org.ergoplatform.settings.Algos
import scorex.core.ModifierId

import scala.util.Try

class FilesObjectsStore(dir: String) extends ObjectsStore {

  def get(id: ModifierId): Option[Array[Byte]] = Try {
    Files.readAllBytes(path(id))
  }.toOption

  def put(id: ModifierId, data: Array[Byte]): Try[Unit] = Try {
    Files.write(path(id), data, StandardOpenOption.WRITE)
  }

  def delete(id: ModifierId): Try[Unit] =  Try {
    Files.delete(path(id))
  }

  private def path(id: ModifierId) = Paths.get(dir + "/" + Algos.encode(id))
}
