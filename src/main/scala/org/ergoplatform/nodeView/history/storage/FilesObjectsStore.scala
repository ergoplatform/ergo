package org.ergoplatform.nodeView.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.LoggingUtils
import scorex.util.{ModifierId, ScorexLogging}

import scala.util.Try

class FilesObjectsStore(dir: String) extends ObjectsStore with ScorexLogging {

  override def get(id: ModifierId): Option[Array[Byte]] = Try {
    Files.readAllBytes(path(id))
  }.toOption

  override def put(m: ErgoPersistentModifier): Try[Unit] = Try {
    val p = path(m.id)
    p.toFile.createNewFile()
    Files.write(p, HistoryModifierSerializer.toBytes(m), StandardOpenOption.WRITE)
    log.trace(s"Modifier ${m.encodedId} saved to FilesObjectsStore")
  }

  override def delete(id: ModifierId): Try[Unit] = Try {
    Files.delete(path(id))
  }.recover { case t: Throwable =>
    log.debug(s"Unable to delete file: ${path(id)}, reason: ${LoggingUtils.getReasonMsg(t)}")
  }

  override def contains(id: ModifierId): Boolean = Files.exists(path(id))

  private def path(id: ModifierId) = Paths.get(dir + "/" + id)
}
