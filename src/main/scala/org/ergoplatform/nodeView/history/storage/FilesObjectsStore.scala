package org.ergoplatform.nodeView.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.Executors

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class FilesObjectsStore(dir: String) extends ObjectsStore {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  override def get(id: ModifierId): Option[Array[Byte]] = Try {
    Files.readAllBytes(path(id))
  }.toOption

  override def put(m: ErgoPersistentModifier): Unit = Future {
    val p = path(m.id)
    p.toFile.createNewFile()
    Files.write(p, HistoryModifierSerializer.toBytes(m), StandardOpenOption.WRITE)
  }

  override def delete(id: ModifierId): Unit = Future {
    Files.delete(path(id))
  }

  override def contains(id: ModifierId): Boolean = Files.exists(path(id))

  private def path(id: ModifierId) = Paths.get(dir + "/" + Algos.encode(id))
}
