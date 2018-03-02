package org.ergoplatform.nodeView.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.LimitedCache
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

class FilesObjectsStore(dir: String) extends ObjectsStore with ScorexLogging {

  private val cache = new LimitedCache[ErgoPersistentModifier](100)

  override def get(id: ModifierId): Option[ErgoPersistentModifier] = cache.get(id).orElse {
    Try {
      Files.readAllBytes(path(id))
    }.toOption.flatMap { bBytes =>
      HistoryModifierSerializer.parseBytes(bBytes) match {
        case Success(b) =>
          Some(b)
        case Failure(e) =>
          log.warn(s"Failed to parse block from db (bytes are: ${bBytes.mkString("-")}): ", e)
          None
      }
    }
  }


  override def put(m: ErgoPersistentModifier): Try[Unit] = Try {
    val p = path(m.id)
    p.toFile.createNewFile()
    Files.write(p, HistoryModifierSerializer.toBytes(m), StandardOpenOption.WRITE)
    cache.put(m.id, m)
  }

  override def delete(id: ModifierId): Try[Unit] = Try {
    cache.delete(id)
    Files.delete(path(id))
  }

  private def path(id: ModifierId) = Paths.get(dir + "/" + Algos.encode(id))
}
