package scorex.db

import java.io.File
import java.nio.file.Path

import swaydb._
import swaydb.serializers.Default._

import scala.collection.mutable

object SWDBFactory {
  val map = new mutable.HashMap[Path, SWDBStoreReader]

  def create(file: File): SWDBStore = {
    val path = file.toPath
    synchronized {
      map.getOrElseUpdate(path, new SWDBStore(persistent.Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO](dir = path).get)).asInstanceOf[SWDBStore]
    }
  }

  def create(file: File, keepVersions: Int): SWDBVersionedStore = {
     synchronized {
      map.getOrElseUpdate(new File(file,  "main").toPath, new SWDBVersionedStore(file, keepVersions)).asInstanceOf[SWDBVersionedStore]
    }
  }

  def create(path: String):  SWDBStore = {
    create(new File(path))
  }

  def unlink(store: SWDBStoreReader): Unit = {
    synchronized {
      map.remove(store.db.path)
    }
  }
}
