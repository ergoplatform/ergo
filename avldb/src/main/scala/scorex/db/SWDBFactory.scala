package scorex.db

import java.io.File

import swaydb._
import swaydb.serializers.Default._

object SWDBFactory {
  def create(file: File): SWDBStore = {
    new SWDBStore(persistent.Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO](dir = file.toPath).get)
  }
  def create(path: String):  SWDBStore = {
    new SWDBStore(persistent.Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO](dir = path).get)
  }
}
