package scorex.db

import java.io.File

import org.iq80.leveldb.Options
import scorex.db.LDBFactory.factory

import scala.collection.mutable

case class Profile(var nRecords : Long, var keySize : Long, var valueSize : Long)

object LDBProfile {
  def main(args: Array[String]): Unit = {
    var dir: String = "."
    var groupBy = false
    for (arg <- args) {
      if (arg.startsWith("-")) {
        if (arg.equals("-g")) {
          groupBy = true
        } else {
          Console.err.println(s"Unknow option: ${arg}")
        }
      } else {
        dir = arg
      }
    }
    val options = new Options()
    options.createIfMissing(false)
    val db = factory.open(new File(dir), options)
    var nRecords : Long = 0
    var keySize : Long = 0
    var valueSize : Long = 0
    val groups = mutable.HashMap.empty[Byte,Profile]
    val iter = db.iterator()
    iter.seekToFirst()
    while (iter.hasNext) {
      val entry = iter.next()
      val k = entry.getKey
      val v = entry.getValue
      nRecords += 1
      keySize += k.size
      valueSize += v.size
      if (groupBy) {
        val p = groups.getOrElseUpdate(v.head, new Profile(0,0,0))
        p.nRecords += 1
        p.keySize += k.size
        p.valueSize += v.size
      }
    }
    Console.println(s"Total records=${nRecords}, key size=${keySize}, value size=${valueSize}")
    if (groupBy) {
      for ((k,v) <- groups) {
        Console.println(s"Group ${k}: count=${v.nRecords}, key size=${v.keySize}, value size=${v.valueSize}")
      }
    }
  }
}
