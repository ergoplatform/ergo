package org.ergoplatform.db

import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.utils.FileUtils

import scala.util.Try

trait DBSpec extends PropSpec with Matchers with FileUtils {

  import LDBFactory.factory

  protected def withDb(body: DB => Unit): Unit = {
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(createTempDir, options)
    Try(body(db)).recoverWith {
      case e =>
        db.close()
        throw e
    }
  }

}
