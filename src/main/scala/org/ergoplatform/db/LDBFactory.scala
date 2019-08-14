package org.ergoplatform.db

import org.iq80.leveldb.DBFactory
import scorex.util.ScorexLogging

import scala.util.Try

object LDBFactory extends ScorexLogging {

  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val pairs = for {
      loader <- List(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader).view
      factoryName <- List(nativeFactory, javaFactory)
      factory <- Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory]).toOption
    } yield (factoryName, factory)

    val (name, factory) = pairs.headOption.getOrElse(
      throw new RuntimeException(s"Could not load any of the factory classes: $nativeFactory, $javaFactory"))

    if (name == javaFactory) {
      log.warn("Using the pure java LevelDB implementation which is still experimental")
    } else {
      log.info(s"Loaded $name with $factory")
    }

    factory
  }

}
