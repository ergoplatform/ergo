package org.ergoplatform.db

import java.io.File

import org.iq80.leveldb.{DBFactory, Options}
import scorex.util.ScorexLogging

object LDBFactory extends ScorexLogging {

  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val loaders = List(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader)
    val factories = List(nativeFactory, javaFactory)
    val pairs = loaders.view
      .zip(factories)
      .flatMap { case (loader, factoryName) =>
        loadFactory(loader, factoryName).map(factoryName -> _)
      }

    val (name, factory) = pairs.headOption.getOrElse(
      throw new RuntimeException(s"Could not load any of the factory classes: $nativeFactory, $javaFactory"))

    if (name == javaFactory) {
      log.warn("Using the pure java LevelDB implementation which is still experimental")
    } else {
      log.info(s"Loaded $name with $factory")
    }

    factory
  }

  private def loadFactory(loader: ClassLoader, factoryName: String): Option[DBFactory] =
    try Some(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory])
    catch {
      case e: Throwable =>
        log.warn(s"Failed to load database factory $factoryName due to: $e")
        None
    }

  def createKvDb(path: String): LDBKVStore = {
    val dir = new File(path)
    dir.mkdirs()
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(dir, options)
    new LDBKVStore(db)
  }

}
