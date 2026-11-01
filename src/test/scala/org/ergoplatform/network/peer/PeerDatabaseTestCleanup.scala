package org.ergoplatform.network.peer

import scorex.db.{LDBFactory, StoreRegistry}

import java.io.File

/** Call only after every actor using this fixture-owned peer directory has stopped. */
object PeerDatabaseTestCleanup {
  def closeOwned(peerDirectory: File): Unit = {
    val registry = LDBFactory.factory match {
      case value: StoreRegistry => value
      case _ => throw new IllegalStateException("Expected the registered LevelDB factory")
    }
    val readLock = registry.lock.readLock()
    readLock.lock()
    val owned = try registry.map.get(peerDirectory) finally readLock.unlock()
    // close() takes the write lock; do not upgrade while holding the read lock.
    owned.foreach(_.close())
  }
}
