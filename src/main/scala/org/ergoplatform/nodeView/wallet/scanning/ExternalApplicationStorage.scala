package org.ergoplatform.nodeView.wallet.scanning

import com.google.common.primitives.Longs
import org.ergoplatform.db.{LDBFactory, LDBKVStore}
import org.ergoplatform.settings.{Constants, ErgoSettings}

import scala.util.Try

/**
  * Database for applications.
  *
  * @param store - LevelDB key-value storage
  */
class ExternalApplicationStorage(store: LDBKVStore) {

  def addApplication(appReq: ExternalAppRequest): Try[ExternalApplication] = {
    val id = lastUsedId + 1
    appReq.toApp(id).flatMap { app =>
      Try(store.insert(Seq(Longs.toByteArray(id) -> ExternalApplicationSerializer.toBytes(app))))
        .map(_ => app)
    }
  }

  def removeApplication(id: Long): Unit = store.remove(Seq(Longs.toByteArray(id)))

  def allApplications: Seq[ExternalApplication] =
    store.getAll.map { case (_, v) => ExternalApplicationSerializer.parseBytes(v) }

  def lastUsedId: Long = Try(Longs.fromByteArray(store.lastKey())).getOrElse(Constants.DefaultAppId)

}

object ExternalApplicationStorage {
  def readOrCreate(settings: ErgoSettings): ExternalApplicationStorage = {
    new ExternalApplicationStorage(LDBFactory.createKvDb(s"${settings.directory}/apps"))
  }
}