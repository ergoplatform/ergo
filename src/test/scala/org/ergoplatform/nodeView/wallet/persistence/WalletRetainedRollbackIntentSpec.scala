package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.nodeView.wallet.ErgoWalletState
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.db.LDBKVStore
import scorex.util.bytesToId

import java.io.IOException
import java.nio.file.Files
import scala.util.{Failure, Success, Try}

class WalletRetainedRollbackIntentSpec extends AnyPropSpec with Matchers {

  private val source = bytesToId(Array.fill(32)(1: Byte))
  private val target = bytesToId(Array.fill(32)(2: Byte))

  private def settings = ErgoNodeTestConstants.settings.copy(
    directory = Files.createTempDirectory("wallet-retained-rollback-").toFile.getAbsolutePath
  )

  property("pending intent survives reopen and fences startup before registry creation") {
    val walletSettings = settings
    val registryFolder = WalletRegistry.registryFolder(walletSettings)
    val storage = WalletStorage.readOrCreate(walletSettings)
    try {
      val shortSource = bytesToId(Array.fill(31)(3: Byte))
      storage.beginRetainedRollback(shortSource, target).isFailure shouldBe true
      storage.retainedRollbackIntent.get shouldBe None
      storage.beginRetainedRollback(source, target).get
      storage.retainedRollbackIntent.get shouldBe Some(WalletStorage.RetainedRollbackIntent(source, target))
      storage.beginRetainedRollback(source, target).isFailure shouldBe true
    } finally storage.close()

    registryFolder.exists() shouldBe false
    ErgoWalletState.initial(walletSettings, parameters).isFailure shouldBe true
    registryFolder.exists() shouldBe false

    val reopened = WalletStorage.readOrCreate(walletSettings)
    try {
      reopened.retainedRollbackIntent.get shouldBe Some(WalletStorage.RetainedRollbackIntent(source, target))
      reopened.clearRetainedRollback(source, source).isFailure shouldBe true
      reopened.clearRetainedRollback(source, target).get
      reopened.retainedRollbackIntent.get shouldBe None
    } finally reopened.close()

    val cleared = WalletStorage.readOrCreate(walletSettings)
    try cleared.retainedRollbackIntent.get shouldBe None
    finally cleared.close()
  }

  property("intent write and readback faults never report a completed prewrite") {
    val walletSettings = settings
    val writeFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = None
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] =
        Failure(new IOException("injected intent write failure"))
    }
    new WalletStorage(writeFailure, walletSettings).beginRetainedRollback(source, target).isFailure shouldBe true

    val readbackFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = None
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = Success(())
    }
    new WalletStorage(readbackFailure, walletSettings).beginRetainedRollback(source, target).isFailure shouldBe true

    val malformed = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = Some(Array(3: Byte))
    }
    new WalletStorage(malformed, walletSettings).retainedRollbackIntent.isFailure shouldBe true
  }

  property("failed synced clear leaves the exact pending intent in place") {
    val walletSettings = settings
    var current: Option[Array[Byte]] = None
    var writes = 0
    val clearFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = current
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = {
        writes += 1
        if (writes == 2) Failure(new IOException("injected intent clear failure"))
        else {
          current = Some(value)
          Success(())
        }
      }
    }
    val storage = new WalletStorage(clearFailure, walletSettings)
    storage.beginRetainedRollback(source, target).get
    storage.clearRetainedRollback(source, target).isFailure shouldBe true
    storage.retainedRollbackIntent.get shouldBe Some(WalletStorage.RetainedRollbackIntent(source, target))
  }
}
