package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.db.LDBKVStore

import java.io.IOException
import java.nio.file.Files
import scala.util.{Failure, Success, Try}

class WalletRescanRecoveryIntentSpec extends AnyPropSpec with Matchers {

  private def settings = ErgoNodeTestConstants.settings.copy(
    directory = Files.createTempDirectory("wallet-rescan-recovery-").toFile.getAbsolutePath
  )

  property("pending rescan intent survives reopen and only a synced clear removes it") {
    val walletSettings = settings
    val storage = WalletStorage.readOrCreate(walletSettings)
    try {
      storage.rescanRecoveryIntent.get shouldBe false
      storage.beginRescanRecovery().get
      storage.rescanRecoveryIntent.get shouldBe true
      storage.pendingRescanStartHeight.get shouldBe Some(1)
    } finally storage.close()

    val reopened = WalletStorage.readOrCreate(walletSettings)
    try {
      reopened.rescanRecoveryIntent.get shouldBe true
      reopened.pendingRescanStartHeight.get shouldBe Some(1)
      reopened.beginRescanRecovery(0).get
      reopened.beginRescanRecovery(2).isFailure shouldBe true
      reopened.clearRescanRecovery().get
      reopened.rescanRecoveryIntent.get shouldBe false
      reopened.clearRescanRecovery().isFailure shouldBe true
    } finally reopened.close()
  }

  property("a suffix intent survives reopen and only its requested height can resume it") {
    val walletSettings = settings
    val storage = WalletStorage.readOrCreate(walletSettings)
    try {
      storage.beginRescanRecovery(2).get
      storage.pendingRescanStartHeight.get shouldBe Some(2)
      storage.deepForkQuarantine.get shouldBe false
    } finally storage.close()

    val reopened = WalletStorage.readOrCreate(walletSettings)
    try {
      reopened.pendingRescanStartHeight.get shouldBe Some(2)
      reopened.deepForkQuarantine.get shouldBe false
      reopened.beginRescanRecovery(0).isFailure shouldBe true
      reopened.beginRescanRecovery(3).isFailure shouldBe true
      reopened.beginRescanRecovery(2).get
      reopened.clearRescanRecovery().get
      reopened.pendingRescanStartHeight.get shouldBe None
    } finally reopened.close()
  }

  property("an explicit earlier replay replaces a pending suffix intent durably") {
    val walletSettings = settings
    val storage = WalletStorage.readOrCreate(walletSettings)
    try {
      storage.beginRescanRecovery(4).get
      storage.restartRescanRecoveryEarlier(2).get
      storage.pendingRescanStartHeight.get shouldBe Some(2)
      storage.beginRescanRecovery(4).isFailure shouldBe true
      storage.restartRescanRecoveryEarlier(3).isFailure shouldBe true
    } finally storage.close()

    val reopened = WalletStorage.readOrCreate(walletSettings)
    try {
      reopened.pendingRescanStartHeight.get shouldBe Some(2)
      reopened.beginRescanRecovery(2).get
      reopened.restartRescanRecoveryEarlier(1).get
      reopened.pendingRescanStartHeight.get shouldBe Some(1)
    } finally reopened.close()
  }

  property("failed earlier replay intent write keeps the existing pending height") {
    val walletSettings = settings
    var current: Option[Array[Byte]] = None
    var writes = 0
    val store = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = current
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = {
        writes += 1
        if (writes == 2) Failure(new IOException("injected earlier intent write failure"))
        else {
          current = Some(value)
          Success(())
        }
      }
    }
    val storage = new WalletStorage(store, walletSettings)
    storage.beginRescanRecovery(4).get
    storage.restartRescanRecoveryEarlier(2).isFailure shouldBe true
    storage.pendingRescanStartHeight.get shouldBe Some(4)
  }

  property("a legacy one-byte pending intent permits genesis only") {
    val walletSettings = settings
    var current: Option[Array[Byte]] = Some(Array(1: Byte))
    val legacy = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = current
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = {
        current = Some(value)
        Success(())
      }
    }
    val storage = new WalletStorage(legacy, walletSettings)
    storage.pendingRescanStartHeight.get shouldBe Some(1)
    storage.beginRescanRecovery(0).get
    storage.beginRescanRecovery(1).get
    storage.beginRescanRecovery(2).isFailure shouldBe true
    storage.clearRescanRecovery().get
    storage.pendingRescanStartHeight.get shouldBe None
  }

  property("rescan intent write and readback faults fail closed") {
    val walletSettings = settings
    val writeFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = None
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] =
        Failure(new IOException("injected intent write failure"))
    }
    new WalletStorage(writeFailure, walletSettings).beginRescanRecovery().isFailure shouldBe true

    val readbackFailure = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = None
      override def insertSync(key: Array[Byte], value: Array[Byte]): Try[Unit] = Success(())
    }
    new WalletStorage(readbackFailure, walletSettings).beginRescanRecovery().isFailure shouldBe true

    val malformed = new LDBKVStore(null) {
      override def get(key: Array[Byte]): Option[Array[Byte]] = Some(Array(2: Byte))
    }
    new WalletStorage(malformed, walletSettings).rescanRecoveryIntent.isFailure shouldBe true

    Seq(
      Array[Byte](2, 0, 0, 0, 0),
      Array[Byte](2, -1, -1, -1, -1)
    ).foreach { invalidHeight =>
      val invalid = new LDBKVStore(null) {
        override def get(key: Array[Byte]): Option[Array[Byte]] = Some(invalidHeight)
      }
      val storage = new WalletStorage(invalid, walletSettings)
      storage.pendingRescanStartHeight.isFailure shouldBe true
      storage.beginRescanRecovery(1).isFailure shouldBe true
    }
  }

  property("failed synced clear retains the pending rescan intent") {
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
    storage.beginRescanRecovery().get
    storage.clearRescanRecovery().isFailure shouldBe true
    storage.rescanRecoveryIntent.get shouldBe true
  }
}
