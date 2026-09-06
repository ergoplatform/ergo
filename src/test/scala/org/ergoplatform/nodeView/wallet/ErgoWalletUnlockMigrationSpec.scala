package org.ergoplatform.nodeView.wallet

import org.ergoplatform.P2PKAddress
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.{ExtendedPublicKey, ExtendedSecretKey}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.wallet.secrets.JsonSecretStorage
import org.iq80.leveldb.DB
import scorex.db.LDBKVStore

import scala.util.{Failure, Try}

class ErgoWalletUnlockMigrationSpec extends ErgoCorePropertyTest with DBSpec {
  private val service = new ErgoWalletServiceImpl(settings)
  private val masterKey = ExtendedSecretKey.deriveMasterKey(Array.fill(32)(1.toByte), usePre1627KeyDerivation = false)
  private val firstKey = masterKey.derive(org.ergoplatform.sdk.wallet.Constants.eip3DerivationPath).publicKey
  private val changeAddress = P2PKAddress(firstKey.key)(settings.chainSettings.addressEncoder)

  private class MigrationStore(db: DB) extends LDBKVStore(db) {
    var failure: Option[(Array[Byte], Throwable)] = None
    var writes: Vector[Vector[Byte]] = Vector.empty

    override def insert(id: Array[Byte], value: Array[Byte]): Try[Unit] = {
      writes :+= id.toVector
      failure match {
        case Some((key, error)) if key.sameElements(id) => Failure(error)
        case _ => super.insert(id, value)
      }
    }
  }

  private def withWallet(keys: Seq[ExtendedPublicKey], withChange: Boolean = false)
                        (test: (ErgoWalletState, MigrationStore) => Unit): Unit = {
    withVersionedStore(2) { versionedStore =>
      withDb { db =>
        val store = new MigrationStore(db)
        val storage = new WalletStorage(store, settings)
        keys.foreach(key => storage.addPublicKey(key).get)
        if (withChange) storage.updateChangeAddress(changeAddress).get
        store.writes = Vector.empty
        val secretStorage = JsonSecretStorage.init(
          Array.fill(32)(1.toByte), SecretString.create("migration-test"), usePre1627KeyDerivation = false
        )(settings.walletSettings.secretStorage.copy(secretDir = createTempDir.getPath))
        val state = ErgoWalletState(
          storage, Some(secretStorage), new WalletRegistry(versionedStore)(settings.walletSettings),
          OffChainRegistry.empty, None, WalletVars(None, Seq.empty, None)(settings),
          None, None, None, parameters, maxInputsToUse = 1000, rescanInProgress = false
        )
        try test(state, store) finally secretStorage.lock()
      }
    }
  }

  private def unlock(state: ErgoWalletState): Try[ErgoWalletState] =
    service.unlockWallet(state, SecretString.create("migration-test"), usePreEip3Derivation = false)

  property("unlock propagates change-address migration failure and retries before adding the master key") {
    withWallet(Seq(firstKey)) { (state, store) =>
      val error = new IllegalStateException("change-address migration write failed")
      store.failure = Some(WalletStorage.ChangeAddressKey -> error)

      unlock(state).failed.get should be theSameInstanceAs error
      state.walletVars.proverOpt shouldBe empty
      state.secretStorageOpt.get.isLocked shouldBe true
      state.storage.readChangeAddress shouldBe empty
      state.storage.containsPublicKey(masterKey.publicKey.path) shouldBe false
      store.writes shouldBe Vector(WalletStorage.ChangeAddressKey.toVector)

      store.failure = None
      store.writes = Vector.empty
      val unlocked = unlock(state).get
      unlocked.walletVars.proverOpt shouldNot be(empty)
      unlocked.secretStorageOpt.get.isLocked shouldBe false
      unlocked.storage.readChangeAddress shouldBe Some(changeAddress)
      unlocked.storage.readAllKeys().toSet shouldBe Set(firstKey, masterKey.publicKey)
      store.writes shouldBe Vector(WalletStorage.ChangeAddressKey.toVector, WalletStorage.pubKeyPrefixKey(masterKey.publicKey).toVector)
    }
  }

  property("unlock propagates master-key migration failure and retries after the change-address write") {
    withWallet(Seq(firstKey)) { (state, store) =>
      val error = new IllegalStateException("master-key migration write failed")
      store.failure = Some(WalletStorage.pubKeyPrefixKey(masterKey.publicKey) -> error)

      unlock(state).failed.get should be theSameInstanceAs error
      state.walletVars.proverOpt shouldBe empty
      state.secretStorageOpt.get.isLocked shouldBe true
      state.storage.readChangeAddress shouldBe Some(changeAddress)
      state.storage.containsPublicKey(masterKey.publicKey.path) shouldBe false
      store.writes shouldBe Vector(WalletStorage.ChangeAddressKey.toVector, WalletStorage.pubKeyPrefixKey(masterKey.publicKey).toVector)

      store.failure = None
      store.writes = Vector.empty
      val unlocked = unlock(state).get
      unlocked.walletVars.proverOpt shouldNot be(empty)
      unlocked.storage.readChangeAddress shouldBe Some(changeAddress)
      unlocked.storage.readAllKeys().toSet shouldBe Set(firstKey, masterKey.publicKey)
      store.writes shouldBe Vector(WalletStorage.pubKeyPrefixKey(masterKey.publicKey).toVector)
    }
  }

  property("unlock performs no writes when existing keys and change address are complete") {
    withWallet(Seq(firstKey, masterKey.publicKey), withChange = true) { (state, store) =>
      val unlocked = unlock(state).get
      unlocked.walletVars.proverOpt shouldNot be(empty)
      unlocked.storage.readChangeAddress shouldBe Some(changeAddress)
      store.writes shouldBe empty
    }
  }

  property("unlock leaves a master-only wallet without a change address") {
    withWallet(Seq(masterKey.publicKey)) { (state, store) =>
      unlock(state).get.walletVars.proverOpt shouldNot be(empty)
      state.storage.readChangeAddress shouldBe empty
      store.writes shouldBe empty
    }
  }

  property("unlock adds the master key without setting change for non-EIP3 or multiple existing keys") {
    Seq(Seq(masterKey.child(1).publicKey), Seq(firstKey, masterKey.child(1).publicKey)).foreach { keys =>
      withWallet(keys) { (state, store) =>
        unlock(state).get.walletVars.proverOpt shouldNot be(empty)
        state.storage.readChangeAddress shouldBe empty
        state.storage.readAllKeys().toSet shouldBe (keys :+ masterKey.publicKey).toSet
        store.writes shouldBe Vector(WalletStorage.pubKeyPrefixKey(masterKey.publicKey).toVector)
      }
    }
  }

  property("failed unlock preserves secret storage that was already unlocked before the attempt") {
    withWallet(Seq(firstKey)) { (state, store) =>
      state.secretStorageOpt.get.unlock(SecretString.create("migration-test")).get
      val error = new IllegalStateException("existing unlocked storage migration write failed")
      store.failure = Some(WalletStorage.ChangeAddressKey -> error)
      unlock(state).failed.get should be theSameInstanceAs error
      state.walletVars.proverOpt shouldBe empty
      state.secretStorageOpt.get.isLocked shouldBe false
    }
  }
}
