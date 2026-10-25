package org.ergoplatform.nodeView.wallet

import java.io.ByteArrayOutputStream

import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry, WalletStorage}
import org.ergoplatform.sdk.wallet.secrets.{ExtendedPublicKey, ExtendedPublicKeySerializer, ExtendedSecretKey}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.iq80.leveldb.DB
import scorex.db.LDBKVStore

import scala.util.{Failure, Try}

class WalletKeyCacheSpec extends ErgoCorePropertyTest with DBSpec {
  private class KeySupport extends ErgoWalletSupport {
    override val ergoSettings: ErgoSettings = settings
    def add(state: ErgoWalletState, secret: ExtendedSecretKey): Try[ErgoWalletState] = addSecretToStorage(state, secret)
  }

  private class ControlledStore(db: DB) extends LDBKVStore(db) {
    var failure: Option[Throwable] = None
    override def insert(key: Array[Byte], value: Array[Byte]): Try[Unit] = failure match {
      case Some(error) => Failure(error)
      case None => super.insert(key, value)
    }
  }

  private final class Fixture(val state: ErgoWalletState, val store: ControlledStore,
                              val root: ExtendedSecretKey, val first: ExtendedSecretKey, val second: ExtendedSecretKey)

  private def withWallet(providedCache: Boolean)(test: Fixture => Unit): Unit = {
    withVersionedStore(2) { versioned =>
      withDb { db =>
        val store = new ControlledStore(db)
        val storage = new WalletStorage(store, settings)
        val root = ExtendedSecretKey.deriveMasterKey(Array.fill[Byte](32)(1), usePre1627KeyDerivation = false)
        val first = root.child(1)
        val second = root.child(2)
        try {
          storage.addPublicKey(root.publicKey).get
          val provided = if (providedCache) Some(WalletCache(Seq(root.publicKey), settings)) else None
          val vars = WalletVars(Some(ErgoProvingInterpreter(root, parameters)), Seq.empty, provided)(settings)
          val state = ErgoWalletState(storage, None, new WalletRegistry(versioned)(settings.walletSettings),
            OffChainRegistry.empty, None, vars, None, None, None, parameters, 1000, rescanInProgress = false)
          test(new Fixture(state, store, root, first, second))
        } finally {
          first.zeroSecret()
          second.zeroSecret()
          root.zeroSecret()
        }
      }
    }
  }

  private def identities(keys: Seq[ExtendedPublicKey]): Seq[Seq[Byte]] =
    keys.map(key => ExtendedPublicKeySerializer.toBytes(key).toSeq)

  private def assertKeys(state: ErgoWalletState, expected: Seq[ExtendedPublicKey]): Unit = {
    val cached = identities(state.walletVars.trackedPubKeys)
    cached shouldBe identities(expected)
    cached.distinct.size shouldBe cached.size
    identities(state.walletVars.proverOpt.get.hdPubKeys) shouldBe identities(expected)
    val stored = identities(state.storage.readAllKeys())
    stored.toSet shouldBe identities(expected).toSet
    stored.size shouldBe expected.size
  }

  private def filterBytes(state: ErgoWalletState): Seq[Byte] = {
    val bytes = new ByteArrayOutputStream()
    state.walletVars.scriptsFilter.writeTo(bytes)
    bytes.toByteArray.toSeq
  }

  for (provided <- Seq(false, true)) {
    property(s"derived key identity appears once in prover, cache and storage with provided cache=$provided") {
      withWallet(provided) { fixture =>
        val support = new KeySupport
        assertKeys(fixture.state, Seq(fixture.root.publicKey))
        val first = support.add(fixture.state, fixture.first).get
        assertKeys(first, Seq(fixture.root.publicKey, fixture.first.publicKey))
        identities(fixture.state.walletVars.trackedPubKeys) shouldBe identities(Seq(fixture.root.publicKey))
        val second = support.add(first, fixture.second).get
        assertKeys(second, Seq(fixture.root.publicKey, fixture.first.publicKey, fixture.second.publicKey))
      }
    }

    property(s"a public-key write failure does not publish the new cache with provided cache=$provided") {
      withWallet(provided) { fixture =>
        val error = new IllegalStateException("controlled public-key persistence failure")
        val previousFilter = filterBytes(fixture.state)
        fixture.store.failure = Some(error)
        new KeySupport().add(fixture.state, fixture.first).failed.get should be theSameInstanceAs error
        assertKeys(fixture.state, Seq(fixture.root.publicKey))
        filterBytes(fixture.state) shouldBe previousFilter
      }
    }
  }
}
