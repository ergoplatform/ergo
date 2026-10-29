package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.settings.{CacheSettings, HistoryCacheSettings}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.SerializedAdProof
import scorex.db.{ByteArrayWrapper, LDBKVStore}
import scorex.util.{bytesToId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class HistoryStorageRemoveSpec extends AnyPropSpec with Matchers {

  private val cacheSettings = CacheSettings(
    HistoryCacheSettings(
      blockSectionsCacheSize = 4,
      extraCacheSize = 4,
      headersCacheSize = 4,
      indexesCacheSize = 4
    ),
    network = null,
    mempool = null
  )

  property("remove propagates success and invalidates modifier and index caches") {
    val f = fixture()

    f.storage.remove(Array(f.indexKey), Array(f.modifier.id)) shouldBe Success(())

    f.objectsStore.removalBatches shouldBe Vector(Vector(ByteArrayWrapper(idToBytes(f.modifier.id))))
    f.indexStore.removalBatches shouldBe Vector(Vector(f.indexKey))
    f.storage.modifierById(f.modifier.id) shouldBe None
    f.objectsStore.getCalls shouldBe 1
    f.storage.getIndex(f.indexKey) shouldBe None
    f.indexStore.getCalls shouldBe 1
  }

  property("remove propagates an object-store failure without changing caches or indexes") {
    val objectStoreFailure = new IllegalStateException("object-store failure")
    val f = fixture(objectRemoval = Failure(objectStoreFailure))

    f.storage.remove(Array(f.indexKey), Array(f.modifier.id)) shouldBe Failure(objectStoreFailure)

    f.objectsStore.removalBatches shouldBe Vector(Vector(ByteArrayWrapper(idToBytes(f.modifier.id))))
    f.indexStore.removalBatches shouldBe empty
    f.storage.modifierById(f.modifier.id) shouldBe Some(f.modifier)
    f.objectsStore.getCalls shouldBe 0
    f.storage.getIndex(f.indexKey) shouldBe Some(f.indexValue)
    f.indexStore.getCalls shouldBe 0
  }

  property("remove propagates an index-store failure after invalidating only modifier caches") {
    val indexStoreFailure = new IllegalStateException("index-store failure")
    val f = fixture(indexRemoval = Failure(indexStoreFailure))

    f.storage.remove(Array(f.indexKey), Array(f.modifier.id)) shouldBe Failure(indexStoreFailure)

    f.objectsStore.removalBatches shouldBe Vector(Vector(ByteArrayWrapper(idToBytes(f.modifier.id))))
    f.indexStore.removalBatches shouldBe Vector(Vector(f.indexKey))
    f.storage.modifierById(f.modifier.id) shouldBe None
    f.objectsStore.getCalls shouldBe 1
    f.storage.getIndex(f.indexKey) shouldBe Some(f.indexValue)
    f.indexStore.getCalls shouldBe 0
  }

  private def fixture(objectRemoval: Try[Unit] = Success(()),
                      indexRemoval: Try[Unit] = Success(())): Fixture = {
    val objectsStore = new DeterministicStore(objectRemoval)
    val indexStore = new DeterministicStore(indexRemoval)
    val extraStore = new DeterministicStore(Success(()))
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, cacheSettings)
    val modifier = ADProofs(
      bytesToId(Array.fill(32)(1.toByte)),
      SerializedAdProof @@ Array[Byte](2, 3, 4)
    )
    val indexKey = ByteArrayWrapper(Array.fill(32)(5.toByte))
    val indexValue = Array[Byte](6, 7, 8)

    storage.insert(Array(indexKey -> indexValue), Array[BlockSection](modifier)) shouldBe Success(())

    Fixture(storage, objectsStore, indexStore, modifier, indexKey, indexValue)
  }

  private case class Fixture(storage: HistoryStorage,
                             objectsStore: DeterministicStore,
                             indexStore: DeterministicStore,
                             modifier: ADProofs,
                             indexKey: ByteArrayWrapper,
                             indexValue: Array[Byte])

  private class DeterministicStore(removeResult: Try[Unit]) extends LDBKVStore(null) {
    private val rows = mutable.Map.empty[ByteArrayWrapper, Array[Byte]]

    var getCalls: Int = 0
    var removalBatches: Vector[Vector[ByteArrayWrapper]] = Vector.empty

    override def get(key: Array[Byte]): Option[Array[Byte]] = {
      getCalls += 1
      rows.get(ByteArrayWrapper(key)).map(_.clone())
    }

    override def insert(id: Array[Byte], value: Array[Byte]): Try[Unit] = {
      rows.update(ByteArrayWrapper(id.clone()), value.clone())
      Success(())
    }

    override def insert(keys: Array[Array[Byte]], values: Array[Array[Byte]]): Try[Unit] = {
      require(keys.length == values.length)
      keys.indices.foreach(i => rows.update(ByteArrayWrapper(keys(i).clone()), values(i).clone()))
      Success(())
    }

    override def remove(keys: Array[Array[Byte]]): Try[Unit] = {
      removalBatches :+= keys.iterator.map(key => ByteArrayWrapper(key.clone())).toVector
      removeResult.map { _ =>
        keys.foreach(key => rows.remove(ByteArrayWrapper(key)))
      }
    }

    override def close(): Unit = ()
  }
}
