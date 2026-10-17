package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.db.LDBFactory
import scorex.util.idToBytes

import java.nio.file.Files

class HistoryStorageIdentitySpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  property("a parsed modifier under a different key is neither returned nor cached") {
    val root = Files.createTempDirectory("history-modifier-identity")
    val indexStore = LDBFactory.createKvDb(root.resolve("index").toString)
    val objectsStore = LDBFactory.createKvDb(root.resolve("objects").toString)
    val extraStore = LDBFactory.createKvDb(root.resolve("extra").toString)
    val storage = new HistoryStorage(indexStore, objectsStore, extraStore, settings.cacheSettings)
    val requested = Gen.listOfN(1, defaultHeaderGen).sample.get.head
    val foreign = requested.copy(timestamp = requested.timestamp + 1)
    val valid = requested.copy(timestamp = requested.timestamp + 2)

    try {
      requested.id should not be foreign.id
      foreign.id should not be valid.id
      objectsStore.insert(idToBytes(valid.id), HistoryModifierSerializer.toBytes(valid)).get
      objectsStore.insert(idToBytes(requested.id), HistoryModifierSerializer.toBytes(foreign)).get

      storage.modifierById(valid.id).map(_.id) shouldBe Some(valid.id)
      val wrongKeyRead = storage.modifierById(requested.id).map(_.id)
      val cacheRead = storage.modifierById(foreign.id).map(_.id)
      (wrongKeyRead, cacheRead) shouldBe ((None, None))
    } finally storage.close()
  }
}
