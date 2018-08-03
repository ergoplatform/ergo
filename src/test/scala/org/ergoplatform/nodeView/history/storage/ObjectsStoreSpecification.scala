package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.Await
import scala.concurrent.duration._

class ObjectsStoreSpecification extends ErgoPropertyTest {

  val folder: String = createTempDir.getAbsolutePath
  val objectsStore = new FilesObjectsStore(folder)

  property("FilesObjectsStore: put, get, delete") {
    forAll(invalidHeaderGen) { header =>
      objectsStore.get(header.id) shouldBe None
      Await.result(objectsStore.put(header), 5.second)
      HistoryModifierSerializer.parseBytes(objectsStore.get(header.id).get).get shouldBe header
      Await.result(objectsStore.delete(header.id), 5.second)
      objectsStore.get(header.id) shouldBe None
    }
  }


}
