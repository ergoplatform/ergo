package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.utils.ErgoPropertyTest

class ObjectsStoreSpecification extends ErgoPropertyTest {

  val folder: String = createTempDir.getAbsolutePath
  val objectsStore = new FilesObjectsStore(folder)

  property("FilesObjectsStore: put, get, delete") {
    forAll(invalidHeaderGen) { header =>
      objectsStore.get(header.id) shouldBe None
      objectsStore.put(header)
      HistoryModifierSerializer.parseBytes(objectsStore.get(header.id).get).get shouldBe header
      objectsStore.delete(header.id)
      objectsStore.get(header.id) shouldBe None
    }
  }


}
