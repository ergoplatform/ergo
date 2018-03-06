package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.utils.{ErgoGenerators, ErgoTestHelpers}
import org.scalatest.PropSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class ObjectsStoreSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with ErgoGenerators
  with ErgoTestHelpers {

  val folder: String = createTempDir.getAbsolutePath
  val objectsStore = new FilesObjectsStore(folder)

  property("FilesObjectsStore: put, get, delete") {
    forAll(invalidHeaderGen) { header =>
      objectsStore.get(header.id) shouldBe None
      objectsStore.put(header)
      objectsStore.get(header.id).get shouldBe header
      objectsStore.delete(header.id)
      objectsStore.get(header.id) shouldBe None
    }
  }

}
