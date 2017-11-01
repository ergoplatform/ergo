package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AlgosTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators with scorex.testkit.SerializationTests {


  property("blockIdDifficulty should be > 0") {
    forAll(genBytesList(32)) { id: Array[Byte] =>
      require(Algos.blockIdDifficulty(Algos.hash(id)) > 0)
    }
  }
}
