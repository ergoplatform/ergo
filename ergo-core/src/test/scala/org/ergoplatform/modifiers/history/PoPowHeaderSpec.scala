package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.popow.PoPowHeader.checkInterlinksProof
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

class PoPowHeaderSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  property("Check interlinks proof should be true") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { interlinks =>
      val interlinksProof = NipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks)).get
      checkInterlinksProof(interlinks, interlinksProof) shouldBe true
    }
  }

  property("Check invalid interlinks proof should be false") {
    forAll(Gen.nonEmptyListOf(modifierIdGen), Gen.nonEmptyListOf(modifierIdGen)) { (interlinks1, interlinks2) =>
        val interlinksProof = NipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks2)).get
        checkInterlinksProof(interlinks1, interlinksProof) shouldBe false
    }
  }
}
