package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.popow.PoPowHeader.{checkProof, nipopowAlgos}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen

class PoPowHeaderSpec extends ErgoPropertyTest {

  property("Check interlinks proof should be true") {
    forAll(Gen.nonEmptyListOf(modifierIdGen)) { interlinks =>
      val interlinksProof = nipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks)).get
      checkProof(interlinks, interlinksProof) shouldBe true
    }
  }

  property("Check invalid interlinks proof should be false") {
    forAll(Gen.nonEmptyListOf(modifierIdGen), Gen.nonEmptyListOf(modifierIdGen)) { (interlinks1, interlinks2) =>
        val interlinksProof = nipopowAlgos.proofForInterlinkVector(nipopowAlgos.interlinksToExtension(interlinks2)).get
        checkProof(interlinks1, interlinksProof) shouldBe false
    }
  }
}
