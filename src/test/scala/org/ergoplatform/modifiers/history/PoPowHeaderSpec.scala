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
}
