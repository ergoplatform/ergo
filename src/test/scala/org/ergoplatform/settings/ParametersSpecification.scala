package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingResults}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

import scala.language.implicitConversions

class ParametersSpecification extends ErgoPropertyTest {
  import Parameters._

  private val headerId = scorex.util.bytesToId(Array.fill(32)(0: Byte))
  private val votingEpochLength = 2

  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate().toExtension(headerId)

  property("simple voting - start - conditions") {
    val p: Parameters = Parameters(2, Map(KIncrease -> 1000000))
    val vr: VotingResults = VotingResults.empty
    val esc = ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
    val votes = Array(KIncrease, NoParameter, NoParameter)
    esc.processExtension(p, votes, 2, votingEpochLength).isSuccess shouldBe true
  }
}
