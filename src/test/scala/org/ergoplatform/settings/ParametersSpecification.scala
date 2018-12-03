package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingResults}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

class ParametersSpecification extends ErgoPropertyTest {
  import Parameters._

  val votingEpochLength = 2

  property("simple voting - start") {
    val p: Parameters = Parameters(0, Map(KIncrease -> 1000000))
    val vr: VotingResults = VotingResults.empty
    val esc = ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
  }
}
