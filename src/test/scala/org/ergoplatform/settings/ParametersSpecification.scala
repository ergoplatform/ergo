package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingResults}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest

class ParametersSpecification extends ErgoPropertyTest {

  property("simple voting") {
    val p: Parameters = ???
    var vr: VotingResults = ???
    val esc = ErgoStateContext(Seq(), ADDigest @@ Array.fill(33)(0: Byte), p, vr)
  }
}
