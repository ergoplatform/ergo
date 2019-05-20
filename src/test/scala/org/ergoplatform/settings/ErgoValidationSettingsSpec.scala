package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.Extension
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.Parameters._
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest
import scala.language.implicitConversions

class ErgoValidationSettingsSpec extends ErgoPropertyTest {

  private val headerId = scorex.util.bytesToId(Array.fill(32)(0: Byte))

  private val votingEpochLength = 2

  override implicit val votingSettings: VotingSettings =
    VotingSettings(votingEpochLength, softForkEpochs = 2, activationEpochs = 3)

  private def toExtension(s: ErgoValidationSettings): Extension = s.toExtensionCandidate().toExtension(headerId)
  private implicit def toExtension(p: Parameters): Extension = p.toExtensionCandidate(Seq.empty).toExtension(headerId)

  ignore("disabled rules are in settings") {
    // todo
  }


}
