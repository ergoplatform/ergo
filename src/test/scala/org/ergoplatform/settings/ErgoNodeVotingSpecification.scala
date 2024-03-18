package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest

class ErgoNodeVotingSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private val votingEpochLength = 2

  private val hfActivationHeight = 100

  private val hfActivationDifficultyHex = "01"

  implicit val votingSettings: VotingSettings =
    VotingSettings(
      votingEpochLength,
      softForkEpochs = 2,
      activationEpochs = 3,
      version2ActivationHeight = hfActivationHeight,
      version2ActivationDifficultyHex = hfActivationDifficultyHex
    )

  private val updSettings = chainSettings.copy(voting = votingSettings)

  val ctx: ErgoStateContext = {
    new ErgoStateContext(Seq.empty, None, genesisStateDigest, parameters, validationSettingsNoIl, VotingData.empty)(updSettings)
      .upcoming(org.ergoplatform.mining.group.generator, 0L, chainSettings.initialNBits, Array.fill(3)(0.toByte), emptyVSUpdate, 0.toByte)
  }
  val initialVs: ErgoValidationSettings = ctx.validationSettings
  val extensionWithAllParams: ExtensionCandidate = {
    parameters.toExtensionCandidate ++ initialVs.toExtensionCandidate
  }

  val emptyParameters = Parameters(0, Map.empty, ErgoValidationSettingsUpdate.empty)

  property("Parameters should be defined at the beginning of the epoch") {
    val chain = genChain(votingEpochLength * 4).map { b =>
      if (b.header.votingStarts(votingEpochLength)) {
        b.copy(extension = extensionWithAllParams.toExtension(b.header.id))
      } else {
        b
      }
    }
    val validChain = chain.init
    val lastBlock = chain.last
    val invalidExtBlock1 = { // extension does not contain all required params
      lastBlock.copy(extension = lastBlock.extension.copy(
        fields = Seq(Array(0: Byte, 1: Byte) -> Array.fill(4)(2: Byte)))
      )
    }
    val invalidExtBlock2 = { // extension contains redundant parameter
      lastBlock.copy(extension = lastBlock.extension.copy(
        fields = parameters.toExtensionCandidate.fields :+ Array(0: Byte, 99: Byte) -> Array.fill(4)(2: Byte))
      )
    }
    val invalidExtBlock3 = { // extension does not contain params at all
      lastBlock.copy(extension = lastBlock.extension.copy(fields = Seq()))
    }
    val validCtx = validChain.foldLeft(ctx)((acc, mod) => acc.appendFullBlock(mod).get)
    validCtx.appendFullBlock(invalidExtBlock1) shouldBe 'failure
    validCtx.appendFullBlock(invalidExtBlock2) shouldBe 'failure
    validCtx.appendFullBlock(invalidExtBlock3) shouldBe 'failure
    validCtx.appendFullBlock(lastBlock) shouldBe 'success
  }

}
