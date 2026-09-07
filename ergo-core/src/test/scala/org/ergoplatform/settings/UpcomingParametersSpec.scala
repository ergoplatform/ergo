package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.utils.ErgoCorePropertyTest

class UpcomingParametersSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import Parameters._

  private val settings = chainSettings.copy(voting = VotingSettings(4, 2, 3, 100, "01"))
  private val emptyUpdate = ErgoValidationSettingsUpdate.empty
  private val update = ErgoValidationSettingsUpdate(Seq(ValidationRules.exDuplicateKeys), Seq.empty)

  private def context(height: Int, p: Parameters, votes: VotingData): ErgoStateContext = {
    val header = defaultHeaderGen.sample.get.copy(height = height, version = p.blockVersion)
    new ErgoStateContext(Seq(header), None, genesisStateDigest, p, validationSettingsNoIl, votes)(settings)
  }

  private def project(sc: ErgoStateContext, version: Byte, proposal: ErgoValidationSettingsUpdate): ErgoStateContext =
    sc.upcoming(defaultMinerPkPoint, defaultTimestamp, defaultNBits, Array.emptyByteArray, proposal, version)

  private def checkParameters(actual: Parameters, expected: Parameters): Unit = {
    actual.height shouldBe expected.height
    actual.parametersTable shouldBe expected.parametersTable
    actual.proposedUpdate shouldBe expected.proposedUpdate
  }

  property("upcoming contexts retain accepted parameters and fork tallies between epochs") {
    val p = Parameters(4, DefaultParameters ++ Map(SoftForkStartingHeight -> 4, SoftForkVotesCollected -> 0), update)
    val sc = context(6, p, VotingData(Array(MinValuePerByteIncrease -> 3, SoftFork -> 3)))
    val next = sc.lastHeaderOpt.get.copy(height = 7, votes = Array.emptyByteArray)
    val accepted = sc.process(next, None).get
    Seq(project(sc, p.blockVersion, emptyUpdate), sc.simplifiedUpcoming()).foreach { projected =>
      checkParameters(projected.currentParameters, accepted.currentParameters)
      projected.validationSettings shouldBe accepted.validationSettings
      projected.currentHeight shouldBe 7
    }
  }

  property("both upcoming contexts apply ordinary votes at the next epoch") {
    val p = Parameters(4, DefaultParameters, emptyUpdate)
    val sc = context(7, p, VotingData(Array(MinValuePerByteIncrease -> 3)))
    val expectedValue = p.minValuePerByte + 10
    val expected = Parameters(8, p.parametersTable.updated(MinValuePerByteIncrease, expectedValue), emptyUpdate)
    val extension = (expected.toExtensionCandidate ++ sc.validationSettings.toExtensionCandidate)
      .toExtension(Header.GenesisParentId)
    val accepted = sc.process(sc.lastHeaderOpt.get.copy(height = 8, votes = Array.emptyByteArray), Some(extension)).get
    Seq(project(sc, p.blockVersion, emptyUpdate), sc.simplifiedUpcoming()).foreach { projected =>
      checkParameters(projected.currentParameters, accepted.currentParameters)
      projected.validationSettings shouldBe accepted.validationSettings
    }
  }

  property("upcoming keeps genesis parameters until the first epoch") {
    val sc = ErgoStateContext.empty(genesisStateDigest, settings, parameters)
    Seq(project(sc, parameters.blockVersion, emptyUpdate), sc.simplifiedUpcoming()).foreach { projected =>
      checkParameters(projected.currentParameters, parameters)
      projected.currentHeight shouldBe 1
    }
  }

  property("upcoming preserves scheduled version two activation at an epoch boundary") {
    val p = Parameters(96, DefaultParameters.updated(BlockVersion, 1), emptyUpdate)
    val before = context(98, p, VotingData.empty)
    val at = context(99, p, VotingData.empty)
    checkParameters(project(before, 1, emptyUpdate).currentParameters, p)
    checkParameters(before.simplifiedUpcoming().currentParameters, p)
    Seq(project(at, 2, emptyUpdate), at.simplifiedUpcoming()).foreach { projected =>
      projected.currentParameters.blockVersion shouldBe 2
      projected.currentParameters.height shouldBe 100
    }
  }

  property("full upcoming applies approved validation changes only at scheduled activation") {
    val table = DefaultParameters ++ Map(SoftForkStartingHeight -> 4, SoftForkVotesCollected -> 8)
    val p = Parameters(20, table, update)
    val before = context(22, p, VotingData.empty)
    val at = context(23, p, VotingData.empty)
    checkParameters(project(before, p.blockVersion, update).currentParameters, p)
    project(before, p.blockVersion, update).validationSettings shouldBe before.validationSettings
    val projected = project(at, (p.blockVersion + 1).toByte, update)
    projected.currentParameters.blockVersion shouldBe (p.blockVersion + 1).toByte
    projected.validationSettings shouldBe at.validationSettings.updated(update)
  }
}
