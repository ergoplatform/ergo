package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.utils.ErgoCorePropertyTest

class LaunchParametersSpec extends ErgoCorePropertyTest {

  property("MainnetLaunchParameters should have default block version") {
    MainnetLaunchParameters.blockVersion shouldBe Parameters.DefaultParameters(Parameters.BlockVersion)
  }

  property("MainnetLaunchParameters should have empty validation settings update") {
    MainnetLaunchParameters.proposedUpdate shouldBe ErgoValidationSettingsUpdate.empty
  }

  property("MainnetLaunchParameters should have height 0") {
    MainnetLaunchParameters.height shouldBe 0
  }

  property("TestnetLaunchParameters should have block version set to Interpreter60Version") {
    TestnetLaunchParameters.blockVersion shouldBe Header.Interpreter60Version
  }

  property("TestnetLaunchParameters should have validation settings update with rules 215 and 409") {
    TestnetLaunchParameters.proposedUpdate.rulesToDisable should contain theSameElementsAs Seq(215, 409)
  }

  property("TestnetLaunchParameters should have empty status updates") {
    TestnetLaunchParameters.proposedUpdate.statusUpdates shouldBe empty
  }

  property("TestnetLaunchParameters should have height 0") {
    TestnetLaunchParameters.height shouldBe 0
  }

  property("DevnetLaunchParameters should have block version set to Interpreter50Version") {
    DevnetLaunchParameters.blockVersion shouldBe Header.Interpreter50Version
  }

  property("DevnetLaunchParameters should have empty validation settings update") {
    DevnetLaunchParameters.proposedUpdate shouldBe ErgoValidationSettingsUpdate.empty
  }

  property("DevnetLaunchParameters should have height 0") {
    DevnetLaunchParameters.height shouldBe 0
  }

  property("Devnet60LaunchParameters should have block version set to Interpreter60Version") {
    Devnet60LaunchParameters.blockVersion shouldBe Header.Interpreter60Version
  }

  property("Devnet60LaunchParameters should have empty validation settings update") {
    Devnet60LaunchParameters.proposedUpdate shouldBe ErgoValidationSettingsUpdate.empty
  }

  property("Devnet60LaunchParameters should have height 0") {
    Devnet60LaunchParameters.height shouldBe 0
  }

  property("all launch parameters should have valid height") {
    Seq(
      MainnetLaunchParameters,
      TestnetLaunchParameters,
      DevnetLaunchParameters,
      Devnet60LaunchParameters
    ).foreach(_.height shouldBe 0)
  }

  property("TestnetLaunchParameters should differ from MainnetLaunchParameters") {
    TestnetLaunchParameters.blockVersion should not be MainnetLaunchParameters.blockVersion
    TestnetLaunchParameters.proposedUpdate should not be MainnetLaunchParameters.proposedUpdate
  }

  property("Devnet60LaunchParameters should have same block version as TestnetLaunchParameters") {
    Devnet60LaunchParameters.blockVersion shouldBe TestnetLaunchParameters.blockVersion
  }

  property("DevnetLaunchParameters should have different block version than Devnet60LaunchParameters") {
    DevnetLaunchParameters.blockVersion should not be Devnet60LaunchParameters.blockVersion
  }

  property("parameters table should contain BlockVersion for all launch parameters") {
    Seq(
      MainnetLaunchParameters,
      TestnetLaunchParameters,
      DevnetLaunchParameters,
      Devnet60LaunchParameters
    ).foreach { params =>
      params.parametersTable should contain key Parameters.BlockVersion
    }
  }

}
