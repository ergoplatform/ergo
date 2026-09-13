package org.ergoplatform.it

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{ErgoSettingsReader, NetworkType}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16

object MatrixDevnetConfig {

  def apply(base: Config): Config = {
    val configured = ConfigFactory.parseString(
      """
        |ergo.networkType = "devnet"
        |ergo.node.useExternalMiner = true
        |ergo.node.mining = true
        |ergo.node.offlineGeneration = true
        |ergo.chain.initialDifficultyHex = "01"
        |ergo.chain.epochLength = 1024
        |ergo.chain.blockInterval = 1s
        |ergo.chain.monetary.minerRewardDelay = 1
        |ergo.chain.voting.votingLength = 8
        |ergo.chain.voting.softForkEpochs = 1
        |ergo.chain.voting.activationEpochs = 1
        |ergo.chain.voting.version2ActivationHeight = 8
        |ergo.chain.voting.version2ActivationDifficultyHex = "0080"
        |ergo.voting.120 = 1
        |""".stripMargin
    ).withFallback(base)
      .withFallback(ConfigFactory.parseResources("devnet.conf"))

    val settings = ErgoSettingsReader.fromConfig(
      configured
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve(),
      Some(NetworkType.DevNet)
    )

    // Reward delay is embedded in the genesis emission script. Derive the root
    // with the same box insertion producer used by UtxoState.fromBoxHolder.
    val boxes = BoxHolder(ErgoState.genesisBoxes(settings.chainSettings))
    val prover = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    boxes.sortedBoxes.foreach { box =>
      prover.performOneOperation(Insert(box.id, ADValue @@ box.bytes)).get
    }

    // Keep directory substitutions unresolved for each container's own data path.
    configured.withValue(
      "ergo.chain.genesisStateDigestHex",
      ConfigValueFactory.fromAnyRef(Base16.encode(prover.digest))
    )
  }
}
