package org.ergoplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.StateType
import scorex.util.ModifierId

import scala.concurrent.duration.FiniteDuration

case class CheckpointSettings(height: Height, blockId: ModifierId)

trait CheckpointingSettingsReader extends ModifierIdReader {
  implicit val checkpointSettingsReader: ValueReader[CheckpointSettings] = { (cfg, path) =>
    CheckpointSettings(
      cfg.as[Int](s"$path.height"),
      ModifierId @@ cfg.as[String](s"$path.blockId")
    )
  }
}

/**
  * Configuration file for Ergo node regime
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class NodeConfigurationSettings(stateType: StateType,
                                     verifyTransactions: Boolean,
                                     blocksToKeep: Int,
                                     poPoWBootstrap: Boolean,
                                     minimalSuffix: Int,
                                     mining: Boolean,
                                     maxTransactionCost: Int,
                                     maxTransactionSize: Int,
                                     useExternalMiner: Boolean,
                                     internalMinersCount: Int,
                                     internalMinerPollingInterval: FiniteDuration,
                                     miningPubKeyHex: Option[String],
                                     offlineGeneration: Boolean,
                                     keepVersions: Int,
                                     acceptableChainUpdateDelay: FiniteDuration,
                                     mempoolCapacity: Int,
                                     mempoolCleanupDuration: FiniteDuration,
                                     rebroadcastCount: Int,
                                     minimalFeeAmount: Long,
                                     headerChainDiff: Int,
                                     adProofsSuffixLength: Int,
                                     extraIndex: Boolean,
                                     blacklistedTransactions: Seq[String] = Seq.empty,
                                     checkpoint: Option[CheckpointSettings] = None) {
  /**
    * Whether the node keeping all the full blocks of the blockchain or not.
    * @return true if the blockchain is pruned, false if not
    */
  val isFullBlocksPruned: Boolean = blocksToKeep >= 0
}

trait NodeConfigurationReaders extends StateTypeReaders with CheckpointingSettingsReader with ModifierIdReader {

  implicit val nodeConfigurationReader: ValueReader[NodeConfigurationSettings] = { (cfg, path) =>
    val stateTypeKey = s"$path.stateType"
    val stateType = stateTypeFromString(cfg.as[String](stateTypeKey), stateTypeKey)
    NodeConfigurationSettings(
      stateType,
      cfg.as[Boolean](s"$path.verifyTransactions"),
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Boolean](s"$path.PoPoWBootstrap"),
      cfg.as[Int](s"$path.minimalSuffix"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[Int](s"$path.maxTransactionCost"),
      cfg.as[Int](s"$path.maxTransactionSize"),
      cfg.as[Boolean](s"$path.useExternalMiner"),
      cfg.as[Int](s"$path.internalMinersCount"),
      cfg.as[FiniteDuration](s"$path.internalMinerPollingInterval"),
      cfg.as[Option[String]](s"$path.miningPubKeyHex"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Int](s"$path.keepVersions"),
      cfg.as[FiniteDuration](s"$path.acceptableChainUpdateDelay"),
      cfg.as[Int](s"$path.mempoolCapacity"),
      cfg.as[FiniteDuration](s"$path.mempoolCleanupDuration"),
      cfg.as[Int](s"$path.rebroadcastCount"),
      cfg.as[Long](s"$path.minimalFeeAmount"),
      cfg.as[Int](s"$path.headerChainDiff"),
      cfg.as[Int](s"$path.adProofsSuffixLength"),
      cfg.as[Boolean](s"$path.extraIndex"),
      cfg.as[Seq[String]](s"$path.blacklistedTransactions"),
      cfg.as[Option[CheckpointSettings]](s"$path.checkpoint")
    )
  }

}
