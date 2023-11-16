package org.ergoplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.mempool.ErgoMemPool.SortingOption
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
  * Settings related to state bootstrapping with UTXO set snapshots. See ergo.node.utxo section for settings description.
  */
case class UtxoSettings(utxoBootstrap: Boolean, storingUtxoSnapshots: Int, p2pUtxoSnapshots: Int)

/**
  * Custom settings reader for `UtxoSettings`
  */
trait UtxoSettingsReader {
  implicit val utxoSettingsReader: ValueReader[UtxoSettings] = { (cfg, path) =>
    UtxoSettings(
      cfg.as[Boolean](s"$path.utxoBootstrap"),
      cfg.as[Int](s"$path.storingUtxoSnapshots"),
      cfg.as[Int](s"$path.p2pUtxoSnapshots")
    )
  }
}


/**
  * Settings related to headers-chain bootstrapping with NiPoPoWs. See ergo.node.nipopow section for settings description.
  */
case class NipopowSettings(nipopowBootstrap: Boolean, p2pNipopows: Int)

/**
  * Custom settings reader for `NipopowSettings`
  */
trait NipopowSettingsReader {
  implicit val nipopowSettingsReader: ValueReader[NipopowSettings] = { (cfg, path) =>
    NipopowSettings(
      cfg.as[Boolean](s"$path.nipopowBootstrap"),
      cfg.as[Int](s"$path.p2pNipopows")
    )
  }
}

/**
  * Features client may have enabled, they are reported to other peers
  */
trait ClientCapabilities {
  val stateType: StateType
  val verifyTransactions: Boolean
  val blocksToKeep: Int
  val utxoSettings: UtxoSettings
  val nipopowSettings: NipopowSettings
}

/**
  * Configuration file for Ergo node regime
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class NodeConfigurationSettings(override val stateType: StateType,
                                     override val verifyTransactions: Boolean,
                                     override val blocksToKeep: Int,
                                     override val utxoSettings: UtxoSettings,
                                     override val nipopowSettings: NipopowSettings,
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
                                     mempoolSorting: SortingOption,
                                     rebroadcastCount: Int,
                                     minimalFeeAmount: Long,
                                     headerChainDiff: Int,
                                     adProofsSuffixLength: Int,
                                     extraIndex: Boolean,
                                     blacklistedTransactions: Seq[String] = Seq.empty,
                                     checkpoint: Option[CheckpointSettings] = None) extends ClientCapabilities {
  /**
    * Whether the node keeping all the full blocks of the blockchain or not.
    * @return true if the blockchain is pruned, false if not
    */
  val isFullBlocksPruned: Boolean = blocksToKeep >= 0 || utxoSettings.utxoBootstrap

  val areSnapshotsStored = utxoSettings.storingUtxoSnapshots > 0
}

/**
  * Custom config reader for ergo.node settings section
  */
trait NodeConfigurationReaders extends StateTypeReaders with CheckpointingSettingsReader
                                  with UtxoSettingsReader with NipopowSettingsReader with ModifierIdReader {

  implicit val nodeConfigurationReader: ValueReader[NodeConfigurationSettings] = { (cfg, path) =>
    val stateTypeKey = s"$path.stateType"
    val stateType = stateTypeFromString(cfg.as[String](stateTypeKey), stateTypeKey)
    NodeConfigurationSettings(
      stateType,
      cfg.as[Boolean](s"$path.verifyTransactions"),
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[UtxoSettings](s"$path.utxo"),
      cfg.as[NipopowSettings](s"$path.nipopow"),
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
      cfg.as[SortingOption](s"$path.mempoolSorting"),
      cfg.as[Int](s"$path.rebroadcastCount"),
      cfg.as[Long](s"$path.minimalFeeAmount"),
      cfg.as[Int](s"$path.headerChainDiff"),
      cfg.as[Int](s"$path.adProofsSuffixLength"),
      cfg.as[Boolean](s"$path.extraIndex"),
      cfg.as[Seq[String]](s"$path.blacklistedTransactions"),
      cfg.as[Option[CheckpointSettings]](s"$path.checkpoint")
    )
  }

  implicit val sortingOptionReader: ValueReader[SortingOption] = { (cfg, path) =>
    val sorting = cfg.as[String](s"$path")
    sorting match {
      case "bySize" => SortingOption.FeePerByte
      case "byExecutionCost" => SortingOption.FeePerCycle
      case _ => SortingOption.random()
    }
  }

}
