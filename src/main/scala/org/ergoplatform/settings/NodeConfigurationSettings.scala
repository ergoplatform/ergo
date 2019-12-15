package org.ergoplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.nodeView.state.StateType

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo node regime
  *
  * @see src/main/resources/application.conf for parameters description
  */
final case class NodeConfigurationSettings(
   stateType: StateType,
   verifyTransactions: Boolean,
   blocksToKeep: Int,
   minimalSuffix: Int,
   mining: Boolean,
   maxTransactionComplexity: Int,
   miningDelay: FiniteDuration,
   useExternalMiner: Boolean,
   miningPubKeyHex: Option[String],
   offlineGeneration: Boolean,
   keepVersions: Int,
   mempoolCapacity: Int,
   blacklistCapacity: Int,
   mempoolCleanupDuration: FiniteDuration,
   rebroadcastCount: Int,
   minimalFeeAmount: Long,
   poPowSettings: PoPowSettings
)

object NodeConfigurationSettings extends StateTypeReaders with ModifierIdReader {

  implicit val reader: ValueReader[NodeConfigurationSettings] = { (cfg, path) =>
    val stateTypeKey = s"$path.stateType"
    val stateType = stateTypeFromString(cfg.as[String](stateTypeKey), stateTypeKey)
    NodeConfigurationSettings(
      stateType,
      cfg.as[Boolean](s"$path.verifyTransactions"),
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Int](s"$path.minimalSuffix"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[Int](s"$path.maxTransactionComplexity"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.useExternalMiner"),
      cfg.as[Option[String]](s"$path.miningPubKeyHex"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Int](s"$path.keepVersions"),
      cfg.as[Int](s"$path.mempoolCapacity"),
      cfg.as[Int](s"$path.blacklistCapacity"),
      cfg.as[FiniteDuration](s"$path.mempoolCleanupDuration"),
      cfg.as[Int](s"$path.rebroadcastCount"),
      cfg.as[Long](s"$path.minimalFeeAmount"),
      cfg.as[PoPowSettings](s"$path.poPow")
    )
  }

}
