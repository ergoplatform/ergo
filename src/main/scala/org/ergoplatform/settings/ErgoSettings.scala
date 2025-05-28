package org.ergoplatform.settings

import org.ergoplatform.mining.groupElemFromBytes
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.util.encode.Base16
import sigma.data.ProveDlog

import scala.util.Try

case class ErgoSettings(directory: String,
                        networkType: NetworkType,
                        chainSettings: ChainSettings,
                        nodeSettings: NodeConfigurationSettings,
                        scorexSettings: ScorexSettings,
                        walletSettings: WalletSettings,
                        cacheSettings: CacheSettings,
                        votingTargets: VotingTargets = VotingTargets.empty) {

  val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(chainSettings.addressPrefix)

  val miningRewardDelay: Int = chainSettings.monetary.minerRewardDelay

  val miningPubKey: Option[ProveDlog] = nodeSettings.miningPubKeyHex
    .flatMap { str =>
      val keyBytes = Base16.decode(str)
        .getOrElse(throw new Error(s"Failed to parse `miningPubKeyHex = ${nodeSettings.miningPubKeyHex}`"))
      Try(ProveDlog(groupElemFromBytes(keyBytes)))
        .orElse(addressEncoder.fromString(str).collect { case p2pk: P2PKAddress => p2pk.pubkey })
        .toOption
    }

  /**
    * @return blockchain parameters at the genesis block
    */
  def launchParameters: Parameters = {
    if (networkType == NetworkType.DevNet) {
      DevnetLaunchParameters
    } else if (networkType == NetworkType.TestNet) {
      TestnetLaunchParameters
    } else {
      MainnetLaunchParameters
    }
  }

}

object ErgoSettings {
  val configPath: String = "ergo"
  val scorexConfigPath: String = "scorex"
}
