package org.ergoplatform

import io.circe
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.transaction.state.StateChanges
import scorex.core.utils.NetworkTime
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}

class ErgoSanity extends BlockchainSanity[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  ErgoState,
  ErgoHistory] with BlockchainPerformance[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  ErgoState,
  ErgoHistory] with ErgoGenerators {

  val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }


  //Node view components
  override val history: ErgoHistory = ErgoHistory.readOrGenerate(settings)
  override val mempool: ErgoMemPool = ErgoMemPool.empty
  override val wallet: ErgoWallet = ErgoWallet.readOrGenerate(settings)
  override val state: ErgoState = ErgoState.readOrGenerate(settings)

  //Generators
  override val transactionGenerator: Gen[AnyoneCanSpendTransaction] = anyoneCanSpendTransactionGen

  override val stateChangesGenerator: Gen[StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = stateChangesGen

  override def genValidModifier(history: ErgoHistory): Header = {
    val bestHeader: Header = ???
    Miner.genHeader(BigInt(1),
      bestHeader,
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      NetworkTime.time())
  }
}
