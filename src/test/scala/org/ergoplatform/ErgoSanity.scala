package org.ergoplatform

import io.circe
import org.ergoplatform.modifiers.block.ErgoBlock
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.transaction.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.transaction.state.StateChanges
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}

class ErgoSanity extends BlockchainSanity[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoBlock,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  ErgoState,
  ErgoHistory] with BlockchainPerformance[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoBlock,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  ErgoState,
  ErgoHistory] with ErgoGenerators {

  val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }


  //Node view components
  override val history = ErgoHistory.readOrGenerate(settings)
  override val mempool = ErgoMemPool.empty
  override val wallet = ErgoWallet.readOrGenerate(settings)
  override val state = ErgoState.readOrGenerate(settings)

  //Generators
  override val transactionGenerator: Gen[AnyoneCanSpendTransaction] = anyoneCanSpendTransactionGen

  override val stateChangesGenerator: Gen[StateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = stateChangesGen

  override def genValidModifier(history: ErgoHistory): ErgoBlock = ???
}
