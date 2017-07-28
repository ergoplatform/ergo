package org.ergoplatform

import io.circe
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.ErgoSettings
import org.scalacheck.Gen
import scorex.core.transaction.state.BoxStateChanges
import scorex.core.utils.NetworkTime
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}

//todo: currently this class parametrized with UtxoState, consider DigestState as well
class ErgoSanity extends BlockchainSanity[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  UtxoState,
  ErgoHistory] with BlockchainPerformance[AnyoneCanSpendProposition,
  AnyoneCanSpendTransaction,
  ErgoPersistentModifier,
  ErgoSyncInfo,
  AnyoneCanSpendNoncedBox,
  ErgoMemPool,
  UtxoState,
  ErgoHistory] with ErgoGenerators {

  val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }


  //Node view components
  override val history: ErgoHistory = ErgoHistory.readOrGenerate(settings)
  override val mempool: ErgoMemPool = ErgoMemPool.empty
  override val wallet: ErgoWallet = ErgoWallet.readOrGenerate(settings)
  override val state = ErgoState.readOrGenerate(settings).asInstanceOf[UtxoState]

  //Generators
  override val transactionGenerator: Gen[AnyoneCanSpendTransaction] = anyoneCanSpendTransactionGen

  override val stateChangesGenerator: Gen[BoxStateChanges[AnyoneCanSpendProposition, AnyoneCanSpendNoncedBox]] = stateChangesGen

  //todo: fix, last 2 params are ignored for now
  override def genValidModifier(history: ErgoHistory,
                                mempoolTransactionFetchOption: Boolean,
                                noOfTransactionsFromMempool: Int): Header = {
    val bestHeader: Header = history.bestHeader
    Miner.genHeader(BigInt(1),
      bestHeader,
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      NetworkTime.time())
  }

  override def genValidTransactionPair(curHistory: ErgoHistory): Seq[AnyoneCanSpendTransaction] = ???

  override def genValidModifierCustomTransactions(curHistory: ErgoHistory, trx: AnyoneCanSpendTransaction): ErgoPersistentModifier = ???
}
