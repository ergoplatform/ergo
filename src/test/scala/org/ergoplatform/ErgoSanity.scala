package org.ergoplatform

import io.circe
import org.ergoplatform.ErgoSanity._
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators
import org.scalacheck.Gen
import scorex.core.utils.NetworkTime
import scorex.testkit.properties._

//todo: currently this class parametrized with UtxoState, consider DigestState as well
class ErgoSanity extends HistoryAppendBlockTest[P, TX, PM, SI, HT]
  //with StateApplyChangesTest[P, TX, PM, B, ST]
  //with WalletSecretsTest[P, TX, PM]
  //with StateRollbackTest[P, TX, PM, B, ST, SI, HT, MPool]
  with MempoolTransactionsTest[P, TX, MPool]
  // todo: convert MempoolFilterPerformanceTest to benchmark
  //  with MempoolFilterPerformanceTest[P, TX, MPool]
  // with MempoolRemovalTest[P, TX, MPool, PM, HT, SI]
  //  with BoxStateChangesGenerationTest[P, TX, PM, B, ST, SI, HT]
  with ErgoGenerators {

  val settings: ErgoSettings = new ErgoSettings {
    override def settingsJSON: Map[String, circe.Json] = Map()
  }


  //Node view components
  override val history: ErgoHistory = ErgoHistory.readOrGenerate(settings)
  override val mempool: ErgoMemPool = ErgoMemPool.empty

  //Generators
  override val transactionGenerator: Gen[AnyoneCanSpendTransaction] = invalidAnyoneCanSpendTransactionGen

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
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(5)(0.toByte),
      Math.max(NetworkTime.time(), bestHeader.timestamp + 1)
    )
  }

}

object ErgoSanity {
  type P = AnyoneCanSpendProposition.type
  type TX = AnyoneCanSpendTransaction
  type B = AnyoneCanSpendNoncedBox
  type PM = ErgoPersistentModifier
  type SI = ErgoSyncInfo
  type HT = ErgoHistory
  type ST = UtxoState
  type MPool = ErgoMemPool
}
