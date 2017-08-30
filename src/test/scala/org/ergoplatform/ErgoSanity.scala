package org.ergoplatform

import java.io.File

import org.ergoplatform.ErgoSanity._
import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.WrappedUtxoState
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, HistorySpecification}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.ErgoGenerators
import org.scalacheck.Gen
import scorex.core.utils.NetworkTime
import scorex.testkit.properties._
import scorex.testkit.properties.mempool.MempoolTransactionsTest
import scorex.testkit.properties.state.StateApplicationTest
import scorex.utils.Random

//todo: currently this class parametrized with UtxoState, consider DigestState as well
class ErgoSanity extends HistoryAppendBlockTest[P, TX, PM, SI, HT]
  with StateApplicationTest[PM, ST]
  //with StateApplyChangesTest[P, TX, PM, B, ST]
  //with WalletSecretsTest[P, TX, PM]
  //with StateRollbackTest[P, TX, PM, B, ST, SI, HT, MPool]
  with MempoolTransactionsTest[P, TX, MPool]
  // todo: convert MempoolFilterPerformanceTest to benchmark
  //  with MempoolFilterPerformanceTest[P, TX, MPool]
  // with MempoolRemovalTest[P, TX, MPool, PM, HT, SI]
  //  with BoxStateChangesGenerationTest[P, TX, PM, B, ST, SI, HT]
  with ErgoGenerators
  with HistorySpecification {

  lazy val settings: ErgoSettings = ErgoSettings.read(None)

  //Node view components
  override val history: ErgoHistory = generateHistory(verifyTransactions = true, ADState = false,
    PoPoWBootstrap = false, -1)

  override val memPool: MPool = ErgoMemPool.empty

  //Generators
  override val transactionGenerator: Gen[AnyoneCanSpendTransaction] = invalidAnyoneCanSpendTransactionGen
  
  override def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(NetworkTime.time())

    Miner.genHeader(Constants.InitialNBits,
      history.bestHeaderOpt,
      Array.fill(hashLength + 1)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(5)(0.toByte),
      Math.max(NetworkTime.time(), bestTimestamp)
    )
  }

  override def syntacticallyInvalidModifier(history: HT): PM =
    syntacticallyValidModifier(history).copy(parentId = Random.randomBytes(32))

  override val stateGen: Gen[WrappedUtxoState] = boxesHolderGen.map{bh =>
    val f = new File(s"/tmp/ergo/${scala.util.Random.nextInt(100000)}")
    f.mkdirs()
    WrappedUtxoState(bh, f)
  }

  override def semanticallyValidModifier(state: ST): PM = validFullBlock(None, state.asInstanceOf[WrappedUtxoState])

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
