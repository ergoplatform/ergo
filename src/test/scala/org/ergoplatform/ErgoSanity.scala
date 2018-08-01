package org.ergoplatform

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoSanity._
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.utils.{ErgoTestHelpers, HistorySpecification}
import org.scalacheck.Gen
import scorex.core.{bytesToId, idToBytes, idToVersion}
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.testkit.generators.{ModifierProducerTemplateItem, SynInvalid, Valid}
import scorex.testkit.properties._
import scorex.testkit.properties.mempool.MempoolTransactionsTest
import scorex.testkit.properties.state.StateApplicationTest
import scorex.utils.Random

import scala.concurrent.ExecutionContext

//todo: currently this class parametrized with UtxoState, consider DigestState as well
trait ErgoSanity[ST <: MinimalState[PM, ST]] extends HistoryTests[TX, PM, SI, HT]
  with StateApplicationTest[PM, ST]
  //with StateApplyChangesTest[P, TX, PM, B, ST]
  //with WalletSecretsTest[P, TX, PM]
  //with StateRollbackTest[P, TX, PM, B, ST, SI, HT, MPool]
  with MempoolTransactionsTest[TX, MPool]
  // todo: convert MempoolFilterPerformanceTest to benchmark
  //with MempoolFilterPerformanceTest[P, TX, MPool]
  //with MempoolRemovalTest[P, TX, MPool, PM, PM, HT, SI]
  //with BoxStateChangesGenerationTest[P, TX, PM, B, ST]
  with NodeViewHolderTests[TX, PM, ST, SI, HT, MPool]
  with ErgoTestHelpers
  with HistorySpecification {

  //Node view components
  //override val historyGen: Gen[HT] = generateHistory(verifyTransactions = true, StateType.Utxo,
  //PoPoWBootstrap = false, -1)

  override val memPool: MPool = ErgoMemPool.empty

  //Generators
  override lazy val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
  override lazy val memPoolGenerator: Gen[MPool] = emptyMemPoolGen

  override def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(timeProvider.time())

    DefaultFakePowScheme.prove(
      history.bestHeaderOpt,
      Constants.InitialNBits,
      ADDigest @@ Array.fill(hashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(hashLength)(0.toByte),
      Digest32 @@ Array.fill(hashLength)(0.toByte),
      Math.max(timeProvider.time(), bestTimestamp),
      Digest32 @@ Array.fill(hashLength)(0.toByte)
    ).get
  }

  override def syntacticallyInvalidModifier(history: HT): PM =
    syntacticallyValidModifier(history).copy(parentId = bytesToId(Random.randomBytes(32)))

  private val hf = Blake2b256

  def makeSyntacticallyInvalid(mod: PM): PM = mod match {
    case fb: ErgoFullBlock => {
      val parentId = fb.header.parentId
      val header = fb.header.copy(parentId = bytesToId(hf(parentId)))
      fb.copy(header = header)
    }
    case h: Header => h.copy(parentId = bytesToId(hf(h.parentId)))
    case v => v
  }

  def customModifiers(history: HT,
                      state: ST,
                      template: Seq[ModifierProducerTemplateItem]): Seq[PM] =
    template.zip(totallyValidModifiers(history, state, template.length))
      .map { case (templateItem, mod) =>
        templateItem match {
          case Valid => mod
          case SynInvalid => makeSyntacticallyInvalid(mod)
        }
      }

}

object ErgoSanity {
  type TX = ErgoTransaction
  type B = ErgoBox
  type PM = ErgoPersistentModifier
  type SI = ErgoSyncInfo
  type HT = ErgoHistory
  type UTXO_ST = UtxoState
  type DIGEST_ST = DigestState
  type MPool = ErgoMemPool
}
