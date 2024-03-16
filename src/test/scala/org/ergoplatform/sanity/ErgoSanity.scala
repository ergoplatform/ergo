package org.ergoplatform.sanity

import akka.actor.ActorRef
import org.ergoplatform.ErgoBox
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker}
import org.ergoplatform.nodeView.NodeViewSynchronizerTests
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.sanity.ErgoSanity._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.settings.Constants.HashLength
import scorex.testkit.generators.{ModifierProducerTemplateItem, SynInvalid, Valid}
import scorex.testkit.properties.HistoryTests
import scorex.testkit.properties.mempool.{MempoolRemovalTest, MempoolTransactionsTest}
import scorex.testkit.properties.state.StateApplicationTest
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.core.bytesToId
import org.scalacheck.Gen
import scorex.core.network.DeliveryTracker
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random

import scala.concurrent.ExecutionContext

trait ErgoSanity[ST <: ErgoState[ST]] extends NodeViewSynchronizerTests[ST]
  with StateApplicationTest[ST]
  with MempoolTransactionsTest
  with MempoolRemovalTest
  with HistoryTests
  with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  override val memPool: MPool = ErgoMemPool.empty(settings)

  //Generators
  override lazy val transactionGenerator: Gen[ErgoTransaction] = invalidErgoTransactionGen
  override lazy val unconfirmedTxGenerator: Gen[UnconfirmedTransaction] =
    invalidErgoTransactionGen.map(tx => UnconfirmedTransaction(tx, None))
  override lazy val memPoolGenerator: Gen[MPool] = emptyMemPoolGen

  override def syntacticallyValidModifier(history: HT): Header = {
    val bestTimestamp = history.bestHeaderOpt.map(_.timestamp + 1).getOrElse(System.currentTimeMillis())

    powScheme.prove(
      history.bestHeaderOpt,
      Header.InitialVersion,
      settings.chainSettings.initialNBits,
      ADDigest @@ Array.fill(HashLength + 1)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Math.max(System.currentTimeMillis(), bestTimestamp),
      Digest32 @@ Array.fill(HashLength)(0.toByte),
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get
  }

  override def syntacticallyInvalidModifier(history: HT): PM =
    syntacticallyValidModifier(history).copy(parentId = bytesToId(Random.randomBytes(32)))

  private val hf = Blake2b256

  def makeSyntacticallyInvalid(mod: PM): PM = mod match {
    case fb: ErgoFullBlock =>
      val parentId = fb.header.parentId
      val header = fb.header.copy(parentId = bytesToId(hf(parentId)))
      fb.copy(header = header)
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

  class SyncronizerMock(networkControllerRef: ActorRef,
                        viewHolderRef: ActorRef,
                        syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                        settings: ErgoSettings,
                        syncTracker: ErgoSyncTracker,
                        deliveryTracker: DeliveryTracker)
                       (implicit ec: ExecutionContext) extends ErgoNodeViewSynchronizer(
    networkControllerRef,
    viewHolderRef,
    syncInfoSpec,
    settings,
    syncTracker,
    deliveryTracker)(ec)

}

object ErgoSanity {
  type TX = ErgoTransaction
  type B = ErgoBox
  type PM = BlockSection
  type CTM = BlockTransactions
  type SI = ErgoSyncInfo
  type HT = ErgoHistory
  type UTXO_ST = UtxoState
  type DIGEST_ST = DigestState
  type MPool = ErgoMemPool

}
