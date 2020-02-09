package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.WalletVars
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, decodedBoxId, encodedBoxId}
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry}
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings.{Constants, LaunchParameters}
import org.ergoplatform.wallet.Constants.{MiningRewardsAppId, PaymentsAppId}
import org.ergoplatform.wallet.boxes.{BoxCertainty, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.CostTable

/**
  * Functions which do scan boxes, transactions and blocks to find boxes which do really belong to wallet's keys.
  */
object WalletScanLogic extends ScorexLogging {

  /**
    * Tries to prove the given box in order to define whether it could be spent by this wallet.
    *
    * todo: currently used only to decide that a box with mining rewards could be spent, do special method for that?
    */
  private def resolve(box: ErgoBox,
              proverOpt: Option[ErgoProvingInterpreter],
              stateContext: ErgoStateContext,
              height: Int): Boolean = {
    val testingTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(box.id)),
      IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
    )

    val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
    val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty,
      LaunchParameters.maxBlockCost, CostTable.interpreterInitCost)

    proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption).isDefined
  }

  def scanBlockTransactions(registry: WalletRegistry,
                            offChainRegistry: OffChainRegistry,
                            stateContext: ErgoStateContext,
                            walletVars: WalletVars,
                            height: Int,
                            blockId: ModifierId,
                            transactions: Seq[ErgoTransaction]): (WalletRegistry, OffChainRegistry) = {

    //todo: replace with Bloom filter?
    val previousBoxIds = registry.walletUnspentBoxes().map(tb => encodedBoxId(tb.box.id))

    val resolvedBoxes = registry.uncertainBoxes(MiningRewardsAppId).flatMap { tb =>
      //todo: more efficient resolving, just by using height
      val spendable = resolve(tb.box, walletVars.proverOpt, stateContext, height)
      if (spendable) Some(tb.copy(applicationStatuses = Map(PaymentsAppId -> BoxCertainty.Certain))) else None
    }

    //outputs, input ids, related transactions
    type ScanResults = (Seq[TrackedBox], Seq[(ModifierId, EncodedBoxId, TrackedBox)], Seq[WalletTransaction])
    val initialScanResults: ScanResults = (resolvedBoxes, Seq.empty, Seq.empty)

    val scanRes = transactions.foldLeft((initialScanResults, previousBoxIds)) { case ((scanResults, accBoxIds), tx) =>
      val txInputIds = tx.inputs.map(x => encodedBoxId(x.boxId))
      val outputs = extractWalletOutputs(tx, Some(height), walletVars)

      val boxIds: Seq[EncodedBoxId] = accBoxIds ++ outputs.map(x => EncodedBoxId @@ x.boxId)
      val relatedInputIds = txInputIds.filter(x => boxIds.contains(x))

      if (outputs.nonEmpty || relatedInputIds.nonEmpty) {
        val spentBoxes = relatedInputIds.map { inpId =>
          registry.getBox(decodedBoxId(inpId))
            .orElse(scanResults._1.find(tb => tb.box.id.sameElements(decodedBoxId(inpId)))).get //todo: .get
        }
        val walletAppIds = (spentBoxes ++ outputs).flatMap(_.applicationStatuses.keys).toSet
        val wtx = WalletTransaction(tx, height, walletAppIds.toSeq)

        val newRel = (scanResults._2: Seq[(ModifierId, EncodedBoxId, TrackedBox)]) ++
          relatedInputIds.zip(spentBoxes).map(t => (tx.id, t._1, t._2))
        (scanResults._1 ++ outputs, newRel, scanResults._3 :+ wtx) -> boxIds
      } else {
        scanResults -> accBoxIds
      }
    }._1

    val outputs = scanRes._1
    val inputs = scanRes._2
    val affectedTransactions = scanRes._3

    // function effects: updating registry and offchainRegistry datasets
    registry.updateOnBlock(outputs, inputs, affectedTransactions)(blockId, height)

    //data needed to update the offchain-registry
    val walletUnspent = registry.walletUnspentBoxes()
    val newOnChainIds = outputs.map(x => encodedBoxId(x.box.id))

    registry -> offChainRegistry.updateOnBlock(height, walletUnspent, newOnChainIds)
  }


  /**
    * Extracts all outputs which contain tracked bytes from the given transaction.
    */
  def extractWalletOutputs(tx: ErgoTransaction,
                           inclusionHeight: Option[Int],
                           walletVars: WalletVars): Seq[TrackedBox] = {

    val trackedBytes: Seq[Array[Byte]] = walletVars.trackedBytes
    val miningScriptsBytes: Seq[Array[Byte]] = walletVars.miningScriptsBytes
    val externalApplications: Seq[ExternalApplication] = walletVars.externalApplications

    tx.outputs.flatMap { bx =>
      val appsTriggered = externalApplications.filter(_.trackingRule.filter(bx))
        .map(app => app.appId -> app.initialCertainty)
        .toMap

      val miningIncomeTriggered = miningScriptsBytes.exists(ms => bx.propositionBytes.sameElements(ms))

      //tweak for tests
      lazy val miningStatus: (AppId, BoxCertainty) = if (walletVars.minerRewardDelay > 0) {
        MiningRewardsAppId -> BoxCertainty.Certain
      } else {
        PaymentsAppId -> BoxCertainty.Certain
      }

      val prePaymentStatuses = if (miningIncomeTriggered) appsTriggered + miningStatus else appsTriggered

      val statuses: Map[AppId, BoxCertainty] = if (prePaymentStatuses.nonEmpty) {
        prePaymentStatuses
      } else {
        val paymentsTriggered = trackedBytes.exists(bs => bx.propositionBytes.sameElements(bs))

        if (paymentsTriggered) {
          Map(PaymentsAppId -> BoxCertainty.Certain)
        } else {
          Map.empty
        }
      }

      if (statuses.nonEmpty) {
        val tb = TrackedBox(tx.id, bx.index, inclusionHeight, None, None, bx, statuses)
        log.debug("New tracked box: " + tb.boxId)
        Some(tb)
      } else {
        None
      }
    }
  }

}
