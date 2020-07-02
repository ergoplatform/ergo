package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.WalletVars
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, decodedBoxId, encodedBoxId}
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry}
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.settings.{Constants, LaunchParameters}
import org.ergoplatform.wallet.Constants.{ScanId, MiningScanId, PaymentsScanId}
import org.ergoplatform.wallet.boxes.TrackedBox
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
    * todo: currently used only to decide that a box with mining rewards could be spent,
    *       do special efficient method for that? The method would just use height, not doing signing.
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


  /**
    * Updates wallet database by scanning and processing block transactions.
    */
  def scanBlockTransactions(registry: WalletRegistry,
                            offChainRegistry: OffChainRegistry,
                            stateContext: ErgoStateContext,
                            walletVars: WalletVars,
                            height: Int,
                            blockId: ModifierId,
                            transactions: Seq[ErgoTransaction]): (WalletRegistry, OffChainRegistry) = {

    //todo: inefficient for wallets with many outputs, replace with a Bloom/Cuckoo filter?
    val previousBoxIds = registry.walletUnspentBoxes().map(tb => encodedBoxId(tb.box.id))

    val resolvedBoxes = registry.unspentBoxes(MiningScanId).flatMap { tb =>
      val spendable = resolve(tb.box, walletVars.proverOpt, stateContext, height)
      if (spendable) Some(tb.copy(scans = Set(PaymentsScanId))) else None
    }

    //input tx id, input box id, tracked box
    type InputData = Seq[(ModifierId, EncodedBoxId, TrackedBox)]
    //outputs, input ids, related transactions
    type ScanResults = (Seq[TrackedBox], InputData, Seq[WalletTransaction])
    val initialScanResults: ScanResults = (resolvedBoxes, Seq.empty, Seq.empty)

    val scanRes = transactions.foldLeft((initialScanResults, previousBoxIds)) { case ((scanResults, accBoxIds), tx) =>
      val txInputIds = tx.inputs.map(x => encodedBoxId(x.boxId))
      val myOutputs = extractWalletOutputs(tx, Some(height), walletVars)

      val boxIds: Seq[EncodedBoxId] = accBoxIds ++ myOutputs.map(x => EncodedBoxId @@ x.boxId)
      val spendingInputIds = txInputIds.filter(x => boxIds.contains(x))

      if (myOutputs.nonEmpty || spendingInputIds.nonEmpty) {
        val spentBoxes = spendingInputIds.map { inpId =>
          registry.getBox(decodedBoxId(inpId))
            .orElse(scanResults._1.find(tb => tb.box.id.sameElements(decodedBoxId(inpId)))).get //todo: .get
        }

        // Scans related to the transaction
        val walletscanIds = (spentBoxes ++ myOutputs).flatMap(_.scans).toSet
        val wtx = WalletTransaction(tx, height, walletscanIds.toSeq)

        val newRel = (scanResults._2: InputData) ++ spendingInputIds.zip(spentBoxes).map(t => (tx.id, t._1, t._2))
        (scanResults._1 ++ myOutputs, newRel, scanResults._3 :+ wtx) -> boxIds
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
    val updatedOffchainRegistry = offChainRegistry.updateOnBlock(height, walletUnspent, newOnChainIds)

    registry -> updatedOffchainRegistry
  }


  /**
    * Extracts all outputs which contain tracked bytes from the given transaction.
    */
  def extractWalletOutputs(tx: ErgoTransaction,
                           inclusionHeight: Option[Int],
                           walletVars: WalletVars): Seq[TrackedBox] = {

    val trackedBytes: Seq[Array[Byte]] = walletVars.trackedBytes
    val miningScriptsBytes: Seq[Array[Byte]] = walletVars.miningScriptsBytes
    val externalScans: Seq[Scan] = walletVars.externalScans

    tx.outputs.flatMap { bx =>
      val appsTriggered = externalScans.filter(_.trackingRule.filter(bx)).map(app => app.scanId)

      val boxScript = bx.propositionBytes

      val statuses: Set[ScanId] = if (walletVars.filter.lookup(boxScript)) {

        val miningIncomeTriggered = miningScriptsBytes.exists(ms => boxScript.sameElements(ms))

        //tweak for tests
        lazy val miningStatus: ScanId = if (walletVars.settings.miningRewardDelay > 0) {
          MiningScanId
        } else {
          PaymentsScanId
        }

        val prePaymentStatuses = if (miningIncomeTriggered) appsTriggered :+ miningStatus else appsTriggered

        if (prePaymentStatuses.nonEmpty) {
          //if other scans intercept the box, it is not being tracked by the payments app
          prePaymentStatuses.toSet
        } else {
          val paymentsTriggered = trackedBytes.exists(bs => boxScript.sameElements(bs))

          if (paymentsTriggered) {
            Set(PaymentsScanId)
          } else {
            Set.empty
          }
        }
      } else {
        appsTriggered.toSet
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
