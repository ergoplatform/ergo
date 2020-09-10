package org.ergoplatform.nodeView.wallet

import com.google.common.hash.BloomFilter
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.IdUtils.{EncodedBoxId, encodedBoxId}
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletRegistry}
import org.ergoplatform.nodeView.wallet.scanning.Scan
import org.ergoplatform.settings.{Constants, LaunchParameters}
import org.ergoplatform.wallet.Constants.{MiningScanId, PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, UnsignedErgoLikeTransaction, UnsignedInput}
import scorex.util.{ModifierId, ScorexLogging}
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.CostTable
import scorex.util.bytesToId
import scala.collection.mutable

/**
  * Functions which do scan boxes, transactions and blocks to find boxes which belong to wallet's keys.
  */
object WalletScanLogic extends ScorexLogging {

  //input tx id, input box id, tracked box
  case class InputData(inputTxId: ModifierId, inputBoxId: ModifierId, trackedBox: TrackedBox)

  //outputs, input ids, related transactions
  case class ScanResults(outputs: Seq[TrackedBox], inputsSpent: Seq[InputData], relatedTransactions: Seq[WalletTransaction])

  /**
    * Tries to prove the given box in order to define whether it could be spent by this wallet.
    *
    * todo: currently used only to decide that a box with mining rewards could be spent,
    * do special efficient method for that? The method would just use height, not doing signing.
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
                            block: ErgoFullBlock,
                            cachedOutputsFilter: Option[BloomFilter[Array[Byte]]]
                           ): (WalletRegistry, OffChainRegistry, BloomFilter[Array[Byte]]) = {
    scanBlockTransactions(
      registry, offChainRegistry, stateContext, walletVars,
      block.height, block.id, block.transactions, cachedOutputsFilter)
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
                            transactions: Seq[ErgoTransaction],
                            cachedOutputsFilter: Option[BloomFilter[Array[Byte]]]
                           ): (WalletRegistry, OffChainRegistry, BloomFilter[Array[Byte]]) = {

    // Take unspent wallet outputs Bloom Filter from cache
    // or recreate it from unspent outputs stored in the database
    val outputsFilter = cachedOutputsFilter.getOrElse {
      // todo: currently here and other places hardcoded values are used for Bloom filter parameters
      // todo: consider different profiles for the config, such as "user", "wallet", "app hub"
      val bf = WalletCache.emptyFilter(expectedKeys = 20000)

      registry.allUnspentBoxes().foreach { tb =>
        bf.put(tb.box.id)
      }
      bf
    }

    // Resolve boxes related to mining income
    val resolvedBoxes = registry.unspentBoxes(MiningScanId).flatMap { tb =>
      val spendable = resolve(tb.box, walletVars.proverOpt, stateContext, height)
      if (spendable) Some(tb.copy(scans = Set(PaymentsScanId))) else None
    }

    val initialScanResults = ScanResults(resolvedBoxes, Seq.empty, Seq.empty)

    // Wallet unspent outputs, we fetch them only when Bloom filter shows that some outputs may be spent
    val unspentBoxes = mutable.Map[ModifierId, TrackedBox]()

    // Flag which shows whether unspent boxes were fetched from the database
    var unspentRead = false

    val scanRes = transactions.foldLeft(initialScanResults) { case (scanResults, tx) =>

      // extract wallet- (and external scans) related outputs from the transaction
      val myOutputs = extractWalletOutputs(tx, Some(height), walletVars)

      // add extracted outputs to the filter
      myOutputs.foreach { out =>
        outputsFilter.put(out.box.id)
      }

      // Check with the Bloom filter that some outputs maybe spent
      // Bloom filter is a probabilistic data structure, with false-positives may happen
      // So we need to filter spendingInputs again
      val spendingInputs = tx.inputs.filter(inp =>
        outputsFilter.mightContain(inp.boxId)
      )

      if (myOutputs.nonEmpty || spendingInputs.nonEmpty) {

        val spentBoxes: Seq[TrackedBox] = if (spendingInputs.nonEmpty) {

          // Read unspent boxes before this block from the database
          if (!unspentRead) {
            registry.allUnspentBoxes().foreach { tb =>
              unspentBoxes += (tb.boxId -> tb)
            }
            unspentRead = true
          }

          // Filter-out false positives and read the boxes being spent
          spendingInputs.flatMap { inp =>
            val inpId = inp.boxId

            unspentBoxes.get(bytesToId(inpId)).flatMap { _ =>
              registry.getBox(inpId)
                .orElse(scanResults.outputs.find(tb => tb.box.id.sameElements(inpId)))
            }
          }
        } else {
          Seq.empty
        }

        // Add unspent boxes from this block
        if (myOutputs.nonEmpty) {
          myOutputs.foreach { tb =>
            unspentBoxes += (tb.boxId -> tb)
          }
        }

        // Scans related to the transaction
        val walletscanIds = (spentBoxes ++ myOutputs).flatMap(_.scans).toSet
        val wtx = WalletTransaction(tx, height, walletscanIds.toSeq)

        val inputsSpent = scanResults.inputsSpent ++ spentBoxes.map(t => InputData(tx.id, t.boxId, t))
        ScanResults(scanResults.outputs ++ myOutputs, inputsSpent, scanResults.relatedTransactions :+ wtx)
      } else {
        scanResults
      }
    }

    // function effects: updating registry and offchainRegistry datasets
    registry.updateOnBlock(scanRes, blockId, height)

    //data needed to update the offchain-registry
    val walletUnspent = registry.walletUnspentBoxes()
    val newOnChainIds = scanRes.outputs.map(x => encodedBoxId(x.box.id))
    val updatedOffchainRegistry = offChainRegistry.updateOnBlock(height, walletUnspent, newOnChainIds)

    (registry, updatedOffchainRegistry, outputsFilter)
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

      val statuses: Set[ScanId] = if (walletVars.filter.mightContain(boxScript)) {

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

  /**
    * Extracts all input boxes from the given transaction.
    */
  def extractInputBoxes(tx: ErgoTransaction): Seq[EncodedBoxId] = {
    tx.inputs.map(x => encodedBoxId(x.boxId))
  }

}
