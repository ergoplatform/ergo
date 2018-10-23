package org.ergoplatform.utils.generators

import akka.actor.ActorRef
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ExtensionCandidate, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.Matchers
import scorex.core.VersionTag
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.testkit.TestkitHelpers
import scorex.testkit.utils.FileUtils
import sigmastate.Values.TrueLeaf
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.annotation.tailrec
import scala.util.{Random, Try}

trait ValidBlocksGenerators
  extends TestkitHelpers with FileUtils with Matchers with ChainGenerator with ErgoTransactionGenerators {

  def initSettings: ErgoSettings = ErgoSettings.read(None)

  lazy val settings: ErgoSettings = initSettings
  lazy val stateConstants: StateConstants = StateConstants(None, settings)

  lazy val genesisEmissionBox = ErgoState.genesisEmissionBox(settings.emission)

  def createUtxoState(nodeViewHolderRef: Option[ActorRef] = None): (UtxoState, BoxHolder) = {
    val constants = StateConstants(nodeViewHolderRef, settings)
    ErgoState.generateGenesisUtxoState(createTempDir, constants)
  }

  def createUtxoState(bh: BoxHolder): UtxoState =
    UtxoState.fromBoxHolder(bh, None, createTempDir, stateConstants)

  def createDigestState(version: VersionTag, digest: ADDigest): DigestState =
    DigestState.create(Some(version), Some(digest), createTempDir, ErgoSettings.read(None))

  def noProofInput(id: ErgoBox.BoxId): Input =
    Input(id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

  def outputForAnyone(value: Long): ErgoBoxCandidate = new ErgoBoxCandidate(value, TrueLeaf)

  def validTransactionsFromBoxHolder(boxHolder: BoxHolder): (Seq[ErgoTransaction], BoxHolder) =
    validTransactionsFromBoxHolder(boxHolder, new Random)

  /** @param sizeLimit maximum transactions size in bytes */
  protected def validTransactionsFromBoxes(sizeLimit: Int,
                                           stateBoxesIn: Seq[ErgoBox],
                                           rnd: Random): (Seq[ErgoTransaction], Seq[ErgoBox]) = {
    val outBoxesLength = stateBoxesIn.length
    var createdEmissionBox: Seq[ErgoBox] = Seq()

    @tailrec
    def loop(stateBoxes: Seq[ErgoBox],
             selfBoxes: Seq[ErgoBox],
             acc: Seq[ErgoTransaction],
             rnd: Random): (Seq[ErgoTransaction], Seq[ErgoBox]) = {

      def genOuts(remainingAmount: Long,
                  acc: IndexedSeq[ErgoBoxCandidate],
                  limit: Int): IndexedSeq[ErgoBoxCandidate] = {
        val newAmount = remainingAmount / limit
        if (newAmount >= remainingAmount || limit <= 1) {
          acc :+ outputForAnyone(remainingAmount)
        } else {
          genOuts(remainingAmount - newAmount, acc :+ outputForAnyone(newAmount), limit - 1)
        }
      }

      val currentSize = acc.map(_.bytes.length).sum
      val averageSize = if (currentSize > 0) currentSize / acc.length else 1000

      stateBoxes.find(isEmissionBox) match {
        case Some(emissionBox) if currentSize < sizeLimit - averageSize =>
          // Extract money to anyoneCanSpend output and put emission to separate var to avoid it's double usage inside one block
          val height: Int = (emissionBox.additionalRegisters(R4).value.asInstanceOf[Long] + 1).toInt
          val tx = ErgoMiner.createCoinbase(Some(emissionBox), height, Seq.empty, TrueLeaf, settings.emission)
          val remainedBoxes = stateBoxes.filter(b => !isEmissionBox(b))
          createdEmissionBox = tx.outputs.filter(b => isEmissionBox(b))
          val newSelfBoxes = selfBoxes ++ tx.outputs.filter(b => !isEmissionBox(b))
          loop(remainedBoxes, newSelfBoxes, tx +: acc, rnd)

        case _ =>
          if (currentSize < sizeLimit - 2 * averageSize) {
            val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(Try(rnd.nextInt(selfBoxes.size) + 1).getOrElse(0))
            val (consumedBoxesFromState, remainedBoxes) = stateBoxes.splitAt(Try(rnd.nextInt(stateBoxes.size) + 1).getOrElse(0))
            val inputs = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.id).map(noProofInput).toIndexedSeq
            assert(inputs.nonEmpty, "Trying to create transaction with no inputs")
            val totalAmount = (consumedSelfBoxes ++ consumedBoxesFromState).map(_.value).sum
            val outputs = genOuts(totalAmount, IndexedSeq.empty, rnd.nextInt(outBoxesLength) + 1)
            val tx = new ErgoTransaction(inputs, outputs)
            loop(remainedBoxes, remainedSelfBoxes ++ tx.outputs, tx +: acc, rnd)
          } else {
            // take all remaining boxes from state and return transactions set
            val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(1)
            val inputs = (consumedSelfBoxes ++ stateBoxes).map(_.id).map(noProofInput).toIndexedSeq
            assert(inputs.nonEmpty, "Trying to create transaction with no inputs")
            val totalAmount = (consumedSelfBoxes ++ stateBoxes).map(_.value).sum
            val outputs = genOuts(totalAmount, IndexedSeq.empty, rnd.nextInt(outBoxesLength) + 1)
            val tx = new ErgoTransaction(inputs, outputs)
            ((tx +: acc).reverse, remainedSelfBoxes ++ tx.outputs ++ createdEmissionBox)
          }
      }
    }

    loop(stateBoxesIn, Seq.empty, Seq.empty, rnd)
  }

  /**
    * Simplified check that box is an emission box
    */
  private def isEmissionBox(box: ErgoBox): Boolean = box.proposition == genesisEmissionBox.proposition

  /** @param txSizeLimit maximum transactions size in bytes */
  def validTransactionsFromBoxHolder(boxHolder: BoxHolder,
                                     rnd: Random,
                                     txSizeLimit: Int = 10 * 1024): (Seq[ErgoTransaction], BoxHolder) = {
    val (emissionBox, boxHolderWithoutEmission) = boxHolder.take(b => isEmissionBox(b))
    val (regularBoxes, drainedBh) = boxHolderWithoutEmission.take(rnd.nextInt(txSizeLimit / 100) + 1)
    val boxes = emissionBox ++ regularBoxes
    assert(boxes.nonEmpty, s"Was unable to take at least 1 box from box holder $boxHolder")
    val (txs, createdBoxes) = validTransactionsFromBoxes(txSizeLimit, boxes, rnd)
    txs.foreach(_.statelessValidity.get)
    val bs = new BoxHolder(drainedBh.boxes ++ createdBoxes.map(b => ByteArrayWrapper(b.id) -> b))
    txs -> bs
  }


  def validTransactionsFromUtxoState(wus: WrappedUtxoState, rnd: Random = new Random): Seq[ErgoTransaction] = {
    val num = 1 + rnd.nextInt(10)

    val allBoxes = wus.takeBoxes(num + rnd.nextInt(100))
    val anyoneCanSpendBoxes = allBoxes.filter(_.proposition == TrueLeaf)
    val boxes = if (anyoneCanSpendBoxes.nonEmpty) anyoneCanSpendBoxes else allBoxes

    validTransactionsFromBoxes(num, boxes, rnd)._1
  }

  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder): ErgoFullBlock =
    validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, new Random)


  def validFullBlock(parentOpt: Option[Header], utxoState: UtxoState, boxHolder: BoxHolder, rnd: Random): ErgoFullBlock = {
    validFullBlock(parentOpt, utxoState, validTransactionsFromBoxHolder(boxHolder, rnd)._1)
  }

  def validFullBlockWithBlockHolder(parentOpt: Option[Header],
                                    utxoState: UtxoState,
                                    boxHolder: BoxHolder,
                                    rnd: Random): (ErgoFullBlock, BoxHolder) = {
    val txsBh = validTransactionsFromBoxHolder(boxHolder, rnd)
    validFullBlock(parentOpt, utxoState, txsBh._1) -> txsBh._2
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: WrappedUtxoState): ErgoFullBlock = {
    validFullBlock(parentOpt, utxoState, validTransactionsFromUtxoState(utxoState))
  }

  def validFullBlock(parentOpt: Option[Header],
                     utxoState: UtxoState,
                     transactions: Seq[ErgoTransaction],
                     timeOpt: Option[Long] = None): ErgoFullBlock = {
    transactions.foreach(_.statelessValidity shouldBe 'success)
    transactions.nonEmpty shouldBe true
    ErgoState.boxChanges(transactions)._1.foreach { boxId: ADKey =>
      assert(utxoState.boxById(boxId).isDefined, s"Box ${Algos.encode(boxId)} missed")
    }

    val (adProofBytes, updStateDigest) = utxoState.proofsForTransactions(transactions).get

    val time = timeOpt.getOrElse(timeProvider.time())
    val extension: ExtensionCandidate = defaultExtension

    DefaultFakePowScheme.proveBlock(parentOpt, Constants.InitialNBits, updStateDigest, adProofBytes,
      transactions, time, extension, defaultMinerSecret).get
  }
}
