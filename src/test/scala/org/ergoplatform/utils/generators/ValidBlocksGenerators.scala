package org.ergoplatform.utils.generators

import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings, Parameters}
import org.ergoplatform.utils.{LoggingUtil, RandomLike, RandomWrapper}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalatest.matchers.should.Matchers
import org.ergoplatform.core.VersionTag
import scorex.crypto.authds.avltree.batch.Remove
import scorex.crypto.authds.ADDigest
import scorex.db.ByteArrayWrapper
import scorex.testkit.TestkitHelpers
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Random, Success}

object ValidBlocksGenerators
  extends TestkitHelpers with TestFileUtils with Matchers with ScorexLogging {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  def createUtxoState(settings: ErgoSettings, parametersOpt: Option[Parameters] = None): (UtxoState, BoxHolder) = {
    ErgoState.generateGenesisUtxoState(createTempDir, settings, parametersOpt)
  }

  def createUtxoState(bh: BoxHolder, parameters: Parameters): UtxoState =
    UtxoState.fromBoxHolder(bh, None, createTempDir, settings, parameters)

  def createDigestState(version: VersionTag, digest: ADDigest): DigestState =
    DigestState.create(Some(version), Some(digest), createTempDir, settings)

  def validTransactionsFromBoxHolder(boxHolder: BoxHolder): (Seq[ErgoTransaction], BoxHolder) =
    validTransactionsFromBoxHolder(boxHolder, new RandomWrapper)

  def validTransactionsFromBoxes(sizeLimit: Int,
                                           stateBoxesIn: Seq[ErgoBox],
                                           rnd: RandomLike): (Seq[ErgoTransaction], Seq[ErgoBox]) = {
    validTransactionsFromBoxes(sizeLimit, stateBoxesIn, Seq(), rnd)
  }

  /** @param sizeLimit maximum transactions size in bytes
    * @return generated transactions and unspent outputs
    * */
  def validTransactionsFromBoxes(sizeLimit: Int,
                                 stateBoxesIn: Seq[ErgoBox],
                                 dataBoxesIn: Seq[ErgoBox],
                                 rnd: RandomLike): (Seq[ErgoTransaction], Seq[ErgoBox]) = {
    var createdEmissionBox: Seq[ErgoBox] = Seq()

    @tailrec
    def loop(remainingCost: Long,
             stateBoxes: Seq[ErgoBox],
             selfBoxes: Seq[ErgoBox],
             acc: Seq[ErgoTransaction],
             rnd: RandomLike): (Seq[ErgoTransaction], Seq[ErgoBox]) = {

      lazy val dataBoxesToUse: IndexedSeq[ErgoBox] = {
        Random.shuffle(dataBoxesIn ++ stateBoxesIn ++ selfBoxes).take(rnd.nextInt(10)).toIndexedSeq
      }

      val currentSize = acc.map(_.size).sum
      val averageSize = if (currentSize > 0) currentSize / acc.length else 1000
      val customTokens = (stateBoxes ++ selfBoxes).flatMap(_.additionalTokens.toArray)
      val customTokensNum = customTokens.map(ct => ByteArrayWrapper(ct._1.toArray)).toSet.size
      val issueNew = customTokensNum == 0

      stateBoxes.find(isEmissionBox) match {
        case Some(emissionBox) if currentSize < sizeLimit - averageSize =>
          // Extract money to anyoneCanSpend output and put emission to separate var to avoid it's double usage inside one block
          val currentHeight: Int = emissionBox.creationHeight.toInt
          val rewards = CandidateGenerator.collectRewards(Some(emissionBox), currentHeight, Seq.empty, defaultMinerPk, emptyStateContext)
          val outs = rewards.flatMap(_.outputs)
          val remainedBoxes = stateBoxes.filter(b => !isEmissionBox(b))
          createdEmissionBox = outs.filter(b => isEmissionBox(b))
          val newSelfBoxes = selfBoxes ++ outs.filter(b => !isEmissionBox(b))
          val cost: Long = getTxCost(rewards.head, Seq(emissionBox), Seq())
          loop(remainingCost - cost, remainedBoxes, newSelfBoxes, rewards ++ acc, rnd)

        case _ =>
          if (currentSize < sizeLimit - 2 * averageSize && remainingCost > 100000) {
            val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(rnd.nextInt(2) + 1)
            val (consumedBoxesFromState, remainedBoxes) = stateBoxes.splitAt(rnd.nextInt(2) + 1)
            // disable tokens generation to avoid situation with too many tokens
            val boxesToSpend = (consumedSelfBoxes ++ consumedBoxesFromState).toIndexedSeq
            val tx = validTransactionFromBoxes(boxesToSpend, rnd, issueNew, dataBoxes = dataBoxesToUse)
            tx.statelessValidity() match {
              case Failure(e) =>
                log.warn(s"Failed to generate valid transaction: ${LoggingUtil.getReasonMsg(e)}")
                loop(remainingCost, stateBoxes, selfBoxes, acc, rnd)
              case _ =>
                val cost: Long = getTxCost(tx, boxesToSpend, dataBoxesToUse)
                loop(remainingCost - cost, remainedBoxes, remainedSelfBoxes ++ tx.outputs, tx +: acc, rnd)
            }
          } else {
            // take 2 remaining box from state and return transactions set
            val (consumedSelfBoxes, remainedSelfBoxes) = selfBoxes.splitAt(1)
            val boxesToSpend = if(stateBoxes.nonEmpty) stateBoxes.take(2) else consumedSelfBoxes

            val tx = validTransactionFromBoxes(boxesToSpend.toIndexedSeq, rnd, issueNew, dataBoxes = dataBoxesToUse)
            val cost: Long = getTxCost(tx, boxesToSpend, dataBoxesToUse)
            tx.statelessValidity() match {
              case Success(_) if cost <= remainingCost =>
                ((tx +: acc).reverse, remainedSelfBoxes ++ tx.outputs ++ createdEmissionBox)
              case Failure(e) =>
                log.warn(s"Failed to generate valid transaction: ${LoggingUtil.getReasonMsg(e)}")
                loop(remainingCost, stateBoxes, selfBoxes, acc, rnd)
            }
          }
      }
    }

    loop(emptyStateContext.currentParameters.maxBlockCost, stateBoxesIn, mutable.WrappedArray.empty, mutable.WrappedArray.empty, rnd)
  }

  protected def getTxCost(tx: ErgoTransaction, boxesToSpend: Seq[ErgoBox], dataBoxesToUse: Seq[ErgoBox]): Int = {
    tx.statefulValidity(
      tx.inputs.flatMap(i => boxesToSpend.find(_.id == i.boxId)),
      tx.dataInputs.flatMap(i => dataBoxesToUse.find(_.id == i.boxId)),
      emptyStateContext,
      -2000000)(emptyVerifier).getOrElse(0)
  }

  /**
    * Simplified check that box is an emission box
    */
  private def isEmissionBox(box: ErgoBox): Boolean = box.proposition == genesisEmissionBox.proposition

  /** @param txSizeLimit maximum transactions size in bytes */
  def validTransactionsFromBoxHolder(boxHolder: BoxHolder,
                                     rnd: RandomLike,
                                     txSizeLimit: Int = 10 * 1024): (Seq[ErgoTransaction], BoxHolder) = {
    val (emissionBox, boxHolderWithoutEmission) = boxHolder.take(b => isEmissionBox(b))
    val (_, bhWithoutGenesis) = boxHolderWithoutEmission.take(b => genesisBoxes.contains(b))
    val (regularBoxes, drainedBh) = bhWithoutGenesis.take(rnd.nextInt(txSizeLimit / 100) + 1)
    val boxes = emissionBox ++ regularBoxes
    val dataBoxes: Seq[ErgoBox] = Random.shuffle(boxHolder.boxes.values).take(rnd.nextInt(txSizeLimit / 100)).toSeq

    assert(boxes.nonEmpty, s"Was unable to take at least 1 box from box holder $boxHolder")
    val (txs, createdBoxes) = validTransactionsFromBoxes(txSizeLimit, boxes, dataBoxes, rnd)
    txs.foreach(_.statelessValidity().get)
    val bs = new BoxHolder(drainedBh.boxes ++ createdBoxes.map(b => ByteArrayWrapper(b.id) -> b))
    txs -> bs
  }


  def validTransactionsFromUtxoState(wus: WrappedUtxoState, rnd: RandomLike = new RandomWrapper): Seq[ErgoTransaction] = {
    val num = 1 + rnd.nextInt(3)

    val allBoxes = wus.takeBoxes(num + rnd.nextInt(100))
    val anyoneCanSpendBoxes = allBoxes.filter(_.ergoTree == Constants.TrueTree)
    val boxes = if (anyoneCanSpendBoxes.nonEmpty) anyoneCanSpendBoxes else allBoxes

    validTransactionsFromBoxes(num, boxes, rnd)._1
  }

  def validFullBlock(parentOpt: Option[ErgoFullBlock], utxoState: UtxoState, boxHolder: BoxHolder): ErgoFullBlock =
    validFullBlock(parentOpt, utxoState, boxHolder, new RandomWrapper)


  def validFullBlock(parentOpt: Option[ErgoFullBlock], utxoState: UtxoState, boxHolder: BoxHolder, rnd: RandomLike): ErgoFullBlock = {
    validFullBlock(parentOpt, utxoState, validTransactionsFromBoxHolder(boxHolder, rnd)._1)
  }

  def validFullBlockWithBoxHolder(parentOpt: Option[ErgoFullBlock],
                                  utxoState: UtxoState,
                                  boxHolder: BoxHolder,
                                  rnd: RandomLike): (ErgoFullBlock, BoxHolder) = {
    val txsBh = validTransactionsFromBoxHolder(boxHolder, rnd)
    validFullBlock(parentOpt, utxoState, txsBh._1) -> txsBh._2
  }

  def validFullBlock(parentOpt: Option[ErgoFullBlock],
                     wrappedState: WrappedUtxoState): ErgoFullBlock = {
    validFullBlock(parentOpt, wrappedState, wrappedState.versionedBoxHolder)
  }

  def validFullBlock(parentOpt: Option[ErgoFullBlock],
                     wrappedState: WrappedUtxoState,
                     time: Long): ErgoFullBlock = {
    validFullBlock(
      parentOpt,
      wrappedState,
      validTransactionsFromBoxHolder(wrappedState.versionedBoxHolder, new RandomWrapper)._1,
      Some(time)
    )
  }

  def validFullBlock(parentOpt: Option[ErgoFullBlock],
                     utxoState: UtxoState,
                     transactions: Seq[ErgoTransaction],
                     timeOpt: Option[Long] = None): ErgoFullBlock = {
    checkPayload(transactions, utxoState)

    val (adProofBytes, updStateDigest) = utxoState.proofsForTransactions(transactions).get

    val time = timeOpt.orElse(parentOpt.map(_.header.timestamp + 1)).getOrElse(System.currentTimeMillis())
    val interlinks = parentOpt.toSeq.flatMap { block =>
      nipopowAlgos.updateInterlinks(block.header, NipopowAlgos.unpackInterlinks(block.extension.fields).get)
    }
    val extension: ExtensionCandidate =
      parameters.toExtensionCandidate ++
        nipopowAlgos.interlinksToExtension(interlinks) ++
        utxoState.stateContext.validationSettings.toExtensionCandidate
    val votes = Array.fill(3)(0: Byte)

    powScheme.proveBlock(parentOpt.map(_.header), Header.InitialVersion, settings.chainSettings.initialNBits, updStateDigest, adProofBytes,
      transactions, time, extension, votes, defaultMinerSecretNumber).get
  }

  /**
    * Full block valid against state only - allows to create blocks form headers and state,
    * does not contain valid interlinks.
    */
  def statefulyValidFullBlock(wrappedState: WrappedUtxoState,
                              timeOpt: Option[Long] = None): ErgoFullBlock = {
    val parentOpt: Option[Header] = wrappedState.stateContext.lastHeaderOpt
    val parentExtensionOpt: Option[Extension] = wrappedState.stateContext.lastExtensionOpt
    val bh = wrappedState.versionedBoxHolder
    val transactions = validTransactionsFromBoxHolder(bh, new RandomWrapper)._1

    checkPayload(transactions, wrappedState)

    val (adProofBytes, updStateDigest) = wrappedState.proofsForTransactions(transactions).get

    val time = timeOpt.orElse(parentOpt.map(_.timestamp + 1)).getOrElse(System.currentTimeMillis())
    val interlinksExtension = nipopowAlgos.interlinksToExtension(nipopowAlgos.updateInterlinks(parentOpt, parentExtensionOpt))
    val extension: ExtensionCandidate = parameters.toExtensionCandidate ++ interlinksExtension
    val votes = Array.fill(3)(0: Byte)

    powScheme.proveBlock(parentOpt, Header.InitialVersion, settings.chainSettings.initialNBits, updStateDigest,
      adProofBytes, transactions, time, extension, votes, defaultMinerSecretNumber).get
  }

  private def checkPayload(transactions: Seq[ErgoTransaction], us: UtxoState): Unit = {
    transactions.foreach(_.statelessValidity() shouldBe 'success)
    transactions.nonEmpty shouldBe true
    ErgoState.boxChanges(transactions).get._1.foreach { case Remove(boxId) =>
      assert(us.boxById(boxId).isDefined, s"Box ${Algos.encode(boxId)} missed")
    }
  }

}
