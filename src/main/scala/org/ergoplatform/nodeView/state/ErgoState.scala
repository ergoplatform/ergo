package org.ergoplatform.nodeView.state

import java.io.File
import org.ergoplatform.ErgoBox.{AdditionalRegisters, R4, TokenId}
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform._
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.groupElemFromBytes
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.state.StateChanges
import org.ergoplatform.nodeView.ErgoNodeViewHolderLocallyGeneratedModifier._
import org.ergoplatform.nodeView.history.ErgoHistoryConstants._
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings.{ChainSettings, Constants, ErgoSettings, LaunchParameters, NodeConfigurationSettings}
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.validation.ValidationResult.Valid
import org.ergoplatform.validation.{ModifierValidator, ValidationResult}
import org.ergoplatform.core.{VersionTag, idToVersion}
import scorex.crypto.authds.avltree.batch.{Insert, Lookup, Remove}
import scorex.crypto.authds.{ADDigest, ADValue}
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.AtLeast
import sigmastate.Values.{ByteArrayConstant, ErgoTree, IntConstant, SigmaPropConstant}
import sigmastate.crypto.DLogProtocol.ProveDlog
import sigmastate.serialization.ValueSerializer
import spire.syntax.all.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.collection.breakOut

/**
  * Implementation of minimal state concept in Scorex. Minimal state (or just state from now) is some data structure
  * enough to validate a new blockchain element(e.g. block).
  * State in Ergo could be UTXO, like in Bitcoin or just a single digest. If the state is about UTXO, transaction set
  * of a block could be verified with no help of additional data. If the state is about just a digest, then proofs for
  * transformations of UTXO set presented in form of authenticated dynamic dictionary are needed to check validity of
  * a transaction set (see https://eprint.iacr.org/2016/994 for details).
  */
trait ErgoState[IState <: ErgoState[IState]] extends ErgoStateReader {

  self: IState =>

  /**
    *
    * @param mod modifire to apply to the state
    * @param estimatedTip - estimated height of blockchain tip
    * @param generate function that handles newly created modifier as a result of application the current one
    * @return new State
    */
  def applyModifier(mod: BlockSection, estimatedTip: Option[Height])(generate: LocallyGeneratedModifier => Unit): Try[IState]

  def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  /**
    * @return read-only view of this state
    */
  def getReader: ErgoStateReader = this

  /**
    * Close database where state-related data lives
    */
  def closeStorage(): Unit = {
    log.warn("Closing state's store.")
    store.close()
  }

}

object ErgoState extends ScorexLogging {

  type ModifierProcessing[T <: ErgoState[T]] = PartialFunction[BlockSection, Try[T]]

  def stateDir(settings: ErgoSettings): File = new File(s"${settings.directory}/state")

  /**
    * Resolves state changing operations from transactions. There could be invalid sequence
    * of operations like utxo double-spending in which case entire block is considered invalid
    * @param txs - sequence of transactions
    * @return ordered sequence of operations on UTXO set from this sequence of transactions
    *         if some box was created and later spent in this sequence - it is not included in the result at all
    *         if box was first spent and created after that - it is in both toInsert and toRemove
    */
  def stateChanges(txs: Seq[ErgoTransaction]): Try[StateChanges] = {
    boxChanges(txs).map { case (toRemoveChanges, toInsertChanges) =>
      val toLookup: IndexedSeq[Lookup] = txs.flatMap(_.dataInputs).map(b => Lookup(b.boxId))(breakOut)
      StateChanges(toRemoveChanges, toInsertChanges, toLookup)
    }
  }

  /**
    * Tries to validate and execute transactions.
    * @param transactions to be validated and executed
    * @param currentStateContext to be used for tx execution
    * @param checkBoxExistence function to provide ErgoBox by BoxId
    * @return Result of transactions execution with total cost inside
    */
  def execTransactions(transactions: Seq[ErgoTransaction],
                       currentStateContext: ErgoStateContext,
                       nodeSettings: NodeConfigurationSettings)
                      (checkBoxExistence: ErgoBox.BoxId => Try[ErgoBox]): ValidationResult[Long] = {
    val verifier: ErgoInterpreter = ErgoInterpreter(currentStateContext.currentParameters)

    def preAllocatedBuilder[T: ClassTag](sizeHint: Int): mutable.ArrayBuilder[T] = {
      val b = mutable.ArrayBuilder.make[T]()
      b.sizeHint(sizeHint)
      b
    }

    @tailrec
    def collectBoxesById(
                 remainingBoxIds: Iterator[ErgoBox.BoxId],
                 resultingBoxes: Try[mutable.ArrayBuilder[ErgoBox]]
               ): Try[IndexedSeq[ErgoBox]] = {
      if (!remainingBoxIds.hasNext) {
        resultingBoxes.map(_.result())
      } else {
        checkBoxExistence(remainingBoxIds.next()) match {
          case Success(box) =>
            collectBoxesById(remainingBoxIds, resultingBoxes.map(_ += box))
          case Failure(ex) =>
            Failure(ex)
        }
      }
    }

    val checkpointHeight = nodeSettings.checkpoint.map(_.height).getOrElse(0)
    if (currentStateContext.currentHeight <= checkpointHeight) {
      Valid(0L)
    } else {
      import spire.syntax.all.cfor
      var costResult: ValidationResult[Long] = Valid[Long](0L)
      cfor(0)(_ < transactions.length && costResult.isValid, _ + 1) { i =>
        val validCostResult = costResult.asInstanceOf[Valid[Long]]
        val tx = transactions(i)
        val boxesToSpendTry: Try[IndexedSeq[ErgoBox]] =
          collectBoxesById(tx.inputs.iterator.map(_.boxId), Success(preAllocatedBuilder(tx.inputs.length)))
        lazy val dataBoxesTry: Try[IndexedSeq[ErgoBox]] =
          collectBoxesById(tx.dataInputs.iterator.map(_.boxId), Success(preAllocatedBuilder(tx.inputs.length)))
        lazy val boxes: Try[(IndexedSeq[ErgoBox], IndexedSeq[ErgoBox])] = dataBoxesTry.flatMap(db => boxesToSpendTry.map(bs => (db, bs)))
        costResult = tx.validateStateless()
          .validateNoFailure(txBoxesToSpend, boxesToSpendTry, tx.id, tx.modifierTypeId)
          .validateNoFailure(txDataBoxes, dataBoxesTry, tx.id, tx.modifierTypeId)
          .payload[Long](validCostResult.value)
          .validateTry(boxes, e => ModifierValidator.fatal("Missed data boxes", tx.id, tx.modifierTypeId, e)) { case (_, (dataBoxes, toSpend)) =>
            tx.validateStateful(toSpend, dataBoxes, currentStateContext, validCostResult.value)(verifier).result
          }
      }
      costResult
    }
  }

  /**
    * @param txs - sequence of transactions
    * @return modifications from `txs` - sequence of ids to remove, and sequence of ErgoBoxes to create.
    *         if some box was created and later spend in this sequence - it is not included in the result at all
    *         if box was first spend and created after that - it is in both toInsert and toRemove,
    *         and an error will be thrown further during tree modification
    */
  def boxChanges(txs: Seq[ErgoTransaction]): Try[(Vector[Remove], Vector[Insert])] = Try {
    val toInsert: mutable.TreeMap[ModifierId, Insert] = mutable.TreeMap.empty
    val toRemove: mutable.TreeMap[ModifierId, Remove] = mutable.TreeMap.empty

    cfor(0)(_ < txs.length, _ + 1) { i =>
      val tx = txs(i)
      tx.inputs.foreach { i =>
        val wrappedBoxId = bytesToId(i.boxId)
        toInsert.remove(wrappedBoxId) match {
          case None =>
            if (toRemove.put(wrappedBoxId, Remove(i.boxId)).nonEmpty) {
              throw new IllegalArgumentException(s"Tx : ${tx.id} is double-spending input id : $wrappedBoxId")
            }
          case _ => // old value removed, do nothing
        }
      }
      tx.outputs.foreach(o => toInsert += bytesToId(o.id) -> Insert(o.id, ADValue @@ o.bytes))
    }
    (toRemove.map(_._2)(breakOut), toInsert.map(_._2)(breakOut))
  }

  /**
    * @param txs - sequence of transactions
    * @return new ErgoBoxes produced by the transactions
    */
  def newBoxes(txs: Seq[ErgoTransaction]): Vector[ErgoBox] = {
    val newBoxes: mutable.TreeMap[ModifierId, ErgoBox] = mutable.TreeMap.empty
    txs.foreach { tx =>
      tx.inputs.foreach(i => newBoxes.remove(bytesToId(i.boxId)))
      tx.outputs.foreach(o => newBoxes += bytesToId(o.id) -> o)
    }
    newBoxes.map(_._2)(breakOut)
  }

  /**
    * Helper method used to construct boxes for pre-genesis state (state before a genesis block)
    */
  private def createGenesisBox(value: Long,
                        ergoTree: ErgoTree,
                        additionalTokens: Seq[(TokenId, Long)] = Seq.empty,
                        additionalRegisters: AdditionalRegisters = Map.empty): ErgoBox = {
    import sigmastate.eval._

    val creationHeight: Int = EmptyHistoryHeight

    val transactionId: ModifierId = ErgoBox.allZerosModifierId
    val boxIndex: Short = 0: Short

    new ErgoBox(value, ergoTree,
      CostingSigmaDslBuilder.Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)
  }

  /**
    * Genesis box that contains all coins to be collected by Ergo foundation.
    * Box is protected by the script that allows to take part of them every block
    * and proposition from R4
    */
  private def genesisFoundersBox(settings: ChainSettings): ErgoBox = {
    val emission = settings.emissionRules
    val pks = settings.foundersPubkeys
      .map(str => groupElemFromBytes(Base16.decode(str).get))
      .map(pk => SigmaPropConstant(ProveDlog(pk)))
    val protection = AtLeast(IntConstant(2), pks)
    val protectionBytes = ValueSerializer.serialize(protection)
    val value = emission.foundersCoinsTotal - EmissionRules.CoinsInOneErgo
    val prop = ErgoTreePredef.foundationScript(settings.monetary)
    createGenesisBox(value, prop, Seq.empty, Map(R4 -> ByteArrayConstant(protectionBytes)))
  }

  /**
    * Genesis box that contains coins in the system to be collected by miners.
    * Box is protected by the script that allows to take part of them every block.
    */
  private def genesisEmissionBox(chainSettings: ChainSettings): ErgoBox = {
    val value = chainSettings.emissionRules.minersCoinsTotal
    val prop = chainSettings.monetary.emissionBoxProposition
    createGenesisBox(value, prop)
  }

  /**
    * Genesis box that contains proofs of no premine.
    * It is a long-living box with special bytes in registers
    */
  private def noPremineBox(chainSettings: ChainSettings): ErgoBox = {
    val proofsBytes = chainSettings.noPremineProof.map(b => ByteArrayConstant(b.getBytes("UTF-8")))
    val proofs = ErgoBox.nonMandatoryRegisters.zip(proofsBytes).toMap
    createGenesisBox(EmissionRules.CoinsInOneErgo, Constants.FalseLeaf, Seq.empty, proofs)
  }

  /**
    * Genesis state boxes generator.
    * Genesis state is corresponding to the state before the very first block processed.
    * For Ergo mainnet, contains emission contract box, proof-of-no--premine box, and treasury contract box
    */
  def genesisBoxes(chainSettings: ChainSettings): Seq[ErgoBox] = {
    Seq(genesisEmissionBox(chainSettings), noPremineBox(chainSettings), genesisFoundersBox(chainSettings))
  }

  /**
    * Generate genesis full (UTXO-set) state by inserting genesis boxes into empty UTXO set.
    * Assign `genesisStateDigest` from config as its version.
    */
  def generateGenesisUtxoState(stateDir: File, settings: ErgoSettings): (UtxoState, BoxHolder) = {

    log.info("Generating genesis UTXO state")
    val boxes = genesisBoxes(settings.chainSettings)
    val bh = BoxHolder(boxes)

    UtxoState.fromBoxHolder(bh, boxes.headOption, stateDir, settings, LaunchParameters).ensuring(us => {
      log.info(s"Genesis UTXO state generated with hex digest ${Base16.encode(us.rootDigest)}")
      java.util.Arrays.equals(us.rootDigest, settings.chainSettings.genesisStateDigest) && us.version == genesisStateVersion
    }) -> bh
  }

  /**
    * Generate genesis digest state similarly to `generateGenesisUtxoState`, but without really storing boxes
    */
  def generateGenesisDigestState(stateDir: File, settings: ErgoSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(settings.chainSettings.genesisStateDigest), stateDir, settings)
  }

  val preGenesisStateDigest: ADDigest = ADDigest @@ Array.fill(32)(0: Byte)

  lazy val genesisStateVersion: VersionTag = idToVersion(Header.GenesisParentId)

  /**
    * Read from disk or generate genesis UTXO-set or digest based state
    * @param settings - config used to find state database or extract genesis boxes data
    */
  def readOrGenerate(settings: ErgoSettings): ErgoState[_] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    settings.nodeSettings.stateType match {
      case StateType.Digest => DigestState.create(None, None, dir, settings)
      case StateType.Utxo if dir.listFiles().nonEmpty => UtxoState.create(dir, settings)
      case _ => ErgoState.generateGenesisUtxoState(dir, settings)._1
    }
  }

}
