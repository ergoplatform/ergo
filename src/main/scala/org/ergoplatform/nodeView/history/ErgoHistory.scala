package org.ergoplatform.nodeView.history

import akka.actor.ActorContext
import org.ergoplatform.consensus.ProgressInfo

import java.io.File
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.{Header, PreGenesisHeader}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoNodeViewModifier, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.StartExtraIndexer
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{GlobalBoxIndexKey, GlobalTxIndexKey, IndexedHeaderIdKey,
  IndexedHeightKey, NewestVersion, NewestVersionBytes, RollbackToKey, SchemaVersionKey}
import org.ergoplatform.nodeView.history.extra.{IndexedErgoBox, IndexedErgoTransaction, NumericBoxIndex, NumericTxIndex}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.validation.RecoverableModifierError
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.util.{Failure, Success, Try}

/**
  *
  * History of a blockchain system is some blocktree in fact
  * (like this: http://image.slidesharecdn.com/sfbitcoindev-chepurnoy-2015-150322043044-conversion-gate01/95/proofofstake-its-improvements-san-francisco-bitcoin-devs-hackathon-12-638.jpg),
  * where longest chain is being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead.
  *
  * History implementation. It is processing persistent modifiers generated locally or coming from the network.
  * Depending on chosen node settings, it will process modifiers in a different way, different processors define how to
  * process different type of modifiers.
  *
  * HeadersProcessor: processor of block headers. It's the same for all node settings
  * ADProofsProcessor: processor of ADProofs. ADProofs may
  *   1. Be downloaded from other nodes (ADState == true)
  *   2. Be calculated by using local state (ADState == false)
  *   3. Be ignored by history in light mode (verifyTransactions == false)
  * PoPoWProofsProcessor: processor of PoPoWProof. PoPoWProof may
  *   1. Be downloaded once during bootstrap from other peers (poPoWBootstrap == true)
  *   2. Be ignored by history (poPoWBootstrap == false)
  * BlockTransactionsProcessor: Processor of BlockTransactions. BlockTransactions may
  *   1. Be downloaded from other peers (verifyTransactions == true)
  *   2. Be ignored by history (verifyTransactions == false)
  */
trait ErgoHistory
  extends ErgoHistoryReader {

  override protected lazy val requireProofs: Boolean = nodeSettings.stateType.requireProofs

  def closeStorage(): Unit = historyStorage.close()

  /**
    * Dump modifier identifier and bytes to database.
    *
    * Used to dump ADProofs generated locally.
    *
    * @param mId - modifier identifier
    * @param bytes - modifier bytes
    * @return Success if modifier inserted into database successfully, Failure otherwise
    */
  def dumpToDb(mId: Array[Byte], bytes: Array[Byte]): Try[Unit] = {
    historyStorage.insert(mId, bytes)
  }

  /**
    * Append ErgoPersistentModifier to History if valid
    */
  def append(modifier: BlockSection): Try[(ErgoHistory, ProgressInfo[BlockSection])] = synchronized {
    log.debug(s"Trying to append modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} to history")
    applicableTry(modifier).flatMap { _ =>
      modifier match {
        case header: Header =>
          process(header)
        case section: NonHeaderBlockSection =>
          process(section)
      }
    }.map(this -> _).recoverWith { case e =>
      if (!e.isInstanceOf[RecoverableModifierError]) {
        log.warn(s"Error while applying modifier ${modifier.encodedId} of type ${modifier.modifierTypeId}, " +
          s"reason: ${LoggingUtil.getReasonMsg(e)} ")
      }
      Failure(e)
    }
  }

  /**
    * Mark modifier as valid
    */
  def reportModifierIsValid(modifier: BlockSection): Try[ErgoHistory] = synchronized {
    log.debug(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    modifier match {
      case fb: ErgoFullBlock =>
        val nonMarkedIds = (fb.header.id +: fb.header.sectionIds.map(_._2))
          .filter(id => historyStorage.getIndex(validityKey(id)).isEmpty).toArray

        if (nonMarkedIds.nonEmpty) {
          historyStorage.insert(
            nonMarkedIds.map(id => validityKey(id) -> Array(1.toByte)),
            BlockSection.emptyArray).map(_ => this)
        } else {
          Success(this)
        }
      case _ =>
        historyStorage.insert(
          Array(validityKey(modifier.id) -> Array(1.toByte)),
          BlockSection.emptyArray).map(_ => this)
    }
  }

  /**
    * Mark modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid from State point of view
    * @return ProgressInfo with next modifier to try to apply
    */
  @SuppressWarnings(Array("OptionGet", "TraversableHead"))
  def reportModifierIsInvalid(modifier: BlockSection,
                              progressInfo: ProgressInfo[BlockSection]
                             ): Try[(ErgoHistory, ProgressInfo[BlockSection])] = synchronized {
    log.warn(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct.toArray
        val invalidatedIds = invalidatedHeaders.map(_.id).toSet
        val validityRow = invalidatedHeaders.flatMap(h => Seq(h.id, h.transactionsId, h.ADProofsId)
          .map(id => validityKey(id) -> Array(0.toByte)))
        log.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId).mkString("Array(", ", ", ")")}")
        val bestHeaderIsInvalidated = bestHeaderIdOpt.exists(id => invalidatedIds.contains(id))
        val bestFullIsInvalidated = bestFullBlockIdOpt.exists(id => invalidatedIds.contains(id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required
            historyStorage.insert(validityRow, BlockSection.emptyArray).map { _ =>
              this -> ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)
            }
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required
            val newBestHeaderOpt = loopHeightDown(headersHeight, id => !invalidatedIds.contains(id))

            if (!bestFullIsInvalidated) {
              //Only headers chain involved
              historyStorage.insert(
                newBestHeaderOpt.map(h => BestHeaderKey -> idToBytes(h.id)).toArray,
                BlockSection.emptyArray
              ).map { _ =>
                this -> ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)
              }
            } else {
              val invalidatedChain: Seq[ErgoFullBlock] = bestFullBlockOpt.toSeq
                .flatMap(f => headerChainBack(fullBlockHeight + 1, f.header, h => !invalidatedIds.contains(h.id)).headers)
                .flatMap(getFullBlock)
                .ensuring(_.lengthCompare(1) >= 0, "invalidatedChain should contain at least bestFullBlock")

              val genesisInvalidated = invalidatedChain.lengthCompare(1) == 0
              val branchPointHeader = if (genesisInvalidated) PreGenesisHeader else invalidatedChain.head.header

              val validHeadersChain =
                continuationHeaderChains(branchPointHeader,
                  h => getFullBlock(h).isDefined && !invalidatedIds.contains(h.id))
                  .maxBy(_.lastOption.flatMap(x => scoreOf(x.id)).getOrElse(BigInt(0)))

              val validChain = validHeadersChain.tail.flatMap(getFullBlock)

              val chainStatusRow = validChain.map(b =>
                FullBlockProcessor.chainStatusKey(b.id) -> FullBlockProcessor.BestChainMarker) ++
                invalidatedHeaders.map(h =>
                  FullBlockProcessor.chainStatusKey(h.id) -> FullBlockProcessor.NonBestChainMarker)

              val changedLinks = validHeadersChain.lastOption.map(b => BestFullBlockKey -> idToBytes(b.id)) ++
                newBestHeaderOpt.map(h => BestHeaderKey -> idToBytes(h.id)).toSeq
              val toInsert = validityRow ++ changedLinks ++ chainStatusRow
              historyStorage.insert(toInsert, BlockSection.emptyArray).map { _ =>
                val toRemove = if (genesisInvalidated) invalidatedChain else invalidatedChain.tail
                this -> ProgressInfo(Some(branchPointHeader.id), toRemove, validChain, Seq.empty)
              }
            }
        }
      case None =>
        //No headers become invalid. Just mark this modifier as invalid
        log.warn(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is missing corresponding header")
        historyStorage.insert(Array(validityKey(modifier.id) -> Array(0.toByte)), BlockSection.emptyArray).map { _ =>
          this -> ProgressInfo[BlockSection](None, Seq.empty, Seq.empty, Seq.empty)
        }
    }
  }

  /**
    * @return header, that corresponds to modifier
    */
  protected def correspondingHeader(modifier: BlockSection): Option[Header] = modifier match {
    case h: Header => Some(h)
    case full: ErgoFullBlock => Some(full.header)
    case proof: ADProofs => typedModifierById[Header](proof.headerId)
    case txs: BlockTransactions => typedModifierById[Header](txs.headerId)
    case _ => None
  }

  /**
    * Remove header, corresponding block parts, and corresponding indexes from storage and caches
    * @param headerId - header id
    * @return
    */
  def forgetHeader(headerId: ModifierId): Try[Unit] = Try {
    val hOpt = typedModifierById[Header](headerId)
      val hRes = historyStorage.remove(
        indicesToRemove = Array(validityKey(headerId), headerHeightKey(headerId), headerScoreKey(headerId)),
        idsToRemove = Array(headerId)
      )
    log.info(s"Result of removing header $headerId: " + hRes)

    hOpt.foreach { h =>
      requiredModifiersForHeader(h).foreach { case (_, mId) =>
        val mRes = historyStorage.remove(
          indicesToRemove = Array(validityKey(mId)),
          idsToRemove = Array(mId)
        )
        log.info(s"Result of removing modifier $mId: " + mRes)
      }
    }
  }

  /**
    * @return read-only copy of this history
    */
  def getReader: ErgoHistoryReader = this

}

object ErgoHistory extends ScorexLogging {

  def historyDir(settings: ErgoSettings): File = {
    val dir = new File(s"${settings.directory}/history")
    dir.mkdirs()
    dir
  }

  // check if there is possible database corruption when there is header after
  // recognized blockchain tip marked as invalid
  protected[nodeView] def repairIfNeeded(history: ErgoHistory): Boolean = history.historyStorage.synchronized {
    val bestHeaderHeight = history.headersHeight
    val bestFullBlockHeight = history.bestFullBlockOpt.map(_.height).getOrElse(-1)
    val afterHeaders = history.headerIdsAtHeight(bestHeaderHeight + 1)

    if (bestHeaderHeight == bestFullBlockHeight && afterHeaders.nonEmpty) {
      log.warn("Found suspicious continuation, clearing it...")
      afterHeaders.map { hId =>
        history.forgetHeader(hId)
      }
      history.historyStorage.remove(Array(history.heightIdsKey(bestHeaderHeight + 1)), Array.empty[ModifierId])
      true
    } else {
      false
    }
  }

  /**
    * @return ErgoHistory instance with new database or database read from existing folder
    */
  def readOrGenerate(ergoSettings: ErgoSettings)(implicit context: ActorContext): ErgoHistory = {
    var db = HistoryStorage(ergoSettings)

    // ExtraIndexer db check
    if(ergoSettings.nodeSettings.extraIndex) { // check db schema and checkpoint provenance
      def storedBytes(key: Array[Byte]): Option[Array[Byte]] = db.modifierBytesById(bytesToId(key))
      def intValue(bytesOpt: Option[Array[Byte]]): Option[Int] = bytesOpt
        .filter(_.length == Integer.BYTES)
        .map(bytes => ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getInt)
      def longValue(bytesOpt: Option[Array[Byte]]): Option[Long] = bytesOpt
        .filter(_.length == java.lang.Long.BYTES)
        .map(bytes => ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getLong)

      val schemaVersionBytesOpt = storedBytes(SchemaVersionKey)
      val indexedHeightBytesOpt = storedBytes(IndexedHeightKey)
      val globalTxIndexBytesOpt = storedBytes(GlobalTxIndexKey)
      val globalBoxIndexBytesOpt = storedBytes(GlobalBoxIndexKey)
      val rollbackToBytesOpt = storedBytes(RollbackToKey)
      val schemaVersionOpt = intValue(schemaVersionBytesOpt)
      val indexedHeightOpt = intValue(indexedHeightBytesOpt)
      val globalTxIndexOpt = longValue(globalTxIndexBytesOpt)
      val globalBoxIndexOpt = longValue(globalBoxIndexBytesOpt)
      val rollbackToOpt = intValue(rollbackToBytesOpt)
      val indexedHeight = indexedHeightOpt.getOrElse(0)
      val globalTxIndex = globalTxIndexOpt.getOrElse(0L)
      val globalBoxIndex = globalBoxIndexOpt.getOrElse(0L)
      val rollbackTo = rollbackToOpt.getOrElse(0)
      val indexedHeaderIdBytesOpt = db.modifierBytesById(bytesToId(IndexedHeaderIdKey))
      val indexedHeaderOpt = indexedHeaderIdBytesOpt
        .filter(_.length == ErgoNodeViewModifier.ModifierIdSize)
        .flatMap { idBytes =>
        val id = bytesToId(idBytes)
        val validityKey = ByteArrayWrapper(Algos.hash("validity".getBytes(StandardCharsets.UTF_8) ++ idToBytes(id)))
        val isValid = db.getIndex(validityKey).exists(_.sameElements(Array(1.toByte)))
        if (isValid) db.modifierById(id).collect {
          case header: Header if header.height == indexedHeight && header.id == id => header
        } else None
      }
      val terminalRowsMatchCheckpoint = indexedHeaderOpt.exists { header =>
        if (globalTxIndex <= 0 || globalBoxIndex <= 0) {
          false
        } else {
          db.modifierById(header.transactionsId).collect {
            case transactions: BlockTransactions if transactions.headerId == header.id =>
              val lastTx = transactions.txs.last
              val lastTxIndex = globalTxIndex - 1
              val firstTxBoxIndex = globalBoxIndex - lastTx.outputs.size
              val expectedOutputNums = Array.tabulate(lastTx.outputs.size)(i => firstTxBoxIndex + i)
              val expectedInputNumsOpt = if (header.height <= 1) {
                Some(Array.fill[Long](lastTx.inputs.size)(0L))
              } else {
                val inputNums = lastTx.inputs.map { input =>
                  val inputId = bytesToId(input.boxId)
                  db.getExtraIndex(inputId).collect {
                    case box: IndexedErgoBox
                      if box.id == inputId && box.spendingTxIdOpt.contains(lastTx.id) &&
                        box.spendingHeightOpt.contains(header.height) => box.globalIndex
                  }
                }
                if (inputNums.forall(_.isDefined)) Some(inputNums.flatten.toArray) else None
              }
              val numericTxMatches = db.getExtraIndex(bytesToId(NumericTxIndex.indexToBytes(lastTxIndex))).exists {
                case NumericTxIndex(index, id) => index == lastTxIndex && id == lastTx.id
                case _ => false
              }
              val indexedTxMatches = db.getExtraIndex(lastTx.id).exists {
                case tx: IndexedErgoTransaction =>
                  tx.txid == lastTx.id && tx.globalIndex == lastTxIndex && tx.height == header.height &&
                    tx.index == transactions.txs.size - 1 && tx.size == lastTx.size &&
                    expectedInputNumsOpt.exists(expected => tx.inputNums.sameElements(expected)) &&
                    tx.outputNums.sameElements(expectedOutputNums) && tx.dataInputs.sameElements(lastTx.dataInputs)
                case _ => false
              }
              val outputRowsMatch = expectedOutputNums.zip(lastTx.outputs).forall { case (boxIndex, output) =>
                val boxId = bytesToId(output.id)
                val numericBoxMatches = db.getExtraIndex(bytesToId(NumericBoxIndex.indexToBytes(boxIndex))).exists {
                  case NumericBoxIndex(index, id) => index == boxIndex && id == boxId
                  case _ => false
                }
                val indexedBoxMatches = db.getExtraIndex(boxId).exists {
                  case box: IndexedErgoBox =>
                    box.globalIndex == boxIndex && box.inclusionHeight == header.height && box.id == boxId &&
                      box.spendingTxIdOpt.isEmpty && box.spendingHeightOpt.isEmpty && box.spendingProofOpt.isEmpty
                  case _ => false
                }
                numericBoxMatches && indexedBoxMatches
              }
              numericTxMatches && indexedTxMatches && outputRowsMatch
          }.contains(true)
        }
      }
      val numericValuesAreWellFormed = Seq(
        indexedHeightBytesOpt.forall(_.length == Integer.BYTES),
        globalTxIndexBytesOpt.forall(_.length == java.lang.Long.BYTES),
        globalBoxIndexBytesOpt.forall(_.length == java.lang.Long.BYTES),
        rollbackToBytesOpt.forall(_.length == Integer.BYTES)
      ).forall(identity)
      val valuesAreNonNegative = indexedHeight >= 0 && globalTxIndex >= 0 && globalBoxIndex >= 0 && rollbackTo >= 0
      val emptyCheckpoint = indexedHeight == 0 && globalTxIndex == 0 && globalBoxIndex == 0 &&
        rollbackTo == 0 && indexedHeaderIdBytesOpt.isEmpty
      val nonEmptyCheckpoint = indexedHeight > 0 && Seq(indexedHeightOpt, globalTxIndexOpt, globalBoxIndexOpt, rollbackToOpt)
        .forall(_.isDefined) && rollbackTo == 0 && terminalRowsMatchCheckpoint
      val checkpointIsValid = schemaVersionOpt.contains(NewestVersion) && numericValuesAreWellFormed &&
        valuesAreNonNegative && (emptyCheckpoint || nonEmptyCheckpoint)
      if (!checkpointIsValid) {
        val freshDb = db.deleteExtraDBTry(ergoSettings).get
        freshDb.insertExtraTry(Array((SchemaVersionKey, NewestVersionBytes)), Array.empty).recoverWith { case error =>
          Try(freshDb.close()).failed.foreach(error.addSuppressed)
          Failure(error)
        }.get
        db = freshDb
      }
    }

    val nodeSettings = ergoSettings.nodeSettings

    val history: ErgoHistory = nodeSettings.verifyTransactions match {
      case true =>
        new ErgoHistory with FullBlockSectionProcessor {
          override protected val settings: ErgoSettings = ergoSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
        }

      case false =>
        new ErgoHistory with EmptyBlockSectionProcessor {
          override protected val settings: ErgoSettings = ergoSettings
          override protected[history] val historyStorage: HistoryStorage = db
          override val powScheme: AutolykosPowScheme = chainSettings.powScheme
        }
    }

    repairIfNeeded(history)

    log.info("History database read")
    if(ergoSettings.nodeSettings.extraIndex) // start extra indexer, if enabled
      context.system.eventStream.publish(StartExtraIndexer(history))
    history
  }

}
