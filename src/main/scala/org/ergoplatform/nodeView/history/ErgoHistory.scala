package org.ergoplatform.nodeView.history

import akka.actor.ActorContext
import org.ergoplatform.consensus.{ModifierSemanticValidity, ProgressInfo}

import java.io.File
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.{Header, PreGenesisHeader}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoNodeViewModifier, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{GlobalBoxIndexKey, GlobalTxIndexKey, IndexedHeaderIdKey,
  IndexedHeightKey, NewestVersion, NewestVersionBytes, RentIndexEnabledBytes, RentIndexEnabledKey, RollbackToKey, SchemaVersionKey}
import org.ergoplatform.nodeView.history.extra.{IndexedErgoBox, IndexedErgoTransaction, NumericBoxIndex, NumericTxIndex}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.validation.RecoverableModifierError
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.annotation.{nowarn, tailrec}
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
    * @return Success after every removal, or the first removal failure
    */
  def forgetHeader(headerId: ModifierId): Try[Unit] = Try {
    historyStorage.modifierById(headerId).collect { case h: Header if h.id == headerId => h }
  }.flatMap { hOpt =>
    // Keep the header until its sections are removed so a retry can still discover their identifiers.
    hOpt.toSeq.flatMap(_.sectionIds).foldLeft[Try[Unit]](Success(())) {
      case (result, (_, mId)) => result.flatMap { _ =>
        removeLogged("modifier", mId)(Array(validityKey(mId)), Array(mId))
      }
    }.flatMap { _ =>
      removeLogged("header", headerId)(
        indicesToRemove = Array(validityKey(headerId), headerHeightKey(headerId), headerScoreKey(headerId)),
        idsToRemove = Array(headerId)
      )
    }
  }

  // Removes the rows and logs the result; a thrown storage error counts as a Failure.
  private def removeLogged(kind: String, id: ModifierId)(
    indicesToRemove: Array[ByteArrayWrapper],
    idsToRemove: Array[ModifierId]
  ): Try[Unit] = {
    val removal = Try(historyStorage.remove(indicesToRemove, idsToRemove)).flatten
    removal match {
      case Success(_) => log.info(s"Result of removing $kind $id: $removal")
      case Failure(error) =>
        log.error(s"Failed to remove $kind $id during history repair", error)
    }
    removal
  }

  /**
    * Moves a best full block pointer that names a block this node cannot assemble to the highest ancestor it can
    * assemble, down to `minimalFullBlockHeight`, or removes the pointer if there is none. Otherwise the block is
    * never applied, as the pointer already names it. The skipped headers lose their best-chain marker, so that the
    * markers still form one chain. Any section missing in any state mode starts the repair: for example the
    * ADProofs a UTXO node stores only after the state update, missing in a history opened in digest mode (#2788),
    * or a lost section (#2778, case 2). A pointer to a header that is not stored, such as the pre-genesis id
    * `reportModifierIsInvalid` writes when the genesis block is invalidated, is left as it is.
    */
  protected[history] def repairIncompleteBestFullBlock(): Try[Unit] = Try(historyStorage.synchronized {
    bestFullBlockIdOpt.flatMap(id => typedModifierById[Header](id)).filter(getFullBlock(_).isEmpty) match {
      case Some(bestHeader) =>
        val lowestHeight = minimalFullBlockHeight

        @tailrec
        def walk(headerOpt: Option[Header], skipped: List[Header]): (List[Header], Option[Header]) =
          headerOpt match {
            case Some(h) if h.height >= lowestHeight && getFullBlock(h).isEmpty =>
              walk(typedModifierById[Header](h.parentId), h :: skipped)
            case Some(h) if h.height >= lowestHeight => (skipped, Some(h))
            case _ => (skipped, None)
          }

        val (skipped, completeOpt) = walk(typedModifierById[Header](bestHeader.parentId), List(bestHeader))
        log.warn(s"Best full block ${bestHeader.id} cannot be assembled in ${nodeSettings.stateType} mode, " +
          s"moving the pointer to ${completeOpt.map(h => s"${h.id} at height ${h.height}").getOrElse("none")}")
        val markers = skipped.map(h => FullBlockProcessor.chainStatusKey(h.id) -> FullBlockProcessor.NonBestChainMarker)
        completeOpt match {
          case Some(header) =>
            historyStorage.insert((BestFullBlockKey -> idToBytes(header.id)) +: markers.toArray, BlockSection.emptyArray)
          case None =>
            historyStorage.insert(markers.toArray, BlockSection.emptyArray)
              .flatMap(_ => historyStorage.remove(Array(BestFullBlockKey), Array.empty[ModifierId]))
        }
      case None =>
        Success(())
    }
  }).flatten

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

  // A retained alternative fork may continue above the selected full-block tip.
  // Keep a continuation only when it has a readable, matching header and is not invalid.
  // Success(false) means no repair was needed; Success(true) means all removals completed.
  // Failure preserves the first error; the height index is removed only after all headers are forgotten.
  protected[nodeView] def repairIfNeeded(history: ErgoHistory): Try[Boolean] = Try(history.historyStorage.synchronized {
    val bestHeaderHeight = history.headersHeight
    val bestFullBlockHeight = history.bestFullBlockOpt.map(_.height).getOrElse(-1)
    val afterHeaders = history.headerIdsAtHeight(bestHeaderHeight + 1)

    val removableContinuations = afterHeaders.nonEmpty && afterHeaders.forall { id =>
      history.isSemanticallyValid(id) match {
        case ModifierSemanticValidity.Invalid => true
        case _ => !history.historyStorage.modifierById(id).exists {
          case header: Header => header.id == id
          case _ => false
        }
      }
    }

    if (bestHeaderHeight == bestFullBlockHeight && removableContinuations) {
      log.warn("Found invalid or unreadable continuation, clearing it...")
      afterHeaders.foldLeft[Try[Unit]](Success(())) { (result, hId) =>
        result.flatMap(_ => history.forgetHeader(hId))
      }.flatMap { _ =>
        history.historyStorage.remove(Array(history.heightIdsKey(bestHeaderHeight + 1)), Array.empty[ModifierId])
      }.map(_ => true)
    } else {
      Success(false)
    }
  }).flatten

  /**
    * @return ErgoHistory instance with new database or database read from existing folder
    */
  def readOrGenerate(ergoSettings: ErgoSettings)(implicit context: ActorContext): ErgoHistory =
    readOrGenerate(ergoSettings, HistoryStorage(ergoSettings))

  @nowarn("cat=unused")
  private[history] def readOrGenerate(ergoSettings: ErgoSettings,
                                    storage: HistoryStorage)(implicit context: ActorContext): ErgoHistory = {
    var db = storage

    // The close action follows the store owned at this stage of startup.
    def stopOnFailure[A](step: String, result: Try[A])(closeStore: => Unit): A = result match {
      case Failure(error) =>
        Try(closeStore).failed.foreach { closeError =>
          if (closeError ne error) error.addSuppressed(closeError)
        }
        log.error(
          s"$step failed; stop this node. Operator recovery: " +
            "inspect the error and correct its cause (disk space, permissions, " +
            "or open handles); keep history/index and history/objects intact, " +
            "then restart to retry.",
          error)
        throw error
      case Success(value) => value
    }

    // ExtraIndexer db check
    if(ergoSettings.nodeSettings.extraIndex) { // check db schema and checkpoint provenance
      // Only preflight reads use this guard. Rebuild operations already close their own stores.
      def readPreflight[A](read: => A): A =
        stopOnFailure("Extra-index preflight read", Try(read))(db.close())

      def storedBytes(key: Array[Byte]): Option[Array[Byte]] =
        readPreflight(db.modifierBytesById(bytesToId(key)))
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
      val indexedHeaderIdBytesOpt = storedBytes(IndexedHeaderIdKey)
      val indexedHeaderOpt = indexedHeaderIdBytesOpt
        .filter(_.length == ErgoNodeViewModifier.ModifierIdSize)
        .flatMap { idBytes =>
        val id = bytesToId(idBytes)
        val validityKey = ByteArrayWrapper(Algos.hash("validity".getBytes(StandardCharsets.UTF_8) ++ idToBytes(id)))
        // Unknown validity is accepted: the indexer does not gate on semantic validity, so a
        // checkpoint over a not-yet-marked header is legitimate. An explicitly Invalid
        // checkpoint header still forces a rebuild - the indexed chain was invalidated.
        val isInvalid = readPreflight(db.getIndex(validityKey)).exists(_.sameElements(Array(0.toByte)))
        if (!isInvalid && readPreflight(FullBlockProcessor.isInBestFullChain(db, id))) readPreflight(db.modifierById(id)).collect {
          case header: Header if header.height == indexedHeight && header.id == id => header
        } else None
      }
      val terminalRowsMatchCheckpoint = schemaVersionOpt.contains(NewestVersion) && indexedHeaderOpt.exists { header =>
        if (globalTxIndex <= 0 || globalBoxIndex <= 0) {
          false
        } else {
          readPreflight(db.modifierById(header.transactionsId)).collect {
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
                  readPreflight(db.getExtraIndex(inputId)).collect {
                    case box: IndexedErgoBox
                      if box.id == inputId && box.spendingTxIdOpt.contains(lastTx.id) &&
                        box.spendingHeightOpt.contains(header.height) => box.globalIndex
                  }
                }
                if (inputNums.forall(_.isDefined)) Some(inputNums.flatten.toArray) else None
              }
              val numericTxMatches = readPreflight(db.getExtraIndex(bytesToId(NumericTxIndex.indexToBytes(lastTxIndex)))).exists {
                case NumericTxIndex(index, id) => index == lastTxIndex && id == lastTx.id
                case _ => false
              }
              val indexedTxMatches = readPreflight(db.getExtraIndex(lastTx.id)).exists {
                case tx: IndexedErgoTransaction =>
                  tx.txid == lastTx.id && tx.globalIndex == lastTxIndex && tx.height == header.height &&
                    tx.blockId == header.id &&
                    tx.index == transactions.txs.size - 1 && tx.size == lastTx.size &&
                    expectedInputNumsOpt.exists(expected => tx.inputNums.sameElements(expected)) &&
                    tx.outputNums.sameElements(expectedOutputNums) && tx.dataInputs.sameElements(lastTx.dataInputs)
                case _ => false
              }
              val outputRowsMatch = expectedOutputNums.zip(lastTx.outputs).forall { case (boxIndex, output) =>
                val boxId = bytesToId(output.id)
                val numericBoxMatches = readPreflight(db.getExtraIndex(bytesToId(NumericBoxIndex.indexToBytes(boxIndex)))).exists {
                  case NumericBoxIndex(index, id) => index == boxIndex && id == boxId
                  case _ => false
                }
                val indexedBoxMatches = readPreflight(db.getExtraIndex(boxId)).exists {
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
        val failedChecks = Seq(
          !schemaVersionOpt.contains(NewestVersion) -> "schema version",
          !numericValuesAreWellFormed -> "counter encoding",
          !valuesAreNonNegative -> "negative counters",
          (rollbackTo != 0) -> "rollback marker",
          (indexedHeight == 0 && !emptyCheckpoint) -> "empty checkpoint shape",
          (indexedHeight > 0 && Seq(indexedHeightOpt, globalTxIndexOpt, globalBoxIndexOpt, rollbackToOpt)
            .exists(_.isEmpty)) -> "missing checkpoint counters",
          (indexedHeight > 0 && (globalTxIndex <= 0 || globalBoxIndex <= 0)) ->
            "non-positive global index counters",
          (indexedHeight > 0 && indexedHeaderOpt.isEmpty) -> "checkpoint header or validity",
          (indexedHeight > 0 && schemaVersionOpt.contains(NewestVersion) && indexedHeaderOpt.isDefined &&
            globalTxIndex > 0 && globalBoxIndex > 0 && !terminalRowsMatchCheckpoint) ->
            "terminal transaction or box rows"
        ).collect { case (true, name) => name }
        log.warn(s"Rebuilding invalid extra index checkpoint: ${failedChecks.mkString(", ")}")
        val freshDb = db.deleteExtraDBTry(ergoSettings).recoverWith { case error =>
          val extraIndexPath = new File(s"${ergoSettings.directory}/history/extra").getAbsolutePath
          log.error(
            "Extra index rebuild failed; node startup is aborting. Operator recovery: " +
              "1) stop every Ergo node process using this data directory; " +
              "2) inspect the preceding exception and correct its cause, such as disk space, permissions, or open handles; " +
              s"3) move or remove only '$extraIndexPath'; " +
              "4) verify that directory no longer exists; 5) restart the node to retry the extra index rebuild. " +
              "Do not remove the history/index or history/objects directories.",
            error)
          Failure(error)
        }.get
        val rentMarkerEntry: Array[(Array[Byte], Array[Byte])] =
          if (ergoSettings.nodeSettings.storageRentCollection) {
            Array(RentIndexEnabledKey -> RentIndexEnabledBytes)
          } else {
            Array.empty
          }
        freshDb.insertExtraTry(Array((SchemaVersionKey, NewestVersionBytes)) ++ rentMarkerEntry, Array.empty).recoverWith { case error =>
          Try(freshDb.close()).failed.foreach(error.addSuppressed)
          Failure(error)
        }.get
        db = freshDb
      }

      // Rent rows are written only while storageRentCollection is on, which the schema
      // version can not capture (see ExtraIndexer.RentIndexEnabledKey): turning the flag
      // on after an off period must rebuild the index (blocks indexed while off have no
      // rent rows); turning it off just clears the marker, keeping the index intact.
      val rentEnabledStored = storedBytes(RentIndexEnabledKey)
        .exists(_.sameElements(RentIndexEnabledBytes))
      if (ergoSettings.nodeSettings.storageRentCollection && !rentEnabledStored) {
        log.warn("storageRentCollection is on but the extra index has no rent rows, rebuilding it")
        val freshDb = db.deleteExtraDBTry(ergoSettings).recoverWith { case error =>
          val extraIndexPath = new File(s"${ergoSettings.directory}/history/extra").getAbsolutePath
          log.error(
            "Extra index rebuild failed; node startup is aborting. Operator recovery: " +
              "1) stop every Ergo node process using this data directory; " +
              "2) inspect the preceding exception and correct its cause, such as disk space, permissions, or open handles; " +
              s"3) move or remove only '$extraIndexPath'; " +
              "4) verify that directory no longer exists; 5) restart the node to retry the extra index rebuild. " +
              "Do not remove the history/index or history/objects directories.",
            error)
          Failure(error)
        }.get
        freshDb.insertExtraTry(Array(
          (SchemaVersionKey, NewestVersionBytes),
          (RentIndexEnabledKey, RentIndexEnabledBytes)), Array.empty).recoverWith { case error =>
          Try(freshDb.close()).failed.foreach(error.addSuppressed)
          Failure(error)
        }.get
        db = freshDb
      } else if (!ergoSettings.nodeSettings.storageRentCollection && rentEnabledStored) {
        db.removeExtra(Array(bytesToId(RentIndexEnabledKey)))
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

    // A failed repair closes the constructed history and stops the start-up.
    stopOnFailure("History repair", repairIfNeeded(history))(history.closeStorage())
    stopOnFailure("Best full block repair", history.repairIncompleteBestFullBlock())(history.closeStorage())

    // If the node is restarted in the middle of UTXO set snapshot bootstrapping (headers are
    // already persisted, but the snapshot was not applied yet), the headers chain is already
    // complete - via a NiPoPoW proof or via ordinary header sync - so mark it synced. Otherwise
    // the synchronizer never asks for the snapshot manifest / block sections
    // (ErgoNodeViewSynchronizer.requestMoreModifiers) and bootstrapping can never resume.
    // Only the in-memory flag is set; no heights are written here (isUtxoSnapshotApplied is
    // derived from the persisted minimalFullBlockHeight, which must stay untouched until the
    // snapshot is actually applied).
    // The persisted best header must be fresh: a stale or partial header chain does not
    // establish that headers sync completed (such a node transitions organically when the
    // next fresh header arrives, see ToDownloadProcessor). A header persisted during an
    // interrupted bootstrap is at the network tip, so recovery after a mid-bootstrap restart
    // is unaffected.
    stopOnFailure("UTXO bootstrap recovery", Try {
      if (nodeSettings.utxoSettings.utxoBootstrap &&
          history.bestHeaderOpt.exists(_.isNew(ergoSettings.chainSettings.blockInterval * nodeSettings.headerChainDiff)) &&
          !history.isUtxoSnapshotApplied) {
        history.setHeadersChainSynced()
      }
    })(history.closeStorage())

    log.info("History database read")
    history
  }

}
