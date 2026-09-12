package org.ergoplatform.nodeView.history

import akka.actor.ActorContext
import org.ergoplatform.consensus.ProgressInfo

import java.io.File
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.{Header, PreGenesisHeader}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, NonHeaderBlockSection}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.StartExtraIndexer
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{IndexedHeightKey, NewestVersion, NewestVersionBytes, SchemaVersionKey, getIndex}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.LoggingUtil
import org.ergoplatform.validation.RecoverableModifierError
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.collection.mutable.ArrayBuffer
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

  // maximum number of headers above best full block checked for corruption during repair
  private val MaxHeadersToRepair: Int = 1000

  def historyDir(settings: ErgoSettings): File = {
    val dir = new File(s"${settings.directory}/history")
    dir.mkdirs()
    dir
  }

  /**
    * Repair history database after possible corruption (e.g. caused by disk overflow which
    * could lead to lost or partial writes).
    *
    * Two kinds of problems are checked and fixed:
    *
    * 1) suspicious continuation: there is a header after the recognized blockchain tip
    *    marked as invalid (it is forgotten then, to be re-downloaded)
    *
    * 2) stuck full block chain: the full block chain is behind the headers chain and cannot
    *    advance because of corrupted or invalidated block sections (or headers), or lost
    *    height indexes. In this case the node would never re-download damaged modifiers
    *    (they are considered as stored in history) and never apply them. Repair removes
    *    such damaged records and validity marks (for headers and block sections of the
    *    best headers chain above the best full block), restores lost height -> header ids
    *    indexes, and truncates the headers chain (to be re-downloaded) if a header record
    *    is found corrupted or missing. State and already applied full blocks are not
    *    touched, so no full resync is needed.
    *
    * @return true if any repair was done
    */
  protected[nodeView] def repairIfNeeded(history: ErgoHistory): Boolean = history.historyStorage.synchronized {
    val suspiciousContinuationRepaired = repairSuspiciousContinuation(history)
    val stuckFullChainRepaired = repairStuckFullChain(history)
    suspiciousContinuationRepaired || stuckFullChainRepaired
  }

  /**
    * Check if there is possible database corruption when there is a header after the
    * recognized blockchain tip marked as invalid, and clear it
    */
  private def repairSuspiciousContinuation(history: ErgoHistory): Boolean = {
    val bestHeaderHeight = history.headersHeight
    val bestFullBlockHeight = history.bestFullBlockOpt.map(_.height).getOrElse(-1)
    val afterHeaders = history.headerIdsAtHeight(bestHeaderHeight + 1)

    if (bestHeaderHeight == bestFullBlockHeight && afterHeaders.nonEmpty) {
      log.warn("Found suspicious continuation, clearing it...")
      afterHeaders.foreach(hId => history.forgetHeader(hId))
      history.historyStorage.remove(Array(history.heightIdsKey(bestHeaderHeight + 1)), Array.empty[ModifierId])
      true
    } else {
      false
    }
  }

  /**
    * Read header from history database, returning None if the record is missing or corrupted
    * (fails to parse). The read is done bypassing in-memory caches, as repair procedures
    * must detect corrupted records even if parsed copies of them are cached.
    */
  private def readHeader(history: ErgoHistory, id: ModifierId): Option[Header] = {
    history.historyStorage.modifierByIdFromDb(id).collect { case h: Header => h }
  }

  /**
    * @return true if modifier is marked as semantically invalid in history indexes
    */
  private def isMarkedInvalid(history: ErgoHistory, id: ModifierId): Boolean = {
    history.historyStorage.getIndex(history.validityKey(id)).exists(_.headOption.contains(0.toByte))
  }

  /**
    * Repair full block chain stuck after history database corruption: walk best headers chain down
    * to the best full block, and fix block sections and indexes of the headers above the best full block,
    * so that full blocks syncing can proceed (damaged modifiers get re-downloaded from the network).
    *
    * @return true if any repair was done
    */
  private def repairStuckFullChain(history: ErgoHistory): Boolean = {
    val bestFullBlockOpt = history.bestFullBlockOpt
    if (bestFullBlockOpt.isEmpty) {
      // nothing to do if there are no full blocks (e.g. headers-only regime or empty history)
      false
    } else {
      val fullHeight = bestFullBlockOpt.get.height

      // headers of the best headers chain above last full block, collected from top to bottom
      val headersAbove = ArrayBuffer.empty[Header]
      // id of a header which record is corrupted or missing (then the chain above it is truncated)
      var damagedHeaderIdOpt: Option[ModifierId] = None

      var currentOpt: Option[Header] = history.bestHeaderIdOpt.flatMap(readHeader(history, _))
      var checked = 0
      while (currentOpt.exists(_.height > fullHeight) && checked < MaxHeadersToRepair) {
        val header = currentOpt.get
        headersAbove += header
        checked += 1
        if (header.height > fullHeight + 1) {
          readHeader(history, header.parentId) match {
            case Some(parent) =>
              currentOpt = Some(parent)
            case None =>
              // parent header record is corrupted or missing, the chain above it cannot
              // be used and is to be truncated (headers are re-downloaded starting from it)
              damagedHeaderIdOpt = Some(header.parentId)
              currentOpt = None
          }
        } else {
          // parent of the last collected header is the best full block (or below), walk is done
          currentOpt = None
        }
      }

      damagedHeaderIdOpt match {
        case Some(damagedHeaderId) =>
          truncateHeadersChain(history, headersAbove, damagedHeaderId)
        case None =>
          repairBlockSections(history, headersAbove)
      }
    }
  }

  /**
    * Remove corrupted or missing header `damagedHeaderId` and all headers above it (`headersAbove`,
    * from the top of the chain down to the child of the damaged header), resetting best header to the
    * highest readable header below the damaged one. Removed headers and their block sections are
    * going to be re-downloaded from the network.
    *
    * @return true if repair was done
    */
  private def truncateHeadersChain(history: ErgoHistory,
                                   headersAbove: ArrayBuffer[Header],
                                   damagedHeaderId: ModifierId): Boolean = {
    val storage = history.historyStorage
    val removedIds = headersAbove.map(_.id).toSet + damagedHeaderId
    val damagedHeightOpt = headersAbove.lastOption.map(_.height - 1)

    // remove headers above the damaged one, with their block sections and indexes
    headersAbove.foreach(h => history.forgetHeader(h.id))

    // remove damaged header record if it exists (its bytes may be corrupted)
    storage.remove(
      indicesToRemove = Array(history.validityKey(damagedHeaderId),
        history.headerHeightKey(damagedHeaderId),
        history.headerScoreKey(damagedHeaderId)),
      idsToRemove = Array(damagedHeaderId))

    // clean height -> header ids indexes at heights of removed headers
    val heightsToClean = headersAbove.map(_.height).toSet ++ damagedHeightOpt.toSet
    heightsToClean.foreach { h =>
      removeFromHeightIndex(history, h, removedIds)
    }

    // reset best header to the highest readable header below the damaged one: first try
    // the best header id at the height just below the damaged header, then walk down
    // looking for any readable header (in case of multiple corrupted records)
    val newBestHeaderOpt = damagedHeightOpt
      .flatMap(h => history.bestHeaderIdAtHeight(h))
      .flatMap(readHeader(history, _))
      .orElse(damagedHeightOpt.flatMap(h => history.loopHeightDown(h, id => readHeader(history, id).isDefined)))

    newBestHeaderOpt match {
      case Some(newBestHeader) =>
        log.warn(s"Truncated headers chain at height ${damagedHeightOpt.get}, " +
          s"new best header: ${newBestHeader.encodedId} at height ${newBestHeader.height}")
        storage.insert(Array(history.BestHeaderKey -> idToBytes(newBestHeader.id)), Array.empty[BlockSection])
        true
      case None =>
        log.error(s"Could not find a readable header below damaged header $damagedHeaderId, " +
          s"the chain is likely broken and a full resync is needed")
        false
    }
  }

  /**
    * Remove given ids from the index of header ids at given height (or remove the index completely
    * if no ids left)
    */
  private def removeFromHeightIndex(history: ErgoHistory, height: Int, idsToRemove: Set[ModifierId]): Unit = {
    val ids = history.headerIdsAtHeight(height)
    val remaining = ids.filterNot(idsToRemove)
    if (remaining.length != ids.length) {
      if (remaining.isEmpty) {
        history.historyStorage.remove(Array(history.heightIdsKey(height)), Array.empty[ModifierId])
      } else {
        history.historyStorage.insert(
          Array(history.heightIdsKey(height) -> remaining.flatMap(idToBytes).toArray),
          Array.empty[BlockSection])
      }
    }
  }

  /**
    * Repair block sections of given headers (of the best headers chain above the best full block):
    *   - block sections marked as invalid get their validity marks cleared and their records removed,
    *     so they can be re-downloaded and re-applied
    *   - block section records which fail to parse (corrupted) are removed
    *   - a header with all the required block sections present but never applied (possible after
    *     interrupted write) gets its block sections removed, to trigger block processing when they
    *     are re-downloaded
    *   - lost or damaged height -> header ids indexes are restored
    *
    * @return true if any repair was done
    */
  private def repairBlockSections(history: ErgoHistory, headersAbove: Seq[Header]): Boolean = {
    val storage = history.historyStorage
    var repaired = false

    headersAbove.foreach { header =>
      val requiredSectionIds = history.requiredModifiersForHeader(header).map(_._2)
      val headerInvalid = isMarkedInvalid(history, header.id)

      val sectionsToRemove = ArrayBuffer.empty[ModifierId]
      val validityKeysToRemove = ArrayBuffer.empty[ByteArrayWrapper]

      // schedule all the required sections of the header for removal (with their validity marks),
      // so that they are re-downloaded and the block is applied anew
      def removeRequiredSections(): Unit = {
        requiredSectionIds.foreach { sectionId =>
          sectionsToRemove += sectionId
          validityKeysToRemove += history.validityKey(sectionId)
        }
      }

      if (headerInvalid) {
        log.warn(s"Header ${header.encodedId} at height ${header.height} is marked as invalid, clearing the mark")
        validityKeysToRemove += history.validityKey(header.id)
      }

      val corruptedOrInvalidSections = requiredSectionIds.filter { sectionId =>
        val invalid = isMarkedInvalid(history, sectionId)
        val corrupt = storage.contains(sectionId) && storage.modifierByIdFromDb(sectionId).isEmpty
        if (corrupt) {
          log.warn(s"Block section $sectionId of header ${header.encodedId} at height ${header.height} " +
            s"is corrupted, removing it")
        }
        invalid || corrupt
      }

      val allSectionsPresentAndValid = !headerInvalid && requiredSectionIds.nonEmpty &&
        corruptedOrInvalidSections.isEmpty &&
        requiredSectionIds.forall(sectionId => storage.modifierByIdFromDb(sectionId).isDefined)

      if (headerInvalid || corruptedOrInvalidSections.nonEmpty) {
        // the header or some of its required sections is invalid or corrupted, so all the required
        // sections are removed: remaining sections of a damaged block may be corrupted as well,
        // and sections already in storage would not be re-processed otherwise
        removeRequiredSections()
        repaired = true
      } else if (allSectionsPresentAndValid) {
        // all the required sections are in storage and valid, but the block was never applied
        // (possible when objects were written but the following index update was interrupted),
        // removing the sections to re-trigger block processing after they are re-downloaded
        log.warn(s"Block at height ${header.height} with header ${header.encodedId} is complete but was never " +
          s"applied to the full chain, removing its sections to re-process the block")
        removeRequiredSections()
        repaired = true
      }

      // clear invalidity marks of non-required sections (e.g. ADProofs in UTXO mode), their records are kept
      header.sectionIds.map(_._2).filterNot(requiredSectionIds.contains).foreach { optionalSectionId =>
        if (isMarkedInvalid(history, optionalSectionId)) {
          validityKeysToRemove += history.validityKey(optionalSectionId)
        }
      }

      if (sectionsToRemove.nonEmpty || validityKeysToRemove.nonEmpty) {
        storage.remove(validityKeysToRemove.distinct.toArray, sectionsToRemove.distinct.toArray)
      }

      // restore height -> header ids index if it is lost or does not contain the header
      // (without it full blocks downloading stops at this height)
      val idsAtHeight = history.headerIdsAtHeight(header.height)
      if (!idsAtHeight.contains(header.id)) {
        log.warn(s"Height index at height ${header.height} is missing header ${header.encodedId}, restoring it")
        storage.insert(
          Array(history.heightIdsKey(header.height) -> (Seq(header.id) ++ idsAtHeight).flatMap(idToBytes).toArray),
          Array.empty[BlockSection])
        repaired = true
      }
    }

    repaired
  }

  /**
    * @return ErgoHistory instance with new database or database read from existing folder
    */
  def readOrGenerate(ergoSettings: ErgoSettings)(implicit context: ActorContext): ErgoHistory = {
    var db = HistoryStorage(ergoSettings)

    // ExtraIndexer db check
    if(ergoSettings.nodeSettings.extraIndex) { // check db schema
      val schemaVersion: Int = getIndex(SchemaVersionKey, db).getInt
      if (schemaVersion != NewestVersion) {
        if(getIndex(IndexedHeightKey, db).getInt > 0)
          db = db.deleteExtraDB(ergoSettings) // older schema -> delete and reopen db
        db.insertExtra(Array((SchemaVersionKey, NewestVersionBytes)), Array.empty) // update version key
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
