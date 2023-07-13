package org.ergoplatform.nodeView.history.storage.modifierprocessors

import com.google.common.primitives.Ints
import org.ergoplatform.ErgoApp.CriticalSystemException
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultyAdjustment
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, GenesisHeight, Height}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.Constants.{HashLength, MakeSnapshotEvery}
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import scorex.core.consensus.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{InvalidModifier, ModifierValidator, ValidationResult, ValidationState}
import scorex.db.ByteArrayWrapper
import scorex.util._
import scorex.util.encode.Base16

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/**
  * Contains all functions required by History to process Headers.
  */
trait HeadersProcessor extends ToDownloadProcessor with PopowProcessor with ScorexLogging with ScorexEncoding {

  /**
    * Key for database record storing ID of best block header
    */
  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(Header.modifierTypeId))

  /**
    * Key for database record storing ID of best full block
    */
  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(HashLength)(-1))

  /**
    * Key for database record storing height of first full block stored
    */
  protected val MinFullBlockHeightKey = {
    // hash of "minfullheight" UTF-8 string
    ByteArrayWrapper(Base16.decode("4987eb6a8fecbed88a6f733f456cdf4e334b944f4436be4cab50cacb442e15e6").get)
  }


  protected val historyStorage: HistoryStorage

  protected val settings: ErgoSettings

  val powScheme: AutolykosPowScheme

  // Maximum time in future block header may have
  protected lazy val MaxTimeDrift: Long = 10 * chainSettings.blockInterval.toMillis

  lazy val difficultyCalculator = new DifficultyAdjustment(chainSettings)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  // todo for performance reasons we may just use key like s"score$id" but this will require to redownload blockchain
  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  protected[history] def validityKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))

  override def writeMinimalFullBlockHeight(height: Height): Unit = {
    historyStorage.insert(
      indexesToInsert = Array(MinFullBlockHeightKey -> Ints.toByteArray(height)),
      objectsToInsert = BlockSection.emptyArray)
  }

  override def readMinimalFullBlockHeight(): Height = {
    historyStorage.getIndex(MinFullBlockHeightKey).map(Ints.fromByteArray).getOrElse(ErgoHistory.GenesisHeight)
  }

  def bestHeaderIdOpt: Option[ModifierId] = historyStorage.getIndex(BestHeaderKey).map(bytesToId)

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestFullBlockIdOpt: Option[ModifierId] = None

  /**
    * @return height of best header
    */
  def headersHeight: Height = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @return height of best header with all block sections
    */
  def fullBlockHeight: Height = bestFullBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(ErgoHistory.EmptyHistoryHeight)

  /**
    * @param id - id of ErgoPersistentModifier
    * @return height of modifier with such id if is in History
    */
  def heightOf(id: ModifierId): Option[Int] = historyStorage.getIndex(headerHeightKey(id))
    .map(Ints.fromByteArray)

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h)).contains(id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).contains(h.id)

  override protected def process(h: Header, nipopowMode: Boolean = false): Try[ProgressInfo[BlockSection]] = synchronized {
    val dataToInsert: (Array[(ByteArrayWrapper, Array[Byte])], Array[BlockSection]) = toInsert(h, nipopowMode)

    historyStorage.insert(dataToInsert._1, dataToInsert._2).flatMap { _ =>
      bestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          // If we verify transactions, we don't need to send this header to state.
          // If we don't and this is the best header, we should send this header to state to update state root hash
          val toProcess = if (nodeSettings.verifyTransactions || !(bestHeaderId == h.id)) Seq.empty else Seq(h)
          Success(ProgressInfo(None, Seq.empty, toProcess, toDownload(h)))
        case None =>
          Failure(CriticalSystemException("History should always have best header on header application"))
      }
    }
  }

  /**
    * Data to add to and remove from the storage to process a header
    * @param h - header to be written into the storage
    * @param nipopowMode - flag showing whether header `h` is applied sequentially (so parent is already there), or
    *                     coming after a possible gap (during nipopow application). If true, a gap is possible.
    */
  private def toInsert(h: Header, nipopowMode: Boolean): (Array[(ByteArrayWrapper, Array[Byte])], Array[BlockSection]) = {
    //todo: construct resulting Array without ++

    val requiredDifficulty: Difficulty = h.requiredDifficulty
    val score = scoreOf(h.parentId).getOrElse(BigInt(0)) + requiredDifficulty

    // in nipopow comment, we consider that header we got is in best chain (which is guaranteed by the proof),
    // so we do not check chain's score (and we do not have it)
    val bestHeader = if (nipopowMode) {
      true
    } else {
      score > bestHeadersChainScore
    }

    val bestRow: Seq[(ByteArrayWrapper, Array[Byte])] =
      if (bestHeader) Seq(BestHeaderKey -> idToBytes(h.id)) else Seq.empty
    val scoreRow = headerScoreKey(h.id) -> score.toByteArray
    val heightRow = headerHeightKey(h.id) -> Ints.toByteArray(h.height)

    val headerIdsRow = if (bestHeader) {
      if (h.isGenesis) log.info(s"Processing genesis header ${h.encodedId}")
      if(nipopowMode) log.info(s"Processing nipopow header ${h.encodedId}")
      bestBlockHeaderIdsRow(h, score)
    } else {
      orphanedBlockHeaderIdsRow(h, score)
    }

    // checks if it is time to take nipopow proof
    // UTXO set snapshot is taken on last block of a UtXO set snapshot epoch
    // e.g. on a height h , where h % MakeSnapshotEvery == MakeSnapshotEvery - 1
    // and NiPoPoW proof is taken Constants.LastHeadersInContext before to have Constants.LastHeadersInContext
    // (actually, Constants.LastHeadersInContext + 1 even) consecutive headers before first full block to be validated
    val timeToTakeNipopowProof: Boolean = if (settings.nodeSettings.popowBootstrap) {
      // todo: currently the node is not taking NiPoPoW proof if was bootstrapped via a NiPoPoW itself,
      // todo: but actually, nipopows can be generated by such a node? check and enable if so
      false
    } else {
      this.isHeadersChainSynced &&
        h.height % MakeSnapshotEvery == MakeSnapshotEvery - 1 - Constants.LastHeadersInContext
    }

    val nipopowsRow = if (timeToTakeNipopowProof) {
      val ts0 = System.currentTimeMillis()
      popowProofBytes() match {
        case Success(pbs) =>
          val ts = System.currentTimeMillis()
          log.info(s"Dumping nipopow proof (size: ${pbs.length}) @ ${h.height}, generated in ${ts - ts0} ms.")
          Array(NipopowSnapshotHeightKey -> pbs)
        case Failure(e) =>
          log.error("Nipopow proof generation failed ", e)
          Array.empty
      }
    } else {
      Array.empty
    }

    (Array(scoreRow, heightRow) ++ bestRow ++ headerIdsRow ++ nipopowsRow, Array(h))
  }

  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, Array[Byte])] = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> (headerIdsAtHeight(h.height) :+ h.id).flatMap(idToBytes).toArray)
  }

  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, Array[Byte])] = {
    val prevHeight = headersHeight
    log.info(s"New best header ${h.encodedId} with score $score. New height ${h.height}, old height $prevHeight")
    val self: (ByteArrayWrapper, Array[Byte]) =
      heightIdsKey(h.height) -> (Seq(h.id) ++ headerIdsAtHeight(h.height)).flatMap(idToBytes).toArray
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, Array[Byte])] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => id != header.id)
      heightIdsKey(header.height) -> (Seq(header.id) ++ otherIds).flatMap(idToBytes).toArray
    }
    forkIds :+ self
  }

  /** Validates given header
    *
    * @return Success() if header is valid, Failure(error) otherwise
    */
  protected def validate(header: Header): Try[Unit] = HeaderValidator.validate(header).toTry

  /**
    * @param id - header id
    * @return score of header with such id if is in History
    */
  def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.getIndex(headerScoreKey(id))
    .map(BigInt.apply)

  /**
    * Get main chain header id
    *
    * Optimized version of headerIdsAtHeight(height).headOption
    *
    * @param height - height to get header id at
    * @return - header id or None
    */
  def bestHeaderIdAtHeight(height: Int): Option[ModifierId] = {
    historyStorage.getIndex(heightIdsKey(height: Int)).map { bs =>
      // in 99% cases bs.length == 32 (no orphaned headers)
      if (bs.length == 32) {
        bytesToId(bs)
      } else {
        bytesToId(bs.take(32))
      }
    }
  }

  def bestHeaderAtHeight(h: Int): Option[Header] = bestHeaderIdAtHeight(h).flatMap { id =>
    typedModifierById[Header](id)
  }

  /**
    * @note this method implementation should be changed along with `bestHeaderIdAtHeight`
    *
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    historyStorage.getIndex(heightIdsKey(height: Int))
      .getOrElse(Array()).grouped(32).map(bytesToId).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: scala.collection.mutable.Buffer[Header]): Seq[Header] = {
      if (acc.lengthCompare(limit) == 0 || until(header)) {
        acc
      } else {
        typedModifierById[Header](header.parentId) match {
          case Some(parent: Header) =>
            loop(parent, acc += parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc += header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) {
      HeaderChain(ArrayBuffer.empty)
    } else {
      HeaderChain(loop(startHeader, ArrayBuffer.empty += startHeader).reverse)
    }
  }

  /**
    * Find first header with height <= `height` which id satisfies condition `p`
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = {
    headerIdsAtHeight(height).find(id => p(id)).flatMap(id => typedModifierById[Header](id)) match {
      case Some(header) => Some(header)
      case None if height > 0 => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  private def bestHeadersChainScore: BigInt = bestHeaderIdOpt.flatMap(id => scoreOf(id)).getOrElse(0)

  protected def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  /**
    * Calculate difficulty for the next block
    *
    * @param parent - latest block
    * @return - difficulty for the next block
    */
  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val parentHeight = parent.height

    val eip37ActivationHeight = 844673

    if (settings.networkType.isMainNet && parentHeight + 1 >= eip37ActivationHeight) {
      val epochLength = 128 // epoch length after EIP-37 activation
      if (parentHeight % epochLength == 0) {
        val heights = difficultyCalculator.previousHeightsRequiredForRecalculation(parentHeight + 1, epochLength)
        val headers = if (historyReader.isInBestChain(parent)) {
          heights.flatMap(height => historyReader.bestHeaderAtHeight(height))
        } else {
          val chain = headerChainBack(heights.max - heights.min + 1, parent, _ => false)
          chain.headers.filter(p => heights.contains(p.height))
        }
        difficultyCalculator.eip37Calculate(headers, chainSettings.eip37EpochLength.get) // .get is ok for the mainnet
      } else {
        parent.requiredDifficulty
      }
    } else {
      if (parentHeight == settings.chainSettings.voting.version2ActivationHeight ||
          parent.height + 1 == settings.chainSettings.voting.version2ActivationHeight) {
        // Set difficulty for version 2 activation height (where specific difficulty is needed due to PoW change)
        settings.chainSettings.initialDifficultyVersion2
      } else {
        val epochLength = settings.chainSettings.epochLength

        if (parentHeight % epochLength == 0) {
          val heights = difficultyCalculator.previousHeightsRequiredForRecalculation(parentHeight + 1, epochLength)
            .ensuring(_.last == parentHeight)
          if (heights.lengthCompare(1) == 0) {
            difficultyCalculator.calculate(Array(parent), epochLength)
          } else {
            val headers = if (historyReader.isInBestChain(parent)) {
              heights.flatMap(height => historyReader.bestHeaderAtHeight(height))
            } else {
              val chain = headerChainBack(heights.max - heights.min + 1, parent, _ => false)
              chain.headers.filter(p => heights.contains(p.height))
            }
            difficultyCalculator.calculate(headers, epochLength)
          }
        } else {
          parent.requiredDifficulty
        }
      }
    }
  }

  private object HeaderValidator extends ScorexEncoding {

    private def validationState: ValidationState[Unit] = ModifierValidator(ErgoValidationSettings.initial)

    private def time(): ErgoHistory.Time = System.currentTimeMillis()

    def validate(header: Header): ValidationResult[Unit] = {
      if (header.isGenesis) {
        validateGenesisBlockHeader(header)
      } else {
        val parentOpt = typedModifierById[Header](header.parentId)
        parentOpt map { parent =>
          validateChildBlockHeader(header, parent)
        } getOrElse {
          validationState.validate(hdrParent, condition = false, InvalidModifier(Algos.encode(header.parentId), header.id, header.modifierTypeId))
        }
      }
    }

    private def validateGenesisBlockHeader(header: Header): ValidationResult[Unit] = {
      validationState
        .validateEqualIds(hdrGenesisParent, header.parentId, Header.GenesisParentId, header.modifierTypeId)
        .validateOrSkipFlatten(hdrGenesisFromConfig, chainSettings.genesisId, (id: ModifierId) => id.equals(header.id), header.id, header.modifierTypeId)
        .validate(hdrGenesisHeight, header.height == GenesisHeight, InvalidModifier(header.toString, header.id, header.modifierTypeId))
        .validateNoFailure(hdrPoW, powScheme.validate(header), header.id, header.modifierTypeId)
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, chainSettings.initialDifficulty, header.id, header.modifierTypeId)
        .validateNot(alreadyApplied, historyStorage.contains(header.id), InvalidModifier(header.toString, header.id, header.modifierTypeId))
        .validate(hdrTooOld, fullBlockHeight < nodeSettings.keepVersions, InvalidModifier(heightOf(header.parentId).toString, header.id, header.modifierTypeId))
        .validate(hdrFutureTimestamp, header.timestamp - time() <= MaxTimeDrift, InvalidModifier(s"${header.timestamp} vs ${time()}", header.id, header.modifierTypeId))
        .result
    }

    /**
      * Validate non-genesis block header.
      */
    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult[Unit] = {
      validationState
        .validate(hdrNonIncreasingTimestamp, header.timestamp > parent.timestamp, InvalidModifier(s"${header.timestamp} > ${parent.timestamp}", header.id, header.modifierTypeId))
        .validate(hdrHeight, header.height == parent.height + 1, InvalidModifier(s"${header.height} vs ${parent.height}", header.id, header.modifierTypeId))
        .validateNoFailure(hdrPoW, powScheme.validate(header), header.id, header.modifierTypeId)
        .validateEquals(hdrRequiredDifficulty, header.requiredDifficulty, requiredDifficultyAfter(parent), header.id, header.modifierTypeId)
        .validate(hdrTooOld, heightOf(header.parentId).exists(h => fullBlockHeight - h < nodeSettings.keepVersions), InvalidModifier(heightOf(header.parentId).toString, header.id, header.modifierTypeId))
        .validateSemantics(hdrParentSemantics, isSemanticallyValid(header.parentId), InvalidModifier(s"Parent semantics broken", header.id, header.modifierTypeId))
        .validate(hdrFutureTimestamp, header.timestamp - time() <= MaxTimeDrift, InvalidModifier(s"${header.timestamp} vs ${time()}", header.id, header.modifierTypeId))
        .validateNot(alreadyApplied, historyStorage.contains(header.id), InvalidModifier(s"${header.id} already applied", header.id, header.modifierTypeId))
        .validate(hdrCheckpoint, checkpointCondition(header), InvalidModifier(s"${header.id} wrong checkpoint", header.id, header.modifierTypeId))
        .result
    }

    /**
      * Helper method to validate checkpoint given in config, if it is provided.
      *
      * Checks that block at checkpoint height has id provided.
      */
    private def checkpointCondition(header: Header): Boolean = {
      settings.nodeSettings.checkpoint.map { checkpoint =>
        if (checkpoint.height == header.height) {
          header.id == checkpoint.blockId
        } else {
          true
        }
      }.getOrElse(true)
    }

  }

}
