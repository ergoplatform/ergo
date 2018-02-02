package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs.{ADProofsProcessor, ADStateProofsProcessor, EmptyADProofsProcessor, FullStateProofsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions.{BlockTransactionsProcessor, EmptyBlockTransactionsProcessor, FullnodeBlockTransactionsProcessor}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.popow.{EmptyPoPoWProofsProcessor, FullPoPoWProofsProcessor, PoPoWProofsProcessor}
import org.ergoplatform.nodeView.history.storage.{FilesObjectsStore, HistoryStorage}
import org.ergoplatform.settings.{ChainSettings, ErgoSettings, NodeConfigurationSettings}
import scorex.core._
import scorex.core.consensus.History
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * History implementation. It is processing persistent modifiers generated locally or coming from network.
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
  extends History[ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
    with ErgoHistoryReader
    with HeadersProcessor
    with ADProofsProcessor
    with PoPoWProofsProcessor
    with UTXOSnapshotChunkProcessor
    with BlockTransactionsProcessor
    with ScorexLogging {

  override type NVCT = ErgoHistory

  /**
    * Append ErgoPersistentModifier to History if valid
    */
  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    applicableTry(modifier).map { _ =>
      modifier match {
        case header: Header =>
          (this, process(header))
        case blockTransactions: BlockTransactions =>
          (this, process(blockTransactions))
        case aDProofs: ADProofs =>
          (this, process(aDProofs))
        case poPoWProof: PoPoWProof =>
          (this, process(poPoWProof))
        case chunk: UTXOSnapshotChunk =>
          (this, process(chunk))
      }
    }
  }


  /**
    * Report some modifier as valid or invalid semantically
    */
  override def reportSemanticValidity(modifier: ErgoPersistentModifier,
                                      valid: Boolean,
                                      unusedParam: ModifierId): (ErgoHistory, ProgressInfo[ErgoPersistentModifier]) = {
    def validityRowsForHeader(h: Header): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
      Seq(h.id, h.transactionsId, h.ADProofsId).map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte)))
    }

    if (valid) {
      modifier match {
        case fb: ErgoFullBlock =>
          val bestHeader = bestHeaderOpt.get
          val nonMarkedIds = (Seq(fb.header.id, fb.blockTransactions.id) ++ fb.aDProofs.map(_.id))
            .filter(id => historyStorage.getIndex(validityKey(id)).isEmpty)

          if (nonMarkedIds.nonEmpty) {
            historyStorage.insert(validityKey(nonMarkedIds.head),
              nonMarkedIds.map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte))),
              Seq())
          }
          if (bestFullBlockOpt.contains(fb)) {
            //applied best header to history
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
          } else {
            //in fork processing
            val modHeight = heightOf(fb.header.id).get
            val chainBack = headerChainBack(headersHeight - modHeight, bestHeader, h => h.parentId sameElements fb.header.id)
            //block in the best chain that link to this header
            val toApply = chainBack.headOption.flatMap(opt => getFullBlock(opt))
              .ensuring(_.get.header.parentId sameElements fb.header.id, "Should never be here, State is inconsistent")
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), toApply, Seq())
          }
        case _ =>
          historyStorage.insert(validityKey(modifier.id),
            Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(1.toByte))),
            Seq())
          this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
      }

    } else {
      val headerOpt: Option[Header] = modifier match {
        case h: Header => Some(h)
        case full: ErgoFullBlock => Some(full.header)
        case proof: ADProofs => typedModifierById[Header](proof.headerId)
        case txs: BlockTransactions => typedModifierById[Header](txs.headerId)
        case _ => None
      }
      headerOpt match {
        case Some(h) =>
          val invalidatedHeaders = continuationHeaderChains(h).flatMap(_.headers).distinct
          log.info(s"Invalidated header ${h.encodedId} and linked ${invalidatedHeaders.map(_.encodedId).mkString(",")}")
          val validityRow = invalidatedHeaders.flatMap(h => validityRowsForHeader(h))

          def isStillValid(id: ModifierId): Boolean = !invalidatedHeaders.exists(_.id sameElements id)

          def loopHeightDown(height: Int): Header = {
            assert(height >= 0, s"Mark genesis invalid is not true")
            headerIdsAtHeight(height).find(id => isStillValid(id)).flatMap(id => typedModifierById[Header](id)) match {
              case Some(header) => header
              case None => loopHeightDown(height - 1)
            }
          }

          val branchValidHeader: Header = loopHeightDown(headersHeight)
          val bestValidFullOpt: Option[Header] = bestFullBlockOpt.flatMap(h => heightOf(h.header.id))
            .map(loopHeightDown)

          if (bestHeaderOpt.contains(branchValidHeader) && bestFullBlockOpt.forall(b => bestValidFullOpt.contains(b))) {
            historyStorage.insert(validityKey(modifier.id), validityRow, Seq())
            this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
          } else {
            val changedLinks = bestValidFullOpt.toSeq.map(h => BestFullBlockKey -> ByteArrayWrapper(h.id)) :+
              (BestHeaderKey, ByteArrayWrapper(branchValidHeader.id))
            val (validChain, invalidatedChain) = ((bestValidFullOpt, bestFullBlockOpt) match {
              case (Some(bestValid), Some(bestFull)) =>
                val headersChain = commonBlockThenSuffixes(bestValid, bestFull.header)
                (headersChain._1.headers.flatMap(h => getFullBlock(h)), headersChain._2.headers.flatMap(h => getFullBlock(h)))
              case _ =>
                val headersChain = commonBlockThenSuffixes(branchValidHeader, bestHeaderOpt.get)
                (headersChain._1.headers, headersChain._2.headers)
            }).ensuring(c => c._1.head == c._2.head)
            val branchPoint: Some[ModifierId] = invalidatedChain.head match {
              case fullBlock: ErgoFullBlock => Some(fullBlock.header.id)
              case header: Header => Some(header.id)
            }

            val toInsert = validityRow ++ changedLinks
            historyStorage.insert(validityKey(modifier.id), toInsert, Seq())

            //TODO ???
            this -> ProgressInfo[ErgoPersistentModifier](branchPoint, invalidatedChain.tail,
              validChain.tail.headOption, Seq())
          }
        case None =>
          historyStorage.insert(validityKey(modifier.id),
            Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(0.toByte))),
            Seq())
          this -> ProgressInfo[ErgoPersistentModifier](None, Seq(), None, Seq())
      }
    }
  }

}

object ErgoHistory extends ScorexLogging {

  type Height = Int
  type Score = BigInt
  type Difficulty = BigInt
  type NBits = Long

  val GenesisHeight = 0

  def historyDir(settings: ErgoSettings):File = {
    val dir = new File(s"${settings.directory}/history")
    dir.mkdirs()
    dir
  }

  def readOrGenerate(settings: ErgoSettings, ntp: NetworkTimeProvider): ErgoHistory = {
    val historyFolder = historyDir(settings)
    historyFolder.mkdirs()
    val indexStore = new LSMStore(historyFolder, keepVersions = 0)
    val objectsStore = new FilesObjectsStore(historyFolder.getAbsolutePath)
    val db = new HistoryStorage(indexStore, objectsStore)
    val nodeSettings = settings.nodeSettings

    val history: ErgoHistory = (nodeSettings.ADState, nodeSettings.verifyTransactions, nodeSettings.PoPoWBootstrap) match {
      case (true, true, true) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (true, true, false) =>
        new ErgoHistory with ADStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (false, true, true) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (false, true, false) =>
        new ErgoHistory with FullStateProofsProcessor
          with FullnodeBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (_, false, true) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with FullPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (_, false, false) =>
        new ErgoHistory with EmptyADProofsProcessor
          with EmptyBlockTransactionsProcessor
          with EmptyPoPoWProofsProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val config: NodeConfigurationSettings = nodeSettings
          override protected val historyStorage: HistoryStorage = db
          override val powScheme: PoWScheme = chainSettings.poWScheme
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case m =>
        throw new Error(s"Unsupported settings combination ADState==${m._1}, verifyTransactions==${m._2}, " +
          s"poPoWBootstrap==${m._1}")
    }
    history
  }
}