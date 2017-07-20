package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, PoPoWProof}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

//TODO replace ErgoPersistentModifier to HistoryModifier
trait ErgoHistory extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
  with HeadersProcessor
  with ADProofsProcessor
  with BlockTransactionsProcessor
  with ScorexLogging {

  protected val config: HistoryConfig
  protected val storage: LSMStore

  lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)

  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty


  //It is safe to call this function right after history initialization with genesis block
  def bestHeader: Header = bestHeaderOpt.get

  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap { id =>
    historyStorage.modifierById(id) match {
      case Some(h: Header) => Some(h)
      case _ => None
    }
  }

  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = historyStorage.modifierById(id)

  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = Try {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} to history")
    applicableTry(modifier).get
    val env = ModifierProcessorEnvironment(BigInt(1))
    modifier match {
      case m: Header =>
        assert(isEmpty || (bestHeaderIdOpt sameElements bestHeaderIdOpt), "History is inconsistent")
        //TODO calculate
        val dataToInsert = toInsert(m, env)
        historyStorage.insert(m.id, dataToInsert)
        if (bestHeaderIdOpt sameElements bestHeaderIdOpt) {
          log.info(s"New orphaned header ${m.encodedId}")
          (this, ProgressInfo(None, Seq(), Seq()))
        } else {
          log.info(s"New best header ${m.encodedId}")
          //TODO Notify node view holder that it should download transactions ?
          (this, ProgressInfo(None, Seq(), Seq()))
        }
      case m: BlockTransactions =>
        (this, process(m))
      case m: ADProofs =>
        (this, process(m))
      case m: PoPoWProof =>
        //        storage.insert(m)

        ???
      case m =>
        throw new Error(s"Modifier $m have incorrect type")
    }
  }

  override def compare(other: ErgoSyncInfo): HistoryComparisonResult.Value = ???


  override def reportInvalid(modifier: ErgoPersistentModifier): ErgoHistory = {
    val idsToRemove = modifier match {
      case h: Header => toDrop(h)
      case _ => ???
    }
    historyStorage.drop(modifier.id, idsToRemove)
    this
  }

  override def openSurfaceIds(): Seq[ModifierId] = ???

  override def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = Try {
    modifier match {
      case m: Header =>
        validate(m).get
      case m: BlockTransactions =>
        validate(m)
      case m: ADProofs =>
        validate(m)
      case m: PoPoWProof =>
        ???
      case m =>
        throw new Error(s"Modifier $m have incorrect type")
    }
  }

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  //TODO last full blocks and last headers
  override def syncInfo(answer: Boolean): ErgoSyncInfo = ???

  private def headerChainBack(count: Int, startBlock: Header, until: Header => Boolean): Seq[Header] = {
    @tailrec
    def loop(remain: Int, block: Header, acc: Seq[Header]): Seq[Header] = {
      if (until(block) || remain == 0) {
        acc
      } else {
        modifierById(block.parentId) match {
          case Some(parent: Header) =>
            loop(remain - 1, parent, acc :+ parent)
          case _ =>
            log.warn(s"No parent header in history for block ${block.encodedId}")
            acc
        }
      }
    }
    if (isEmpty) Seq()
    else loop(count, startBlock, Seq(startBlock)).reverse
  }

  override type NVCT = ErgoHistory

}

object ErgoHistory extends ScorexLogging {

  def readOrGenerate(settings: ErgoSettings): ErgoHistory = {
    val dataDir = settings.dataDir
    val iFile = new File(s"$dataDir/history")
    iFile.mkdirs()
    val db = new LSMStore(iFile, maxJournalEntryCount = 10000)

    val historyConfig: HistoryConfig = HistoryConfig(settings.poPoWBootstrap, settings.blocksToKeep, settings.minimalSuffix)

    val history = if (!settings.verifyTransactions) {
      new ErgoHistory with EmptyADProofsProcessor with EmptyBlockTransactionsProcessor {
        override protected val config: HistoryConfig = historyConfig
        override protected val storage: LSMStore = db
      }
    } else {
      new ErgoHistory with FullnodeADProofsProcessor with FullnodeBlockTransactionsProcessor {
        override protected val config: HistoryConfig = historyConfig
        override protected val storage: LSMStore = db
      }
    }
    if (history.isEmpty) {
      log.info("Initialize empty history with genesis block")
      val genesis: ErgoFullBlock = {
        val genesisTimestamp = 1500203225564L
        val initialState = ErgoState.initialState
        val stateRoot = initialState.rootHash()
        val genesisTx = new AnyoneCanSpendTransaction(
          initialState.anyoneCanSpendBoxesAtHeight(0).map(b => (b.proposition, b.value)),
          IndexedSeq((new AnyoneCanSpendProposition, 0L)),
          genesisTimestamp)

        val header: Header = Header(0.toByte,
          Array.fill(32)(0.toByte),
          Seq(),
          Algos.EmptyMerkleTreeRoot,
          stateRoot: Array[Byte],
          BlockTransactions.rootHash(Seq(genesisTx.id)),
          genesisTimestamp,
          0)
        val blockTransactions: BlockTransactions = BlockTransactions(header.id, Seq(genesisTx))
        val aDProofs: ADProofs = ADProofs(header.id, Array())
        ErgoFullBlock(header, blockTransactions, aDProofs)
      }

      val historyWithHeader = history.append(genesis.header).get._1
      if (settings.verifyTransactions) historyWithHeader.append(genesis.aDProofs).get._1.append(genesis.blockTransactions).get._1
      else historyWithHeader
    } else {
      log.info("Initialize non-empty history ")
      history
    }
  }
}


