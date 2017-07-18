package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, PoPoWProof}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
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
    with ScorexLogging {

  val config: HistoryConfig

  lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)
  //TODO .get.asInstanceOf ??
  def bestHeader: Header = modifierById(bestHeaderId).get.asInstanceOf[Header]
  def bestHeaderIdWithTransactions: ModifierId = ???

  override lazy val isEmpty: Boolean = Try(bestHeaderId).isFailure

  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = historyStorage.modifierById(id)

  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = Try {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} to history")
    applicableTry(modifier).get
    modifier match {
      case m: Header =>
        assert(isEmpty || (bestHeaderId sameElements bestHeaderId), "History is inconsistent")
        //TODO calculate
        val env = ModifierProcessorEnvironment(BigInt(1))
        val indexesRow = indexes(m, env)
        historyStorage.insert(m, indexesRow)
        if (bestHeaderId sameElements bestHeaderId) {
          log.info(s"New orphaned header ${m.encodedId}")
          (this, ProgressInfo(None, Seq(), Seq()))
        } else {
          log.info(s"New best header ${m.encodedId}")
          //TODO Notify node view holder that it should download transactions ?
          (this, ProgressInfo(None, Seq(), Seq()))
        }
      case m: BlockTransactions =>
        //        storage.insert(m)

        ???
      case m: ADProofs =>
        //        storage.insert(m)

        ???
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
      case h: Header => idsToDrop(h)
      case _ => ???
    }
    historyStorage.drop(modifier.id, idsToRemove)
    this
  }

  override def openSurfaceIds(): Seq[ModifierId] = ???

  override def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = Try {
    modifier match {
      case m: Header if m.isGenesis =>
        require(isEmpty, "Trying to append genesis block to non-empty history")
      case m: Header =>
        val parentOpt = modifierById(m.parentId)
        require(parentOpt.isDefined, "Parent header is no defined")
        require(!contains(m.id), "Header is already in history")
      //TODO require(Algos.blockIdDifficulty(m.headerHash) >= difficulty, "Block difficulty is not enough")
      //TODO check timestamp
      case m: BlockTransactions =>
        require(contains(m.headerId), s"Header for modifier $m is no defined")
        require(!contains(m.id), s"Modifier $m is already in history")
      case m: ADProofs =>
        require(contains(m.headerId), s"Header for modifier $m is no defined")
        require(!contains(m.id), s"Modifier $m is already in history")
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


    //TODO state should not be empty
    val stateRoot = Array.fill(32)(0.toByte)
    val genesis: ErgoFullBlock = {
      val header: Header = Header(0.toByte,
        Array.fill(32)(0.toByte),
        Seq(),
        Algos.EmptyMerkleTreeRoot,
        stateRoot: Array[Byte],
        Algos.EmptyMerkleTreeRoot,
        1500203225564L,
        0)
      val blockTransactions: BlockTransactions = BlockTransactions(header.id, Seq())
      val aDProofs: ADProofs = ADProofs(header.id, Array())
      ErgoFullBlock(header, blockTransactions, aDProofs)
    }
    val historyConfig: HistoryConfig = HistoryConfig(settings.poPoWBootstrap, settings.blocksToKeep, settings.minimalSuffix)

    val history = new ErgoHistory with EmptyADProofsProcessor {
      override val config: HistoryConfig = historyConfig
      override protected val storage: LSMStore = db
    }
    if (history.isEmpty) {
      history.append(genesis.header).get._1.append(genesis.aDProofs).get._1.append(genesis.blockTransactions).get._1
    } else {
      history
    }
  }
}


