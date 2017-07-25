package org.ergoplatform.nodeView.history

import java.io.File

import io.iohk.iodb.LSMStore
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.storage._
import org.ergoplatform.nodeView.history.storage.modifierprocessors._
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.{Failure, Try}

//TODO replace ErgoPersistentModifier to HistoryModifier
trait ErgoHistory
  extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
    with HeadersProcessor
    with ADProofsProcessor
    with BlockTransactionsProcessor
    with ScorexLogging {

  protected val config: HistoryConfig
  protected val storage: LSMStore
  //TODO what should be the limit?
  val MaxRollback = 10000

  lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)

  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty


  //It is safe to call this function right after history initialization with genesis block
  def bestHeader: Header = bestHeaderOpt.get

  //None for light mode, Some for fullnode regime after initial bootstrap
  def bestFullBlockOpt: Option[ErgoFullBlock] = Try {
    getFullBlock(typedModifierById[Header](bestFullBlockId.get).get)
  }.toOption

  protected def getFullBlock(header: Header): ErgoFullBlock = {
    val aDProofs = typedModifierById[ADProof](header.ADProofsId)
    val txs = typedModifierById[BlockTransactions](header.transactionsId).get
    ErgoFullBlock(header, txs, aDProofs)
  }

  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = historyStorage.modifierById(id)

  def typedModifierById[T <: ErgoPersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T) => Some(m)
    case _ => None
  }

  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = Try {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} to history")
    applicableTry(modifier).get
    modifier match {
      case m: Header =>
        val dataToInsert = toInsert(m)
        historyStorage.insert(m.id, dataToInsert)
        if (isEmpty || (bestHeaderIdOpt.get sameElements m.id)) {
          log.info(s"New best header ${m.encodedId}")
          //TODO Notify node view holder that it should download transactions ?
          (this, ProgressInfo(None, Seq(), Seq()))
        } else {
          log.info(s"New orphaned header ${m.encodedId}, best: ${}")
          (this, ProgressInfo(None, Seq(), Seq()))
        }
      case m: BlockTransactions =>
        (this, process(m))
      case m: ADProof =>
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

  override def openSurfaceIds(): Seq[ModifierId] = bestFullBlockId.orElse(bestHeaderIdOpt).toSeq

  override def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = {
    modifier match {
      case m: Header =>
        validate(m)
      case m: BlockTransactions =>
        validate(m)
      case m: ADProof =>
        validate(m)
      case m: PoPoWProof =>
        Failure(new NotImplementedError)
      case m =>
        Failure(new Error(s"Modifier $m have incorrect type"))
    }
  }

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  //TODO last full blocks and last headers
  override def syncInfo(answer: Boolean): ErgoSyncInfo = ???

  def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain) = {
    assert(contains(header1))
    assert(contains(header2))
    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack)
      if (r._1.head == r._2.head) {
        r
      } else if (numberBack < MaxRollback) {
        val biggerOther = headerChainBack(numberBack, otherChain.head, (h: Header) => h.isGenesis) ++ otherChain.tail
        loop(biggerOther.size, biggerOther)
      } else {
        throw new Error(s"Common point not found for headers $header1 and $header2")
      }
    }
    loop(2, HeaderChain(Seq(header2)))
  }

  def commonBlockThenSuffixes(otherChain: HeaderChain,
                              startHeader: Header,
                              limit: Int = MaxRollback): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)
    val ourChain = headerChainBack(limit, startHeader, until)
    val commonBlock = ourChain.head
    val commonBlockThenSuffixes = otherChain.takeAfter(commonBlock)
    (ourChain, commonBlockThenSuffixes)
  }

  private def headerChainBack(count: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(remain: Int, block: Header, acc: Seq[Header]): Seq[Header] = {
      if (until(block) || remain == 0) {
        acc
      } else {
        modifierById(block.parentId) match {
          case Some(parent: Header) =>
            loop(remain - 1, parent, acc :+ parent)
          case _ =>
            log.warn(s"No parent header in history for block $block")
            acc
        }
      }
    }

    if (isEmpty) HeaderChain(Seq())
    else HeaderChain(loop(count, startHeader, Seq(startHeader)).reverse)
  }

  override type NVCT = ErgoHistory

}

object ErgoHistory extends ScorexLogging {

  def readOrGenerate(settings: ErgoSettings): ErgoHistory = {
    val dataDir = settings.dataDir
    val iFile = new File(s"$dataDir/history")
    iFile.mkdirs()
    val db = new LSMStore(iFile, maxJournalEntryCount = 10000)

    val historyConfig = HistoryConfig(settings.poPoWBootstrap, settings.blocksToKeep, settings.minimalSuffix)

    val history: ErgoHistory = if (!settings.verifyTransactions) {
      new ErgoHistory with EmptyADProofsProcessor with EmptyBlockTransactionsProcessor {
        override protected val config: HistoryConfig = historyConfig
        override protected val storage: LSMStore = db
      }
    } else if (settings.ADState) {
      new ErgoHistory with FullnodeADProofsProcessor with FullnodeBlockTransactionsProcessor {
        override protected val config: HistoryConfig = historyConfig
        override protected val storage: LSMStore = db
      }
    } else
      new ErgoHistory with EmptyADProofsProcessor with FullnodeBlockTransactionsProcessor {
        override protected val config: HistoryConfig = historyConfig
        override protected val storage: LSMStore = db
      }


    if (history.isEmpty) {
      log.info("Initialize empty history with genesis block")
      //todo: real definition of a genesis block, do we need genesis block at all?
      val genesis: ErgoFullBlock = {
        val genesisTimestamp = 1500203225564L
        val initialState = ErgoState.initialState
        //TODO        val stateRoot = initialState.rootHash()
        val stateRoot = Algos.hash("Initial state")
        val genesisTx = new AnyoneCanSpendTransaction(
          IndexedSeq(new AnyoneCanSpendProposition -> 0L),
          IndexedSeq((new AnyoneCanSpendProposition, 0L)),
          genesisTimestamp)
        val proofs = initialState.proofsForTransactions(Seq(genesisTx))
        val proofsRoot = ADProof.proofDigest(proofs)

        val header: Header = Header(0.toByte,
          Array.fill(hashLength)(0.toByte),
          Seq(),
          proofsRoot,
          stateRoot: Array[Byte],
          BlockTransactions.rootHash(Seq(genesisTx.id)),
          genesisTimestamp,
          0)
        val blockTransactions: BlockTransactions = BlockTransactions(header.id, Seq(genesisTx))
        val aDProofs: ADProof = ADProof(header.id, proofs)
        assert(header.ADProofsRoot sameElements aDProofs.digest)
        assert(header.transactionsRoot sameElements blockTransactions.digest)
        val aDProofsOpt: Option[ADProof] = if (settings.ADState) Some(aDProofs) else None
        ErgoFullBlock(header, blockTransactions, aDProofsOpt)
      }

      val historyWithHeader = history.append(genesis.header).get._1
      if (settings.verifyTransactions) {
        val historyWithBlocks = historyWithHeader.append(genesis.blockTransactions).get._1
        genesis.aDProofs.map(p => historyWithBlocks.append(p).get._1).getOrElse(historyWithBlocks)
      }
      else historyWithHeader
    } else {
      log.info("Initialize non-empty history ")
      history
    }
  }

}


