package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header, PoPoWProof}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewModifier._
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds, ProgressInfo}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

//TODO replace ErgoPersistentModifier to HistoryModifier
class ErgoHistory(storage: HistoryStorage,
                  indexStorage: IndexStorage,
                  config: HistoryConfig)
  extends History[AnyoneCanSpendProposition, AnyoneCanSpendTransaction, ErgoPersistentModifier, ErgoSyncInfo, ErgoHistory]
    with ScorexLogging {

  lazy val bestHeaderId: ModifierId = storage.bestHeaderId
  lazy val bestHeaderIdWithTransactions: ModifierId = ???

  override def isEmpty: Boolean = bestHeaderId sameElements config.GenesisId

  override def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = storage.modifierById(id)

  override def append(modifier: ErgoPersistentModifier): Try[(ErgoHistory, ProgressInfo[ErgoPersistentModifier])] = Try {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} to history")
    applicableTry(modifier).get
    modifier match {
      case m: Header =>
        assert(bestHeaderId sameElements storage.bestHeaderId, "History is inconsistent")
        storage.insert(m)
        if (bestHeaderId sameElements storage.bestHeaderId) {
          log.info(s"New orphaned header ${m.encodedId}")
          (new ErgoHistory(storage, indexStorage, config), ProgressInfo(None, Seq(), Seq()))
        } else {
          log.info(s"New best header ${m.encodedId}")
          //TODO Notify node view holder that it should download transactions ?
          (new ErgoHistory(storage, indexStorage, config), ProgressInfo(None, Seq(), Seq()))
        }
      case m: BlockTransactions =>
        storage.insert(m)

        ???
      case m: ADProofs =>
        storage.insert(m)

        ???
      case m: PoPoWProof =>
        storage.insert(m)

        ???
      case m =>
        throw new Error(s"Modifier $m have incorrect type")
    }
  }

  override def compare(other: ErgoSyncInfo): HistoryComparisonResult.Value = ???

  override def drop(modifierId: ModifierId): ErgoHistory = {
    storage.drop(modifierId)
    this
  }

  override def openSurfaceIds(): Seq[ModifierId] = ???

  override def applicable(modifier: ErgoPersistentModifier): Boolean = applicableTry(modifier).isSuccess

  def applicableTry(modifier: ErgoPersistentModifier): Try[Unit] = Try {
    modifier match {
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

  override def contains(pm: ErgoPersistentModifier): Boolean = ???

  override def contains(id: ModifierId): Boolean = ???

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = ???

  //TODO last full blocks and last headers
  override def syncInfo(answer: Boolean): ErgoSyncInfo = ???

  private def validate(modifier: ErgoPersistentModifier): Try[Unit] = ???


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
    ???
  }
}


