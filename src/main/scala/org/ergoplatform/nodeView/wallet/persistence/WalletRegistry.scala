package org.ergoplatform.nodeView.wallet.persistence

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.persistence.RegistryOpA.RegistryOp
import org.ergoplatform.settings.{ErgoSettings, WalletSettings}
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.VersionTag
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.Try

/**
  * Provides an access to version-sensitive wallet-specific indexes.
  * (Such as on-chain UTXO's or balances)
  */
final class WalletRegistry(store: Store)(ws: WalletSettings) extends ScorexLogging {

  import RegistryOps._
  import org.ergoplatform.nodeView.wallet.IdUtils._

  private val keepHistory = ws.keepSpentBoxes

  def readIndex: RegistryIndex =
    getIndex.transact(store)

  def readTransactions: Seq[ErgoTransaction] =
    getAllTxs.transact(store)

  def readCertainUnspentBoxes: Seq[TrackedBox] = {
    val query = for {
      allBoxes <- getAllBoxes
      index <- getIndex
    } yield {
      val uncertainIds = index.uncertainBoxes
      allBoxes.filterNot(b =>
        uncertainIds.contains(encodedBoxId(b.box.id)) || b.spendingHeightOpt.isDefined
      )
    }
    query.transact(store)
  }

  def readCertainBoxes: Seq[TrackedBox] = {
    val query = for {
      allBoxes <- getAllBoxes
      index <- getIndex
    } yield {
      val uncertainIds = index.uncertainBoxes
      allBoxes.filterNot(b => uncertainIds.contains(encodedBoxId(b.box.id)))
    }
    query.transact(store)
  }

  def readUncertainBoxes: Seq[TrackedBox] = {
    val query = for {
      index <- getIndex
      uncertainBoxes <- getBoxes(index.uncertainBoxes.map(decodedBoxId))
    } yield uncertainBoxes.flatten
    query.transact(store)
  }

  def readHistoricalBoxes: Seq[TrackedBox] = getAllBoxes
    .map(_.filter(_.spendingHeightOpt.isDefined))
    .transact(store)

  /**
    * Updates indexes according to a data extracted from a block and performs versioned update.
    */
  def updateOnBlock(certainBxs: Seq[TrackedBox], uncertainBxs: Seq[TrackedBox],
                    inputs: Seq[(ModifierId, EncodedBoxId)], txs: Seq[ErgoTransaction])
                   (blockId: ModifierId, blockHeight: Int): Unit = {
    val update = for {
      _ <- putBoxes(certainBxs ++ uncertainBxs)
      _ <- putTxs(txs)
      spentBoxesWithTx <- getAllBoxes.map(_.flatMap(bx =>
        inputs.find(_._2 == encodedBoxId(bx.box.id)).map { case (txId, _) => txId -> bx })
      )
      _ <- processHistoricalBoxes(spentBoxesWithTx, blockHeight)
      _ <- updateIndex { case RegistryIndex(_, balance, tokensBalance, _) =>
        val spentBoxes = spentBoxesWithTx.map(_._2)
        val spentAmt = spentBoxes.map(_.box.value).sum
        val spentTokensAmt = spentBoxes
          .flatMap(_.box.additionalTokens.toArray)
          .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
            acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
          }
        val receivedTokensAmt = certainBxs
          .flatMap(_.box.additionalTokens.toArray)
          .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
            acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
          }
        val decreasedTokensBalance = spentTokensAmt
          .foldLeft(tokensBalance) { case (acc, (encodedId, amt)) =>
            val decreasedAmt = acc.getOrElse(encodedId, 0L) - amt
            if (decreasedAmt > 0) acc.updated(encodedId, decreasedAmt) else acc - encodedId
          }
        val newTokensBalance = receivedTokensAmt
          .foldLeft(decreasedTokensBalance) { case (acc, (encodedId, amt)) =>
            acc.updated(encodedId, acc.getOrElse(encodedId, 0L) + amt)
          }
        val receivedAmt = certainBxs.map(_.box.value).sum
        val newBalance = balance - spentAmt + receivedAmt
        val uncertain = uncertainBxs.map(x => encodedBoxId(x.box.id))
        require(
          (newBalance >= 0 && newTokensBalance.forall(_._2 >= 0)) || ws.testMnemonic.isDefined,
          "Balance could not be negative")
        RegistryIndex(blockHeight, newBalance, newTokensBalance, uncertain)
      }
    } yield ()

    update.transact(store, idToBytes(blockId))
  }

  def rollback(version: VersionTag): Try[Unit] =
    Try(store.rollback(ByteArrayWrapper(Base16.decode(version).get)))

  /**
    * Transits used boxes to a spent state or simply deletes them depending on a settings.
    */
  private[persistence] def processHistoricalBoxes(spentBoxes: Seq[(ModifierId, TrackedBox)],
                                                  spendingHeight: Int): RegistryOp[Unit] = {
    if (keepHistory) {
      updateBoxes(spentBoxes.map(_._2.box.id)) { tb =>
        val spendingTxIdOpt = spentBoxes
          .find { case (_, x) => encodedBoxId(x.box.id) == encodedBoxId(tb.box.id) }
          .map(_._1)
        tb.copy(spendingHeightOpt = Some(spendingHeight), spendingTxIdOpt = spendingTxIdOpt)
      }
    } else {
      removeBoxes(spentBoxes.map(_._2.box.id))
    }
  }

}

object WalletRegistry {

  def readOrCreate(settings: ErgoSettings): WalletRegistry = {
    val dir = new File(s"${settings.directory}/wallet/registry")
    dir.mkdirs()
    new WalletRegistry(new LSMStore(dir))(settings.walletSettings)
  }

}
