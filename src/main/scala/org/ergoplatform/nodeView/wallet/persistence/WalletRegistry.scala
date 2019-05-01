package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.VersionTag
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.Try

/**
  * Provides an access to version-sensitive wallet-specific indexes.
  * (Such as UTXO's, balances)
  */
final class WalletRegistry(store: Store) extends ScorexLogging {

  import RegistryOps._

  def readIndex: RegistryIndex =
    getIndex.transact(store)

  def readCertainBoxes: Seq[TrackedBox] = {
    val query = for {
      allBoxes <- getAllBoxes
      index <- getIndex
    } yield {
      val uncertainIds = index.uncertainBoxes
      allBoxes.filterNot(b => uncertainIds.contains(b.box.id))
    }
    query.transact(store)
  }

  def readUncertainBoxes: Seq[TrackedBox] = {
    val query = for {
      index <- getIndex
      uncertainBoxes <- getBoxes(index.uncertainBoxes)
    } yield uncertainBoxes.flatten
    query.transact(store)
  }

  def updateOnBlock(certainBxs: Seq[TrackedBox], uncertainBxs: Seq[TrackedBox], inputs: Seq[BoxId])
                   (blockId: ModifierId, blockHeight: Int): Unit = {
    val update = for {
      _ <- putBoxes(certainBxs ++ uncertainBxs)
      spentBoxes <- getAllBoxes.map(_.filter(x => inputs.contains(x.box.id)))
      _ <- updateIndex { case RegistryIndex(_, balance, tokensBalance, _) =>
        val spentAmt = spentBoxes.map(_.box.value).sum
        val spentTokensAmt = spentBoxes
          .flatMap(_.box.additionalTokens)
          .foldLeft(Map.empty[ByteArrayWrapper, Long]) { case (acc, (id, amt)) =>
            acc.updated(ByteArrayWrapper(id), acc.getOrElse(ByteArrayWrapper(id), 0L) + amt)
          }
        val tokensBalanceMap = tokensBalance.map(x => ByteArrayWrapper(x._1) -> x._2).toMap
        val newTokensBalance = spentTokensAmt.foldLeft(Seq.empty[(TokenId, Long)]) {
          case (acc, (wrappedId, amt)) =>
            val newAmt = tokensBalanceMap.getOrElse(wrappedId, 0L) - amt
            acc :+ (Digest32 @@ wrappedId.data -> newAmt)
        }
        val receivedAmt = certainBxs.map(_.box.value).sum
        val newBalance = balance - spentAmt + receivedAmt
        RegistryIndex(blockHeight, newBalance, newTokensBalance, uncertainBxs.map(_.box.id))
      }
      _ <- removeBoxes(spentBoxes.map(_.box.id))
    } yield ()

    update.transact(store, idToBytes(blockId))
  }

  def rollback(version: VersionTag): Try[Unit] =
    Try(store.rollback(ByteArrayWrapper(Base16.decode(version).get)))

}
