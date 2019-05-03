package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.core.VersionTag
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.Try

/**
  * Provides an access to version-sensitive wallet-specific indexes.
  * (Such as UTXO's, balances)
  */
final class WalletRegistry(store: Store) extends ScorexLogging {

  import RegistryOps._
  import org.ergoplatform.nodeView.wallet.IdUtils._

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
      uncertainBoxes <- getBoxes(ADKey @@ index.uncertainBoxes.map(decodedId))
    } yield uncertainBoxes.flatten
    query.transact(store)
  }

  def updateOnBlock(certainBxs: Seq[TrackedBox], uncertainBxs: Seq[TrackedBox], inputs: Seq[EncodedBoxId])
                   (blockId: ModifierId, blockHeight: Int): Unit = {
    val update = for {
      _ <- putBoxes(certainBxs ++ uncertainBxs)
      spentBoxes <- getAllBoxes.map(_.filter(x => inputs.contains(encodedId(x.box.id))))
      _ <- updateIndex { case RegistryIndex(_, balance, tokensBalance, _) =>
        val spentAmt = spentBoxes.map(_.box.value).sum
        val spentTokensAmt = spentBoxes
          .flatMap(_.box.additionalTokens)
          .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
            acc.updated(encodedId(id), acc.getOrElse(encodedId(id), 0L) + amt)
          }
        val receivedTokensAmt = certainBxs
          .flatMap(_.box.additionalTokens)
          .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
            acc.updated(encodedId(id), acc.getOrElse(encodedId(id), 0L) + amt)
          }
        val decreasedTokensBalance = spentTokensAmt
          .foldLeft(tokensBalance) { case (acc, (encodedId, amt)) =>
            acc.updated(encodedId, acc.getOrElse(encodedId, 0L) - amt)
          }
        val newTokensBalance = receivedTokensAmt
          .foldLeft(decreasedTokensBalance) { case (acc, (encodedId, amt)) =>
            acc.updated(encodedId, acc.getOrElse(encodedId, 0L) + amt)
          }
        val receivedAmt = certainBxs.map(_.box.value).sum
        val newBalance = balance - spentAmt + receivedAmt
        val uncertain = uncertainBxs.map(x => encodedId(x.box.id))
        RegistryIndex(blockHeight, newBalance, newTokensBalance, uncertain)
      }
      _ <- removeBoxes(spentBoxes.map(_.box.id))
    } yield ()

    update.transact(store, idToBytes(blockId))
  }

  def rollback(version: VersionTag): Try[Unit] =
    Try(store.rollback(ByteArrayWrapper(Base16.decode(version).get)))

}
