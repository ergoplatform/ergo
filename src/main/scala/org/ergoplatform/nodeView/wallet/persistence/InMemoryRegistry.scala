package org.ergoplatform.nodeView.wallet.persistence

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.wallet.boxes.TrackedBox
import scorex.crypto.hash.Digest32

/**
  * Holds version-agnostic indexes (such as off-chain boxes) in runtime memory.
  */
final case class InMemoryRegistry(height: Int,
                                  certainBoxes: Seq[TrackedBox],
                                  uncertainBoxes: Seq[TrackedBox]) {

  val readIndex: RegistryIndex = {
    val balance = certainBoxes.map(_.box.value).sum
    val tokensBalance = certainBoxes
      .flatMap(_.box.additionalTokens)
      .foldLeft(Map.empty[ByteArrayWrapper, Long]) { case (acc, (id, amt)) =>
        acc.updated(ByteArrayWrapper(id), acc.getOrElse(ByteArrayWrapper(id), 0L) + amt)
      }
      .map { case (wrappedId, amt) => Digest32 @@ wrappedId.data -> amt }
      .toSeq
    RegistryIndex(height, balance, tokensBalance, uncertainBoxes.map(_.box.id))
  }

  def updated(certain: Seq[TrackedBox],
              uncertain: Seq[TrackedBox],
              spent: Seq[BoxId]): InMemoryRegistry =
    this.copy(certainBoxes = certainBoxes.filterNot(x => spent.contains(x.box.id)) ++ certain,
              uncertainBoxes = uncertainBoxes.filterNot(x => spent.contains(x.box.id)) ++ uncertain)

}

object InMemoryRegistry {

  def empty(height: Int): InMemoryRegistry =
    InMemoryRegistry(height, Seq.empty, Seq.empty)

}
