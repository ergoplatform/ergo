package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.wallet.boxes.TrackedBox

/**
  * Holds version-agnostic indexes (such as off-chain boxes) in runtime memory.
  */
final case class InMemoryRegistry(height: Int,
                                  certainBoxes: Seq[TrackedBox],
                                  uncertainBoxes: Seq[TrackedBox]) {

  import org.ergoplatform.nodeView.wallet.IdUtils._

  val readIndex: RegistryIndex = {
    val balance = certainBoxes.map(_.box.value).sum
    val tokensBalance = certainBoxes
      .flatMap(_.box.additionalTokens)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc.updated(encodedId(id), acc.getOrElse(encodedId(id), 0L) + amt)
      }
    RegistryIndex(height, balance, tokensBalance, uncertainBoxes.map(x => encodedId(x.box.id)))
  }

  def updated(certain: Seq[TrackedBox],
              uncertain: Seq[TrackedBox],
              spent: Seq[EncodedBoxId]): InMemoryRegistry = {
    val unspentCertain = certainBoxes.filterNot(x => spent.contains(encodedId(x.box.id))) ++ certain
    val unspentUncertain = uncertainBoxes.filterNot(x => spent.contains(encodedId(x.box.id))) ++ uncertain
    this.copy(certainBoxes = unspentCertain.distinct, uncertainBoxes = unspentUncertain.distinct)
  }

}

object InMemoryRegistry {

  def empty(height: Int): InMemoryRegistry =
    InMemoryRegistry(height, Seq.empty, Seq.empty)

}
