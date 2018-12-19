package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{Header, NiPoPowProof, NiPoPowProofPrefix, NiPoPowProofPrefixSerializer}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.consensus.History
import scorex.core.consensus.History.ProgressInfo
import scorex.util.ModifierId

import scala.util.{Success, Try}

trait FullNiPoPowProofsProcessor extends NiPoPowProofsProcessor {

  protected val bestProofPrefixIdOpt: Option[ModifierId]

  protected val BestProofPrefixIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(Constants.HashLength)(NiPoPowProofPrefix.TypeId))

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  private var proofsChecked: Int = 0

  def proofPrefixById(id: ModifierId): Option[NiPoPowProofPrefix] = historyStorage.get(id)
    .flatMap(NiPoPowProofPrefixSerializer.parseBytes(_).toOption)

  def process(m: NiPoPowProof): History.ProgressInfo[ErgoPersistentModifier] = {
    proofsChecked = proofsChecked + 1
    val isBest = bestProofPrefixIdOpt.flatMap(proofPrefixById).forall(m.prefix.isBetterThan)
    if (isBest && proofsChecked >= config.poPowSettings.minProofsToCheck) {
      ???
    } else if (isBest) {
      historyStorage.insert(Algos.idToBAW(m.id), Seq(BestProofPrefixIdKey -> Algos.idToBAW(m.id)), Seq(m))
      emptyProgressInfo
    } else {
      emptyProgressInfo
    }
  }

  def validate(m: NiPoPowProof): Try[Unit] = {
    m.validate.flatMap { _ =>
      val genesis = m.prefix.chain.head
      val suffix = m.suffix.chain
      suffix.tail
        .foldLeft[(Try[Unit], Header)](Success(()), suffix.head) { case ((res, parent), header) =>
          res.flatMap(_ => HeaderValidator.validateChildBlockHeader(header, parent).toTry) -> parent
        }
        ._1
        .flatMap(_ => HeaderValidator.validateOrphanedBlockHeader(suffix.head).toTry)
        .flatMap(_ => HeaderValidator.validateGenesisBlockHeader(genesis).toTry)
    }
  }

}
