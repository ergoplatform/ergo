package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.consensus.History.ProgressInfo
import scorex.util.ModifierId

import scala.util.{Success, Try}

trait FullNiPoPowProofsProcessor extends NiPoPowProofsProcessor {

  protected val bestProofPrefixIdOpt: Option[ModifierId]

  protected val BestProofPrefixIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(Constants.HashLength)(NiPoPowProofPrefix.TypeId))

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  private var proofsChecked: Int = 0

  def proofById(id: ModifierId): Option[NiPoPowProof] = historyStorage.get(id)
    .flatMap(NiPoPowProofSerializer.parseBytes(_).toOption)

  def process(m: NiPoPowProof): ProgressInfo[ErgoPersistentModifier] = {
    proofsChecked = proofsChecked + 1
    val isBest = bestProofPrefixIdOpt.flatMap(proofById).forall(p => m.prefix.isBetterThan(p.prefix))
    if (proofsChecked >= config.poPowSettings.minProofsToCheck) {
      val bestProof = bestProofPrefixIdOpt.flatMap(proofById).getOrElse(m)
      config.stateType match {
        case StateType.Utxo =>
          ???
        case StateType.Digest =>
          val bestHeader = bestProof.chain.last
          val indexesToInsert = bestProof.chain
            .foldLeft(Seq.empty[(ByteArrayWrapper, ByteArrayWrapper)]) { case (acc, h) =>
              acc ++ toInsert(h)._1
            }
          historyStorage.insert(Algos.idToBAW(bestHeader.id), indexesToInsert, bestProof.chain)
          ProgressInfo(None, Seq.empty, Seq(bestHeader), toDownload(bestHeader))
      }
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
