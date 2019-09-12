package org.ergoplatform.nodeView.history.components.popow

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPowAlgos.maxLevelOf
import org.ergoplatform.modifiers.history.{Header, PoPowProof, PoPowProofPrefix, PoPowProofSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.components.{ChainSyncComponent, Configuration, HeadersComponent, Persistence}
import org.ergoplatform.nodeView.history.storage.StorageKeys._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{PoPowParams, PoPowSettings}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ModifierValidator
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.util.{Failure, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that accept them.
  */
trait PoPowBootstrapComponent extends PoPowComponent {
  self: HeadersComponent
    with ChainSyncComponent
    with Configuration
    with Persistence
    with ScorexLogging
    with ScorexEncoding =>

  private def proofsChecked: Int = storage.getIndex(ProofsCheckedKey)
    .map(Ints.fromByteArray)
    .getOrElse(0)

  private def poPowSettings: PoPowSettings = settings.nodeSettings.poPowSettings

  final def bestProofIdOpt: Option[ModifierId] = storage.getIndex(BestProofIdKey)
    .map(bytesToId)

  final def proofById(id: ModifierId): Option[PoPowProof] = storage.get(id)
    .flatMap(PoPowProofSerializer.parseBytesTry(_).toOption)

  final def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier] = {
    val proofsCheckedInc = proofsChecked + 1
    storage.updateIndex(ProofsCheckedKey, Ints.toByteArray(proofsCheckedInc))
    val isBest = bestProofIdOpt.flatMap(proofById).forall(p => m.prefix.isBetterThan(p.prefix))
    if (isBest) storage.update(Seq(BestProofIdKey -> idToBytes(m.id)), Seq(m))
    if (proofsCheckedInc >= poPowSettings.minProofsToCheck) {
      val bestProof = bestProofIdOpt.flatMap(proofById).getOrElse(m)
      settings.nodeSettings.stateType match {
        case StateType.Utxo => // request last headers to reach nearest snapshot height
          // todo: Implement when fast-sync is merged.
          ErgoHistory.emptyProgressInfo
        case StateType.Digest => // save proved chain and update best header indexes
          val bestHeader = bestProof.headersChain.last
          val indexesToInsert = bestProof.chain
            .foldLeft(Seq.empty[(ByteArrayWrapper, Array[Byte])]) { case (acc, h) =>
              acc ++ toInsert(h.header)._1
            }
          storage.update(indexesToInsert, bestProof.headersChain)
          ProgressInfo(None, Seq.empty, Seq(bestHeader), toDownload(bestHeader))
      }
    } else {
      ErgoHistory.emptyProgressInfo
    }
  }

  final def validate(m: PoPowProof): Try[Unit] =
    ModifierValidator.failFast
      .demand(
        m.suffix.chain.lengthCompare(m.suffix.k) == 0,
        s"Invalid suffix length, given: ${m.suffix.chain}, required: ${m.suffix.k}"
      )
      .demand(validPrefix(m.prefix), s"Invalid prefix length")
      .demand(
        m.prefix.chain.tail.forall(_.interlinks.headOption.contains(m.prefix.chain.head.id)),
        "Chain is not anchored"
      )
      .demand(
        m.prefix.chain.headOption.exists(_.header.requiredDifficulty == settings.chainSettings.initialDifficulty),
        "Wrong genesis difficulty"
      )
      .validateSeq(m.prefix.headersChain ++ m.suffix.headersChain.headOption)(
        (_, h) => HeadersValidator.Partial.validateOrphanedBlockHeader(h))
      .validateSeq(groupConsistentChain(m.suffix.headersChain)) { case (_, (h0, h1)) =>
        HeadersValidator.Partial.validateChildBlockHeader(h1, h0)
      }
      .result
      .toTry

  final def prove(params: PoPowParams): Try[PoPowProof] =
    Failure(new Exception("PoPow proving is not supported"))

  private def groupConsistentChain(chain: Seq[Header]): Seq[(Header, Header)] =
    chain.sliding(2, 1).foldLeft(Seq.empty[(Header, Header)]) { case (acc, hs) =>
      acc ++ hs.headOption.flatMap(h0 => hs.lastOption.map(h0 -> _))
    }

  private def validPrefix(prefix: PoPowProofPrefix): Boolean = {
    val maxLevel = prefix.headersChain.tail.map(maxLevelOf).max
    assert(maxLevel < 256)
    (0 to maxLevel).exists(l => prefix.headersChain.count(h => maxLevelOf(h) >= l) >= prefix.m)
  }

}
