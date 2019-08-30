package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.PoPowAlgos.maxLevelOf
import org.ergoplatform.modifiers.history.{Header, NiPoPowProofSerializer, PoPowProof, PoPowProofPrefix}
import org.ergoplatform.nodeView.history.storage.modifierprocessors.HeadersProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Constants, ErgoSettings, PoPowSettings}
import scorex.core.consensus.History.ProgressInfo
import scorex.core.validation.ModifierValidator
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Success, Try}

/**
  * Contains all functions required by History to process PoPoWProofs for regime that accept them.
  */
trait FullPoPoWProofsProcessor extends PoPoWProofsProcessor {
  self: HeadersProcessor with ScorexLogging =>

  protected val settings: ErgoSettings

  protected val bestProofPrefixIdOpt: Option[ModifierId] = ???

  protected val BestProofPrefixIdKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(Constants.HashLength)(PoPowProofPrefix.TypeId))

  private val emptyProgressInfo = ProgressInfo[ErgoPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)

  private var proofsChecked: Int = 0

  def poPowSettings: PoPowSettings = settings.nodeSettings.poPowSettings

  def proofById(id: ModifierId): Option[PoPowProof] = historyStorage.get(id)
    .flatMap(NiPoPowProofSerializer.parseBytesTry(_).toOption)

  def process(m: PoPowProof): ProgressInfo[ErgoPersistentModifier] = {
    proofsChecked = proofsChecked + 1
    val isBest = bestProofPrefixIdOpt.flatMap(proofById).forall(p => m.prefix.isBetterThan(p.prefix))
    if (proofsChecked >= poPowSettings.minProofsToCheck) {
      val bestProof = bestProofPrefixIdOpt.flatMap(proofById).getOrElse(m)
      settings.nodeSettings.stateType match {
        case StateType.Utxo => // request last headers to reach nearest snapshot height
          ???
        case StateType.Digest => // save proved chain and update best header indexes
          val bestHeader = bestProof.chain.last
          val indexesToInsert = bestProof.chain
            .foldLeft(Seq.empty[(ByteArrayWrapper, Array[Byte])]) { case (acc, h) =>
              acc ++ toInsert(h)._1
            }
          historyStorage.insert(indexesToInsert, bestProof.chain)
          ProgressInfo(None, Seq.empty, Seq(bestHeader), toDownload(bestHeader))
      }
    } else if (isBest) {
      historyStorage.insert(Seq(BestProofPrefixIdKey -> idToBytes(m.id)), Seq(m))
      emptyProgressInfo
    } else {
      emptyProgressInfo
    }
  }

  def validate(m: PoPowProof): Try[Unit] =
    ModifierValidator.failFast
      .demand(m.suffix.chain.lengthCompare(m.suffix.k) == 0, "Invalid suffix length")
      .demand(validPrefix(m.prefix), s"Invalid prefix length")
      //.demand(m.prefix.chain.tail.forall(_.interlinks.headOption.contains(chain.head.id)), "Chain is not anchored")
      //.validateHeaders
      .result
      .toTry

  private def validPrefix(prefix: PoPowProofPrefix): Boolean = {
    val levels = prefix.chain.tail.map(maxLevelOf)
    (0 to levels.max).forall(l => prefix.chain.count(h => maxLevelOf(h) >= l) >= prefix.m) // todo: check max qty overflow as well.
  }

}

