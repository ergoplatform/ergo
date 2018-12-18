package org.ergoplatform.nodeView.history.storage.modifierprocessors.popow
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{Header, NiPoPowProof}
import scorex.core.consensus.History
import scorex.util.ModifierId

import scala.util.{Success, Try}

trait FullNiPoPowProofsProcessor extends NiPoPowProofsProcessor {

  protected val bestProofPrefixIdOpt: Option[ModifierId]

  def process(m: NiPoPowProof): History.ProgressInfo[ErgoPersistentModifier] = ???

  def validate(m: NiPoPowProof): Try[Unit] = m.validate.flatMap { _ =>
    val genesis = m.prefix.chain.head
    val suffix = m.suffix.chain
    suffix.tail
      .foldLeft[(Try[Unit], Header)](Success(), suffix.head) { case ((res, parent), header) =>
          res.flatMap(_ => HeaderValidator.validateChildBlockHeader(header, parent).toTry) -> parent
      }
      ._1
      .flatMap(_ => HeaderValidator.validateOrphanedBlockHeader(suffix.head).toTry)
      .flatMap(_ => HeaderValidator.validateGenesisBlockHeader(genesis).toTry)
  }

}
