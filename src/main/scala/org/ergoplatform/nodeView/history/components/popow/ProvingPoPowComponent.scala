package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.components.{BasicReaders, HeadersComponent, Persistence}
import org.ergoplatform.nodeView.history.storage.StorageKeys._
import org.ergoplatform.settings.PoPowParams
import scorex.core.utils.ScorexEncoding
import scorex.util.{bytesToId, idToBytes}

import scala.util.{Failure, Try}

trait ProvingPoPowComponent extends EmptyPoPowComponent {
  self: HeadersComponent with BasicReaders with Persistence with ScorexEncoding =>

  override final def prove(params: PoPowParams): Try[PoPowProof] =
    bestHeaderOpt
      .fold[Try[PoPowProof]](Failure(new Exception("Empty chain"))) { bestHeader =>
        val chain = headerChainBack(Int.MaxValue, bestHeader, _.isGenesis)
        val poPowChain = chain.flatMap(h => interlinksFor(h).map(PoPowHeader(h, _)))
        Try(PoPowAlgos.prove(poPowChain)(params))
      }
      .map { proof =>
        getLastProof.foreach(prefix => storage.remove(Seq(prefix.id)))
        storage.update(Seq(LastProofIdKey -> idToBytes(proof.id)), Seq(proof))
        proof
      }

  private[history] final def getLastProof = storage.getIndex(LastProofIdKey)
    .flatMap(id => typedModifierById[PoPowProof](bytesToId(id)))

  private def interlinksFor(header: Header) =
    storage.get(header.extensionId)
      .flatMap(x => ExtensionSerializer.parseBytesTry(x.tail).toOption)
      .flatMap(ext => PoPowAlgos.unpackInterlinks(ext.fields).toOption)

}
