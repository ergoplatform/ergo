package org.ergoplatform.nodeView.history.components.popow

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.components.{BasicReaders, HeadersComponent, Persistence}
import org.ergoplatform.settings.{Algos, PoPowParams}
import scorex.core.utils.ScorexEncoding
import scorex.util.{bytesToId, idToBytes}

import scala.util.{Failure, Try}

trait ProvingPoPowComponent extends EmptyPoPowComponent {
  self: HeadersComponent with BasicReaders with Persistence with ScorexEncoding =>

  val LastProofIdKey = ByteArrayWrapper(Algos.hash("last_proof"))

  override final def prove(params: PoPowParams): Try[PoPowProof] =
    bestHeaderOpt
      .fold[Try[PoPowProof]](Failure(new Exception("Empty chain"))) { bestHeader =>
        val chain = headerChainBack(Int.MaxValue, bestHeader, _.isGenesis).headers
        val poPowChain = chain.flatMap { h =>
          typedModifierById[Extension](h.extensionId)
            .flatMap(ext => PoPowAlgos.unpackInterlinks(ext.fields).toOption)
            .map(PoPowHeader(h, _))
        }
        Try(PoPowAlgos.prove(poPowChain)(params))
      }
      .map { proof =>
        storage.getIndex(LastProofIdKey)
          .flatMap(id => typedModifierById[PoPowProofPrefix](bytesToId(id)))
          .foreach(prefix => storage.remove(Seq(prefix.id)))
        storage.update(Seq(LastProofIdKey -> idToBytes(proof.id)), Seq(proof))
        proof
      }

}
