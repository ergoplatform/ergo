package org.ergoplatform.nodeView.history.components.popow

import org.ergoplatform.modifiers.history._
import org.ergoplatform.nodeView.history.components.{BasicReaders, HeadersComponent, Persistence}
import org.ergoplatform.nodeView.history.storage.StorageKeys._
import org.ergoplatform.settings.PoPowParams
import scorex.core.utils.ScorexEncoding
import scorex.util.{bytesToId, idToBytes}

import scala.util.{Failure, Success, Try}

/**
  * Component which is able to produce NiPoPow proofs but not to process them.
  */
trait ProvingPoPowComponent extends EmptyPoPowComponent {
  self: HeadersComponent with BasicReaders with Persistence with ScorexEncoding =>

  override final def prove(params: PoPowParams): Try[PoPowProof] =
    getLastProof.fold(makeNewProof(params)(None)) { proof =>
      bestHeaderOpt.fold[Try[PoPowProof]](Failure(new Exception("Empty chain"))) { bestHeader =>
        val bestHeaderInProof = proof.chain.last.header
        if (bestHeaderInProof.id == bestHeader.id) Success(proof) else makeNewProof(params)(Some(bestHeader))
      }
    }

  /**
    * Generates new NiPoPow proof for the current chain and saves it to the storage,
    * or simply fetches it from the storage in case proof for the current chain was
    * created earlier.
    */
  private def makeNewProof(params: PoPowParams)
                          (prefetchedHeaderOpt: Option[Header]) =
    (prefetchedHeaderOpt orElse bestHeaderOpt)
      .fold[Try[PoPowProof]](Failure(new Exception("Empty chain"))) { bestHeader =>
        val chain = headerChainBack(Int.MaxValue, bestHeader, _.isGenesis)
        val poPowChain = chain.flatMap(h => getInterlinksFor(h).map(PoPowHeader(h, _)))
        Try(PoPowAlgos.prove(poPowChain)(params))
      }
      .map { proof =>
        getLastProof.foreach(proof => storage.remove(Seq(proof.id)))
        storage.update(Seq(LastProofIdKey -> idToBytes(proof.id)), Seq(proof))
        proof
      }

  private[history] final def getLastProof = storage.getIndex(LastProofIdKey)
    .flatMap(id => typedModifierById[PoPowProof](bytesToId(id)))

  private def getInterlinksFor(header: Header) =
    storage.get(header.extensionId)
      .flatMap(x => ExtensionSerializer.parseBytesTry(x.tail).toOption)
      .flatMap(ext => PoPowAlgos.unpackInterlinks(ext.fields).toOption)

}
