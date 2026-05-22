package org.ergoplatform.modifiers.history.header

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId

/**
  * Header without proof-of-work puzzle solution, see Header class description for details.
  */
class HeaderWithoutPow(val version: Header.Version, // 1 byte
                       val parentId: ModifierId, // 32 bytes
                       val ADProofsRoot: Digest32, // 32 bytes
                       val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                       val transactionsRoot: Digest32, // 32 bytes
                       val timestamp: Header.Timestamp,
                       val nBits: Long, //actually it is unsigned int
                       val height: Int,
                       val extensionRoot: Digest32,
                       val votes: Array[Byte], //3 bytes
                       val unparsedBytes: Array[Byte]) {
  def toHeader(powSolution: AutolykosSolution, headerSize: Option[Int] = None): Header =
    Header(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, powSolution, votes, unparsedBytes, headerSize)
}

object HeaderWithoutPow extends ApiCodecs {

  def apply(version: Header.Version, parentId: ModifierId, ADProofsRoot: Digest32, stateRoot: ADDigest,
            transactionsRoot: Digest32, timestamp: Header.Timestamp, nBits: Long, height: Int,
            extensionRoot: Digest32, votes: Array[Byte], unparsedBytes: Array[Byte]): HeaderWithoutPow = {
    new HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, votes, unparsedBytes)
  }

  implicit val jsonEncoder: Encoder[HeaderWithoutPow] = Encoder.instance { h: HeaderWithoutPow =>
    Map(
      "version" -> h.version.asJson,
      "parentId" -> Algos.encode(h.parentId).asJson,
      "adProofsRoot" -> Algos.encode(h.ADProofsRoot).asJson,
      "stateRoot" -> Algos.encode(h.stateRoot).asJson,
      "transactionsRoot" -> Algos.encode(h.transactionsRoot).asJson,
      "timestamp" -> h.timestamp.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "extensionRoot" -> Algos.encode(h.extensionRoot).asJson,
      "votes" -> Algos.encode(h.votes).asJson,
      "unparsedBytes" -> Algos.encode(h.unparsedBytes).asJson
    ).asJson
  }

}
