package org.ergoplatform.mining

import cats.syntax.either._
import sigmastate.utils.Helpers._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.history.header.Header.Version
import org.ergoplatform.settings.Algos
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}
import sigma.crypto.{CryptoConstants, EcPointType}

/**
  * Solution for an Autolykos PoW puzzle.
  *
  * In Autolykos v.1 all the four fields are used, in Autolykos v.2 only pk and n fields are used.
  *
  * The miner public key `pk` and one-time public key `w` are exposed as elliptic-curve points but
  * backed internally by their canonical compressed bytes, decoded lazily on first access. This lets
  * the trusted local-database read path (see `HistoryStorage.modifierById`) rehydrate headers for
  * chain traversal, id computation or re-serialization without paying for EC point decompression,
  * which the profiler flagged as a hotspot. Untrusted (network) input is still decoded eagerly by
  * `AutolykosSolutionSerializer.parse`, which also re-canonicalizes the encoding, so malformed keys
  * are rejected and ids stay consistent with other nodes, exactly as before.
  *
  * Implementation note: this is a regular class rather than a `case class` so the points can be decoded
  * lazily from hidden byte fields. The point-based public surface (`apply`, `copy`, `unapply`, `equals`,
  * `hashCode`) is kept for source compatibility, but this is a binary/ABI-breaking change for code
  * compiled against the previous case-class form. Do not reintroduce the case class.
  *
  * @param pk - miner public key. Should be used to collect block rewards
  * @param w  - one-time public key. Prevents revealing of miners secret
  * @param n  - nonce (8 bytes)
  * @param d  - distance between pseudo-random number, corresponding to nonce `n` and a secret,
  *           corresponding to `pk`. The lower `d` is, the harder it was to find this solution.
  */
class AutolykosSolution private (private[mining] val pkBytes: Array[Byte],
                                 private[mining] val wBytes: Array[Byte],
                                 val n: Array[Byte],
                                 val d: BigInt) {

  lazy val pk: EcPointType = groupElemFromBytes(pkBytes)

  lazy val w: EcPointType = groupElemFromBytes(wBytes)

  def copy(pk: EcPointType = this.pk,
           w: EcPointType = this.w,
           n: Array[Byte] = this.n,
           d: BigInt = this.d): AutolykosSolution = AutolykosSolution(pk, w, n, d)

  override def equals(obj: Any): Boolean = obj match {
    case other: AutolykosSolution =>
      java.util.Arrays.equals(pkBytes, other.pkBytes) &&
        java.util.Arrays.equals(wBytes, other.wBytes) &&
        java.util.Arrays.equals(n, other.n) &&
        d == other.d
    case _ => false
  }

  override def hashCode(): Int = {
    var result = java.util.Arrays.hashCode(pkBytes)
    result = 31 * result + java.util.Arrays.hashCode(wBytes)
    result = 31 * result + java.util.Arrays.hashCode(n)
    result = 31 * result + d.hashCode()
    result
  }

  override def toString: String =
    s"AutolykosSolution(${Algos.encode(pkBytes)},${Algos.encode(wBytes)},${Algos.encode(n)},$d)"
}

object AutolykosSolution extends ApiCodecs {
  // "pk", "w" and "d" values for Autolykos v2 solution, where they not passed from outside
  val pkForV2: EcPointType = CryptoConstants.dlogGroup.identity
  val wForV2: EcPointType = CryptoConstants.dlogGroup.generator
  val dForV2: BigInt = 0

  // compressed bytes of `wForV2`, kept private and cloned per solution so the shared constant
  // cannot be mutated through a parsed solution
  private val wBytesForV2: Array[Byte] = groupElemToBytes(wForV2)

  private[mining] def wBytesForV2Copy: Array[Byte] = wBytesForV2.clone()

  /**
    * Build a solution from decoded EC points. The points are compressed to their canonical byte form
    * once, here, so serialization and id computation never have to compress them again.
    */
  def apply(pk: EcPointType,
            w: EcPointType,
            n: Array[Byte],
            d: BigInt): AutolykosSolution =
    new AutolykosSolution(groupElemToBytes(pk), groupElemToBytes(w), n, d)

  /**
    * Build a solution directly from compressed point bytes, deferring EC point decompression until
    * `pk`/`w` are accessed. Intended for trusted, already-validated data (the storage read path).
    */
  private[mining] def fromBytes(pkBytes: Array[Byte],
                                wBytes: Array[Byte],
                                n: Array[Byte],
                                d: BigInt): AutolykosSolution =
    new AutolykosSolution(pkBytes, wBytes, n, d)

  def unapply(s: AutolykosSolution): Option[(EcPointType, EcPointType, Array[Byte], BigInt)] =
    Some((s.pk, s.w, s.n, s.d))

  implicit val jsonEncoder: Encoder[AutolykosSolution] = Encoder.instance { s: AutolykosSolution =>
    Map(
      "pk" -> s.pk.asJson,
      "w" -> s.w.asJson,
      "n" -> Algos.encode(s.n).asJson,
      "d" -> s.d.asJson(bigIntEncoder)
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AutolykosSolution] = Decoder.instance { c: HCursor =>
    for {
      pkOpt <- c.downField("pk").as[Option[EcPointType]]
      wOpt <- c.downField("w").as[Option[EcPointType]]
      n <- c.downField("n").as[Array[Byte]]
      dOpt <- c.downField("d").as[Option[BigInt]]
    } yield {
      AutolykosSolution(pkOpt.getOrElse(pkForV2), wOpt.getOrElse(wForV2), n, dOpt.getOrElse(dForV2))
    }
  }

}


/**
  * Binary serializer for Autolykos v1 solution,
  * serializing and parsing "pk", "w", "nonce", and "d" values
  */
class AutolykosV1SolutionSerializer extends ErgoSerializer[AutolykosSolution] {

  override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
    val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    w.putBytes(obj.pkBytes)
    w.putBytes(obj.wBytes)
    require(obj.n.length == 8) // non-consensus check on prover side
    w.putBytes(obj.n)
    w.putUByte(dBytes.length)
    w.putBytes(dBytes)
  }

  // eager, validating parse: decode the points (rejecting malformed group elements) and rebuild from
  // them, so the serialized form is canonical - matching the original parser, which round-tripped
  // points through groupElemToBytes (e.g. a non-canonical infinity encoding normalizes to 33 zeroes,
  // which keeps `Header.serializedId` consistent with other nodes)
  override def parse(r: Reader): AutolykosSolution = {
    val raw = parseLazy(r)
    AutolykosSolution(raw.pk, raw.w, raw.n, raw.d)
  }

  // lazy read for the trusted storage path only - defers EC point decompression, no validation
  private[mining] def parseLazy(r: Reader): AutolykosSolution = {
    val pkBytes = r.getBytes(PublicKeyLength)
    val wBytes = r.getBytes(PublicKeyLength)
    val nonce = r.getBytes(8)
    val dBytesLength = r.getUByte()
    val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
    AutolykosSolution.fromBytes(pkBytes, wBytes, nonce, d)
  }

}

/**
  * Binary serializer for Autolykos v2 solution, serializing and parsing "pk" and "nonce" values
  */
class AutolykosV2SolutionSerializer extends ErgoSerializer[AutolykosSolution] {

  import AutolykosSolution.{dForV2, wBytesForV2Copy}

  override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
    w.putBytes(obj.pkBytes)
    require(obj.n.length == 8) // non-consensus check on prover side
    w.putBytes(obj.n)
  }

  // eager, validating parse: decode pk (rejecting malformed group elements) and rebuild from the
  // points so the serialized form is canonical, matching the original parser (see v1 serializer)
  override def parse(r: Reader): AutolykosSolution = {
    val raw = parseLazy(r)
    AutolykosSolution(raw.pk, raw.w, raw.n, raw.d)
  }

  // lazy read for the trusted storage path only - defers EC point decompression, no validation
  private[mining] def parseLazy(r: Reader): AutolykosSolution = {
    val pkBytes = r.getBytes(PublicKeyLength)
    val nonce = r.getBytes(8)
    AutolykosSolution.fromBytes(pkBytes, wBytesForV2Copy, nonce, dForV2)
  }

}


/**
  * Serializing facade for both Autolykos v1 and v2 solutions
  */
object AutolykosSolutionSerializer {
  private val v1Serializer = new AutolykosV1SolutionSerializer
  private val v2Serializer = new AutolykosV2SolutionSerializer

  private def serializer(blockVersion: Version): ErgoSerializer[AutolykosSolution] = {
    if (blockVersion == 1) {
      v1Serializer
    } else {
      v2Serializer
    }
  }

  def serialize(blockVersion: Version, solution: AutolykosSolution, w: Writer): Unit = {
    serializer(blockVersion).serialize(solution, w)
  }

  /**
    * Default, validating parse used for all untrusted (e.g. network) input. The group element(s) are
    * decoded immediately so malformed keys are rejected here, exactly as the original parser did.
    */
  def parse(r: Reader, blockVersion: Version): AutolykosSolution = {
    serializer(blockVersion).parse(r)
  }

  /**
    * Lazy parse for trusted, already-validated input only (the storage read path). EC point
    * decompression is deferred until `pk`/`w` are accessed. Must not be used for untrusted input.
    */
  private[ergoplatform] def parseLazy(r: Reader, blockVersion: Version): AutolykosSolution = {
    if (blockVersion == 1) v1Serializer.parseLazy(r) else v2Serializer.parseLazy(r)
  }

}
