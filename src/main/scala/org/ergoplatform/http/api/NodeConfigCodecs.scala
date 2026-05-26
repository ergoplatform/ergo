package org.ergoplatform.http.api

import io.circe._
import io.circe.generic.semiauto._
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettingsUpdate, VotingTargets}

import scala.concurrent.duration._
import scala.util.Try

/**
  * JSON codecs and DTOs for the `/node/config` API endpoint.
  *
  * Request:  [[NodeConfigPatch]] — all fields are optional; only fields that are
  *           present in the JSON body are applied. Type and range validation is
  *           performed in the decoders' `emap` calls so a single bad field fails
  *           the whole decode (all-or-nothing semantics).
  *
  * Response: [[NodeConfigView]] — the full mutable subset of `ErgoSettings`.
  */
object NodeConfigCodecs {

  // ---------- Request DTOs ----------

  final case class MempoolPatch(
    capacity: Option[Int] = None,
    minimalFeeAmount: Option[Long] = None,
    cleanupDuration: Option[FiniteDuration] = None
  )

  final case class VotingPatch(
    targets: Option[Map[Byte, Int]] = None,
    rulesToDisable: Option[Seq[Short]] = None
  )

  final case class NodeConfigPatch(
    voting: Option[VotingPatch] = None,
    mempool: Option[MempoolPatch] = None
  )

  // ---------- Response DTO ----------

  final case class MempoolView(
    capacity: Int,
    minimalFeeAmount: Long,
    cleanupDuration: FiniteDuration
  )

  final case class VotingView(
    targets: Map[Byte, Int],
    rulesToDisable: Seq[Short]
  )

  final case class NodeConfigView(
    voting: VotingView,
    mempool: MempoolView
  )

  object NodeConfigView {
    def from(s: ErgoSettings): NodeConfigView =
      NodeConfigView(
        voting = VotingView(
          targets = s.votingTargets.targets,
          rulesToDisable = s.votingTargets.desiredUpdate.rulesToDisable
        ),
        mempool = MempoolView(
          capacity = s.nodeSettings.mempoolCapacity,
          minimalFeeAmount = s.nodeSettings.minimalFeeAmount,
          cleanupDuration = s.nodeSettings.mempoolCleanupDuration
        )
      )
  }

  // ---------- Helper codecs (used implicitly by derivation) ----------

  /** Voting parameter ids are 0..255 expressed as quoted JSON object keys. */
  implicit val byteKeyDecoder: KeyDecoder[Byte] = (key: String) =>
    Try(key.toInt).toOption.filter(i => i >= 0 && i <= 255).map(i => (i & 0xFF).toByte)

  implicit val byteKeyEncoder: KeyEncoder[Byte] =
    (b: Byte) => (b.toInt & 0xFF).toString

  /** HOCON-style duration strings, e.g. "15s", "500ms", "2 minutes". */
  implicit val finiteDurationDecoder: Decoder[FiniteDuration] = Decoder.decodeString.emap { str =>
    Try(Duration(str)).toEither.left.map(_.getMessage).flatMap {
      case fd: FiniteDuration if fd.toMillis > 0 => Right(fd)
      case fd: FiniteDuration => Left(s"duration must be positive (got $fd)")
      case d => Left(s"expected finite duration, got $d")
    }
  }

  implicit val finiteDurationEncoder: Encoder[FiniteDuration] =
    Encoder.encodeString.contramap(fd => s"${fd.toMillis}ms")

  /** Rule ids are non-negative shorts. */
  implicit val rangedShortDecoder: Decoder[Short] = Decoder.decodeInt.emap { i =>
    if (i >= 0 && i <= Short.MaxValue) Right(i.toShort)
    else Left(s"value $i out of range 0..${Short.MaxValue}")
  }

  // ---------- Patch decoders ----------

  implicit val mempoolPatchDecoder: Decoder[MempoolPatch] = deriveDecoder[MempoolPatch].emap { p =>
    if (p.capacity.exists(_ <= 0)) Left("mempool.capacity must be > 0")
    else if (p.minimalFeeAmount.exists(_ < 0)) Left("mempool.minimalFeeAmount must be >= 0")
    else Right(p)
  }

  implicit val votingPatchDecoder: Decoder[VotingPatch] = deriveDecoder[VotingPatch]
  implicit val nodeConfigPatchDecoder: Decoder[NodeConfigPatch] = deriveDecoder[NodeConfigPatch]

  // ---------- View encoders ----------

  implicit val mempoolViewEncoder: Encoder[MempoolView] = deriveEncoder[MempoolView]
  implicit val votingViewEncoder: Encoder[VotingView] = deriveEncoder[VotingView]
  implicit val nodeConfigViewEncoder: Encoder[NodeConfigView] = deriveEncoder[NodeConfigView]

  // ---------- Merge logic ----------

  /** Returns `current` with any fields present in `patch` overlaid. Validation
    * already happened in the decoder, so this is pure structural merging. */
  def applyPatch(current: ErgoSettings, patch: NodeConfigPatch): ErgoSettings = {
    val withVoting = patch.voting.fold(current) { vp =>
      val newTargets = vp.targets.getOrElse(current.votingTargets.targets)
      val newRules = vp.rulesToDisable.getOrElse(current.votingTargets.desiredUpdate.rulesToDisable)
      current.copy(votingTargets = VotingTargets(
        targets = newTargets,
        desiredUpdate = ErgoValidationSettingsUpdate(newRules, Seq())
      ))
    }
    patch.mempool.fold(withVoting) { mp =>
      val ns = withVoting.nodeSettings.copy(
        mempoolCapacity = mp.capacity.getOrElse(withVoting.nodeSettings.mempoolCapacity),
        minimalFeeAmount = mp.minimalFeeAmount.getOrElse(withVoting.nodeSettings.minimalFeeAmount),
        mempoolCleanupDuration = mp.cleanupDuration.getOrElse(withVoting.nodeSettings.mempoolCleanupDuration)
      )
      withVoting.copy(nodeSettings = ns)
    }
  }
}
