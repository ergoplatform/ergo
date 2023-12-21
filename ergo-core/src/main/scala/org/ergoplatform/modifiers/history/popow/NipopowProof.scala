package org.ergoplatform.modifiers.history.popow

import io.circe.{Decoder, Encoder}
import org.ergoplatform.mining.difficulty.DifficultyAdjustment
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions.LongOps

/**
  * A structure representing NiPoPow proof as a persistent modifier.
  *
  * For details, see the foundational paper:
  *
  * [KMZ17] Non-Interactive Proofs of Proof-of-Work https://eprint.iacr.org/2017/963.pdf
  *
  * @param m          - security parameter (min μ-level superchain length)
  * @param k          - security parameter (min suffix length, >= 1)
  * @param prefix     - proof prefix headers
  * @param suffixHead - first header of the suffix
  * @param suffixTail - tail of the proof suffix headers
  * @param continuous - if the proof in continuous mode, see `org.ergoplatform.modifiers.history.popow.PoPowParams`
  *                     scaladoc for details
  */
case class NipopowProof(popowAlgos: NipopowAlgos,
                        m: Int,
                        k: Int,
                        prefix: Seq[PoPowHeader],
                        suffixHead: PoPowHeader,
                        suffixTail: Seq[Header],
                        continuous: Boolean) extends ScorexLogging {

  lazy val serializer: ErgoSerializer[NipopowProof] = new NipopowProofSerializer(popowAlgos)

  lazy val headersChain: Seq[Header] = prefixHeaders ++ suffixHeaders

  lazy val prefixHeaders: Seq[Header] = prefix.map(_.header)

  lazy val suffixHeaders: Seq[Header] = suffixHead.header +: suffixTail

  def chainOfLevel(l: Int): Seq[PoPowHeader] = prefix.filter(x => popowAlgos.maxLevelOf(x.header) >= l)

  /**
    * Implementation of the ≥ algorithm from [KMZ17], see Algorithm 4
    *
    * @param that - PoPoW proof to compare with
    * @return whether this PoPoW proof is better than "that"
    */
  def isBetterThan(that: NipopowProof): Boolean = {
    try {
      if (this.isValid && that.isValid) {
        popowAlgos.lowestCommonAncestor(headersChain, that.headersChain)
          .map(h => headersChain.filter(_.height > h.height) -> that.headersChain.filter(_.height > h.height))
          .exists({ case (thisDivergingChain, thatDivergingChain) =>
            popowAlgos.bestArg(thisDivergingChain)(m) > popowAlgos.bestArg(thatDivergingChain)(m)
          })
      } else {
        this.isValid
      }
    } catch {
      case t: Throwable =>
        log.error(s"Nipopow proofs comparison (isBetter) failed due to ${t.getMessage}: ", t)
        false
    }
  }

  /**
    * Checks if the proof is valid: if the heights are consistent and the connections are valid.
    * @return true if the proof is valid
    */
  lazy val isValid: Boolean = {
    this.hasValidConnections && this.hasValidHeights && this.hasValidProofs && this.hasValidDifficultyHeaders
  }

  /**
    * @return true if proof contains headers needed to check difficulty after the suffix,
    *         or if the proof is for non-continuous mode, false otherwise
    */
  lazy val hasValidDifficultyHeaders: Boolean = {
    if (continuous) {
      // check that headers needed to check difficulty are in the proof
      val chainSettings = popowAlgos.chainSettings
      val epochLength = chainSettings.eip37EpochLength.getOrElse(chainSettings.epochLength)
      val diffAdjustment = new DifficultyAdjustment(chainSettings)
      var lastIndex = 0
      diffAdjustment.heightsForNextRecalculation(suffixHead.height, epochLength).forall { height =>
        if (height > 0 && height < suffixHead.height) {
          lastIndex = headersChain.indexWhere(_.height == height, lastIndex)
          if (lastIndex == -1) {
            false
          } else {
            true
          }
        } else {
          true
        }
      }
    } else {
      // if the proof is for non-continuous mode, not checking difficulty headers membership in the proof
      true
    }
  }

  /**
    * Checks if the heights of the header-chain provided are consistent, meaning that for any two blocks b1 and b2,
    * if b1 precedes b2 then b1's height should be smaller.
    *
    * @return true if the heights of the header-chain are consistent
    */
  lazy val hasValidHeights: Boolean = {
    headersChain.zip(headersChain.tail).forall({
      case (prev, next) => prev.height < next.height
    })
  }

  /**
    * Checks the connections of the blocks in the proof.
    *
    * Adjacent blocks in the suffix should be linked either via interlink or parent block id.
    *
    * The same is true for prefix as well, with an exeption for difficulty headers (which are skipped during checks)
    *
    * @return true if all adjacent blocks are correctly connected
    */
  lazy val hasValidConnections: Boolean = {
    val maxDiffHeaders = popowAlgos.chainSettings.useLastEpochs + 1

    val prefixToCheck = prefix :+ suffixHead

    val prefixConnections = (1 until prefixToCheck.length).forall { checkIdx =>
      val next = prefixToCheck(checkIdx)
      (checkIdx - 1).to(Math.max(0, checkIdx - maxDiffHeaders - 1 - 1), -1).exists { prevIdx =>
        val prev = prefixToCheck(prevIdx)
        // Note that blocks with level 0 do not appear at all within interlinks, which is why we need to check the parent
        // block id as well.
        next.interlinks.contains(prev.id) || next.header.parentId == prev.id
      }
    }

    val suffixConnections = (suffixHead.header +: suffixTail).zip(suffixTail).forall({
      case (prev, next) => next.parentId == prev.id
    })

    prefixConnections && suffixConnections
  }

  /**
   * Checks the interlink proofs of the blocks in the proof.
   */
  lazy val hasValidProofs: Boolean = {
    prefix.forall(_.checkInterlinksProof()) &&
      suffixHead.checkInterlinksProof()
  }

}

object NipopowProof {
  import io.circe.syntax._
  import PoPowHeader._

  implicit val nipopowProofEncoder: Encoder[NipopowProof] = { proof: NipopowProof =>
    Map(
      "m" -> proof.m.asJson,
      "k" -> proof.k.asJson,
      "prefix" -> proof.prefix.asJson,
      "suffixHead" -> proof.suffixHead.asJson,
      "suffixTail" -> proof.suffixTail.asJson,
      "continuous" -> proof.continuous.asJson
    ).asJson
  }

  def nipopowProofDecoder(poPowAlgos: NipopowAlgos): Decoder[NipopowProof] = { c =>
    for {
      m <- c.downField("m").as[Int]
      k <- c.downField("k").as[Int]
      prefix <- c.downField("prefix").as[Seq[PoPowHeader]]
      suffixHead <- c.downField("suffixHead").as[PoPowHeader]
      suffixTail <- c.downField("suffixTail").as[Seq[Header]]
      continuous <- c.downField("continuous").as[Boolean]
    } yield NipopowProof(poPowAlgos, m, k, prefix, suffixHead, suffixTail, continuous)
  }

}

class NipopowProofSerializer(poPowAlgos: NipopowAlgos) extends ErgoSerializer[NipopowProof] {

  override def serialize(obj: NipopowProof, w: Writer): Unit = {
    w.putUInt(obj.m.toLong)
    w.putUInt(obj.k.toLong)
    w.putUInt(obj.prefix.size.toLong)
    obj.prefix.foreach { h =>
      val hBytes = h.bytes
      w.putUInt(hBytes.length.toLong)
      w.putBytes(hBytes)
    }
    val suffixHeadBytes = obj.suffixHead.bytes
    w.putUInt(suffixHeadBytes.length.toLong)
    w.putBytes(suffixHeadBytes)
    w.putUInt(obj.suffixTail.size.toLong)
    obj.suffixTail.foreach { h =>
      val hBytes = h.bytes
      w.putUInt(hBytes.length.toLong)
      w.putBytes(hBytes)
    }
    w.put(if (obj.continuous) 1 else 0)
  }

  override def parse(r: Reader): NipopowProof = {
    val m = r.getUInt().toIntExact
    val k = r.getUInt().toIntExact
    val prefixSize = r.getUInt().toIntExact
    val prefix = (0 until prefixSize).map { _ =>
      val size = r.getUInt().toIntExact
      PoPowHeaderSerializer.parseBytes(r.getBytes(size))
    }
    val suffixHeadSize = r.getUInt().toIntExact
    val suffixHead = PoPowHeaderSerializer.parseBytes(r.getBytes(suffixHeadSize))
    val suffixSize = r.getUInt().toIntExact
    val suffixTail = (0 until suffixSize).map { _ =>
      val size = r.getUInt().toIntExact
      HeaderSerializer.parseBytes(r.getBytes(size))
    }
    val continuous = if (r.getByte() == 1) true else false
    NipopowProof(poPowAlgos, m, k, prefix, suffixHead, suffixTail, continuous)
  }

}
