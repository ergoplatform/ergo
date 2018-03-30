package org.ergoplatform.mining

import com.google.common.primitives.Chars
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.ergoplatform.crypto.Equihash
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.Constants
import scorex.core.block.Block.Timestamp
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.annotation.tailrec
import scala.math.BigInt

class EquihashPowScheme(n: Char, k: Char) extends PowScheme with ScorexLogging {
  lazy val ergoPerson: Array[Byte] = "ERGOPoWT1234".getBytes("UTF-8") ++
    Chars.toByteArray(n) ++
    Chars.toByteArray(k)

  @SuppressWarnings(Array("NullParameter"))
  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot: ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     votes: Array[Byte],
                     startingNonce: Long,
                     finishingNonce: Long
                    ): Option[Header] = {
    require(finishingNonce >= startingNonce)

    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val bytesPerWord = n / 8
    val wordsPerHash = 512 / n

    val digest = new Blake2bDigest(null, bytesPerWord * wordsPerHash, null, ergoPerson) // scalastyle:ignore
    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, votes, nonce = 0L, EquihashSolution.empty)

    val I = HeaderSerializer.bytesWithoutPow(h)
    digest.update(I, 0, I.length)

    @tailrec
    def generateHeader(nonce: Long): Option[Header] = {
      log.debug("Trying nonce: " + nonce)
      val currentDigest = new Blake2bDigest(digest)
      Equihash.hashNonce(currentDigest, nonce)
      val solutions = Equihash.gbpBasic(currentDigest, n, k)
      val headerWithSuitableSolution = solutions
        .map { solution => h.copy(nonce = nonce, equihashSolution = solution) }
        .find { newHeader => correctWorkDone(realDifficulty(newHeader), difficulty) }
      headerWithSuitableSolution match {
        case headerWithFoundSolution @ Some(_) => headerWithFoundSolution
        case None if nonce + 1 < finishingNonce => generateHeader(nonce + 1)
        case _ => None
      }
    }

    generateHeader(startingNonce)
  }

  override def verify(header: Header): Boolean =
    Equihash.validateSolution(
      n,
      k,
      ergoPerson,
      HeaderSerializer.bytesWithoutPow(header) ++ Equihash.nonceToLeBytes(header.nonce),
      header.equihashSolution.indexedSeq
    )


  override def realDifficulty(header: Header): Difficulty = Constants.MaxTarget / BigInt(1, header.powHash)

  override def toString: String = s"EquihashPowScheme(n = ${n.toInt}, k = ${k.toInt})"
}
