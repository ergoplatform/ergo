package org.ergoplatform.mining

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.math.BigInt
import scala.util.Random

class FakePowScheme(levelOpt: Option[Int]) extends PowScheme {

  def prove(parentOpt: Option[Header],
            nBits: Long,
            stateRoot: ADDigest,
            adProofsRoot: Digest32,
            transactionsRoot: Digest32,
            timestamp: Timestamp,
            extensionHash: Digest32
           ): Option[Header] = {
    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)
    val level: Int = levelOpt.map(lvl => BigInt(2).pow(lvl).toInt).getOrElse(Random.nextInt(1000) + 1)
    val solution = EquihashSolution(level +: Seq.fill(EquihashSolution.length - 1)(Random.nextInt))
    Some(new Header(version, parentId, interlinks,
      adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits, height, extensionHash, solution))
  }

  override def verify(header: Header): Boolean = true

  override def realDifficulty(header: Header): Difficulty =
    header.equihashSolution.headOption.getOrElse(0) * header.requiredDifficulty

}

object DefaultFakePowScheme extends FakePowScheme(None)
