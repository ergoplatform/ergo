package org.ergoplatform.mining

import org.ergoplatform.autoleakus.Autoleakus
import org.ergoplatform.autoleakus.pow.ksum.hashBinding.HKSumPowTask
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import scorex.core.block.Block.Timestamp
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.math.BigInt

class AutoleakusPowScheme(k: Int, N: Int) extends PowScheme {

  private val powTask = new HKSumPowTask(k: Int, N: Int)
  private val autoleakus = new Autoleakus(powTask)

  override def prove(parentOpt: Option[Header],
                     nBits: Long,
                     stateRoot:
                     ADDigest,
                     adProofsRoot: Digest32,
                     transactionsRoot: Digest32,
                     timestamp: Timestamp,
                     extensionHash: Digest32,
                     minNonce: Long,
                     maxNonce: Long): Option[Header] = {
    val sk: BigInt = ???
    val difficulty = RequiredDifficulty.decodeCompactBits(nBits)

    val (parentId, version, interlinks, height) = derivedHeaderFields(parentOpt)

    val h = Header(version, parentId, interlinks, adProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, null)
    val msg = HeaderSerializer.bytesWithoutPow(h)
    powTask.checkNonces(msg, sk, difficulty, minNonce, maxNonce).map(s => h.copy(powSolution = AutoleakusSolution(s)))
  }

  override def verify(header: Header): Boolean = {
    val difficulty = RequiredDifficulty.decodeCompactBits(header.nBits)
    autoleakus.verify(header.powSolution.solution, difficulty).isSuccess
  }

  override def realDifficulty(header: Header): BigInt = {
    org.ergoplatform.autoleakus.q / header.powSolution.d
  }

}
