package org.ergoplatform.tools

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.{AutolykosPowScheme, CandidateBlock, randomSecret}
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.utils.ErgoTestHelpers

object MinerBench extends App with ErgoTestHelpers {

  val pow = new AutolykosPowScheme(21, 100000000)
  val sk = randomSecret()
  val difficulty = 1000
  val fb = invalidErgoFullBlockGen.sample.get
  val inHeader = fb.header
  val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
  val h = inHeader.copy(nBits = nBits)

  val candidate = new CandidateBlock(None, nBits: Long, h.stateRoot,
    fb.adProofs.get.proofBytes,
    fb.blockTransactions.txs,
    System.currentTimeMillis(),
    ExtensionCandidate(Seq(), Seq()))
  val newHeader = pow.proveCandidate(candidate, sk).get.header

  val Steps = 10000
  val st = System.currentTimeMillis()

  (0 until Steps) foreach { _ =>
    pow.validate(newHeader)
  }

  println(s"M = ${pow.M.length / 1024} Kb:${(System.currentTimeMillis() - st).toDouble / Steps} ms")


}
