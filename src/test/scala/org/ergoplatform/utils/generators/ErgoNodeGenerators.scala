package org.ergoplatform.utils.generators

import org.ergoplatform.modifiers.history.popow.{NipopowProof, PoPowParams}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.utils.ErgoCoreTestConstants.nipopowAlgos
import org.scalacheck.{Arbitrary, Gen}

object ErgoNodeGenerators {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  lazy val emptyMemPoolGen: Gen[ErgoMemPool] =
    Gen.resultOf({ _: Unit => ErgoMemPool.empty(settings) })(Arbitrary(Gen.const(())))


  lazy val poPowProofGen: Gen[NipopowProof] = for {
    m <- Gen.chooseNum(1, 128)
    k <- Gen.chooseNum(1, 128)
    proof <- validNiPoPowProofGen(m, k)
  } yield proof

  def validNiPoPowProofGen(m: Int, k: Int): Gen[NipopowProof] = for {
    mulM <- Gen.chooseNum(1, 20)
  } yield {
    val chain = genHeaderChain(m * mulM + k, diffBitsOpt = None, useRealTs = false)
    val popowChain = popowHeaderChain(chain)
    val params = PoPowParams(m, k, continuous = false)
    nipopowAlgos.prove(popowChain)(params).get
  }
}
