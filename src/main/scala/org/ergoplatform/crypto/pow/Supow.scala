package org.ergoplatform.crypto.pow

import com.google.common.primitives.Longs
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.Try

class Supow(secrets: Seq[PrivateKey],
            val k: Int)(implicit hash: SuHash) extends ScorexLogging {

  val prover: WagnerAlg = new WagnerAlg(k, group.p)

  def prove(finalH: BigInt, message: Array[Byte]): Seq[PublicSolution] = {
    log.debug("Generate initial list")
    val initialMap: Map[Int, BigInt] = secrets.zipWithIndex.toMap.map { is =>
      // TODO we may pre-generate this public keys and reuse between different proves
      val pk = genPk(is._1)
      val x = is._1 + hash.hash(message ++ pk)
      (is._2, x)
    }
    log.debug("Prove")
    prover.prove(initialMap, finalH).map(PublicSolution.apply)
  }

  def verify(s: PublicSolution, finalH: BigInt): Try[Unit] = Try {
    ???
  }

}

object Supow {

  def apply(seed: Array[Byte],
            size: Long,
            k: Int)(implicit hash: SuHash): Supow = {
    val secrets = (0L until size).map(i => BigInt(Blake2b256(seed ++ Longs.toByteArray(i))))
    new Supow(secrets, k)
  }
}