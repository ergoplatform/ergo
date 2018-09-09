package org.ergoplatform.crypto.pow

import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.Try

class Supow(secretsMap: Map[Int, PrivateKey],
            val k: Int)(implicit hash: SuHash) extends ScorexLogging {

  val prover: WagnerAlg = new WagnerAlg(k, group.p)

  def prove(finalH: BigInt, message: Array[Byte]): Seq[PublicSolution] = {
    log.debug("Generate initial list")
    val initialMap: Map[Int, BigInt] = secretsMap.map { is =>
      // TODO we may pre-generate this public keys and reuse between different proves
      val pk = genPk(is._2)
      val x = is._2 - pseudoRandom(message, pk)
      (is._1, x)
    }
    log.debug("Prove")

    prover.prove(initialMap, finalH).map(pr => privateToPublic(pr, message, finalH, secretsMap))
  }

  def verify(s: PublicSolution): Try[Unit] = Try {
    val publicKeys: Seq[ECPoint] = s.pairs.flatMap { p =>
      val pk0 = p._2
      val pk1 = group.exponentiate(group.generator, p._1.bigInteger).subtract(pk0)
      Seq(pk0, pk1)
    }
    val rand: Seq[(ECPoint, BigInt)] = publicKeys.map(pk => pk -> pseudoRandom(s.message, pk))
    val secretsSum = s.pairs.map(_._1).sum.mod(group.p)
    val XSum: BigInt = secretsSum - rand.map(_._2).sum
    val leftSide: ECPoint = publicKeys.reduce((a, b) => a.add(b))
    val exp = (publicKeys.map(pk => pseudoRandom(s.message, pk)).sum + XSum).mod(group.p)
    val rightSide: ECPoint = group.exponentiate(group.generator, exp.bigInteger)
    require(leftSide == rightSide, s"Incorrect pubkey solution: $leftSide == $rightSide")
  }

  private def privateToPublic(pr: PrivateSolution,
                              message: Array[Byte],
                              finalH: BigInt,
                              secretsMap: Map[Int, BigInt]): PublicSolution = {
    assert(pr.numbers.length % 2 == 0, s"Incorrect incoming size: ${pr.numbers.length}")
    val secrets: Seq[PrivateKey] = pr.numbers.map(i => secretsMap(i._1))
    val pairs = secrets.grouped(2).map { s =>
      val s0 = s.head
      val s1 = s.last
      val sum = (s0 + s1).mod(group.p)
      val p0 = group.exponentiate(group.generator, s0.bigInteger)
      (sum, p0)
    }.toSeq
    PublicSolution(pairs, message: Array[Byte], finalH: BigInt)
  }

  private def pseudoRandom(message: Array[Byte], pk: ECPoint): BigInt = hash.hash(message ++ pk.getEncoded(true))

  private def recoverP2(sum: BigInt, p1: ECPoint): ECPoint = {
    val sumPk = group.exponentiate(group.generator, sum.bigInteger)
    sumPk.subtract(p1)
  }

}

object Supow {

  def apply(seedStr: String,
            size: Long,
            k: Int)(implicit hash: SuHash): Supow = {
    val secrets = (1L to size).map(i => BigInt(BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i + seedStr).take(30))))
    new Supow(secrets.zipWithIndex.map(_.swap).toMap, k)
  }
}