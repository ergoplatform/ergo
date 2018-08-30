package org.ergoplatform.crypto.pow

import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class WagnerAlg(k: Int,
                n: Int,
                p: BigInt) extends ScorexLogging {

  val halfP: BigInt = p / 2

  def prove(initialMap: Map[Int, BigInt],
            h: Int,
            finalH: BigInt): Seq[PrivateSolution] = {
    assert((n - h) % (k + 1) == 0, s"${n - h} % ${k + 1} != 0")

    log(s"Going to find solutions form ${initialMap.size} elements for $k rounds, $n bits elements, " +
      s"hardness $h and final difficulty $finalH in group $p")

    @tailrec
    def wagnerStep(round: Int, sortedX: Seq[(BigInt, Seq[Int])]): Seq[(BigInt, Seq[Int])] = {
      val X: mutable.ArrayBuffer[(BigInt, Seq[Int])] = mutable.ArrayBuffer[(BigInt, Seq[Int])]()
      val atMost: BigInt = if (round != k) {
        BigInt(2).pow(n - round * (h / (k + 1)))
      } else finalH
      val atLeast: BigInt = if (round != k) {
        p - atMost
      } else p
      log(s"Round $round: search sums 0-$atMost || $atLeast-$p from ${sortedX.size} elements")

      @tailrec
      def loop(left: Int, right: Int): Unit = if (left < right) {
        val x = sortedX(left)
        val y = sortedX(right)
        if (distinctIndices(x._2, y._2)) {
          val sumMod = (x._1 + y._1).mod(p)
          if (sumMod <= atMost || sumMod >= atLeast) {
            X.append(sumMod -> (x._2 ++ y._2))
          }
          if (sumMod > halfP) {
            loop(left + 1, right)
          } else {
            loop(left, right - 1)
          }
        }
      }

      loop(0, sortedX.size - 1)

      if (round < k) {
        wagnerStep(round + 1, X.sortBy(_._1))
      } else {
        X
      }
    }

    val sols = wagnerStep(1, initialMap.toSeq.map(e => (e._2, Seq(e._1))).sortBy(_._1))

    sols.map { i =>
      PrivateSolution(i._2.map(index => initialMap(index)))
    }
  }

  def validate(solution: PrivateSolution,
               h: Int,
               finalH: BigInt): Try[Unit] = Try {
    require(solution.numbers.size == BigInt(2).pow(k), s"${solution.numbers.size} != ${BigInt(2).pow(k)}")

    val roundIntervals: Map[Int, BigInt] = (1 to k + 1).map(i => i -> (BigInt(2).pow(n - i * (h / (k + 1))))).toMap
    val revercedRoundIntervals: Map[Int, BigInt] = roundIntervals.map(i => i._1 -> (p - i._2))

    def hasCollision(sum: BigInt, round: Int): Boolean = {
      sum < roundIntervals(round) || sum > revercedRoundIntervals(round)
    }

    def check(ints: Seq[BigInt], round: Int): Unit = {
      if (round < k) {
        val sums: Seq[BigInt] = ints.grouped(2).map(p => p.head + p.last).map(_.mod(p)).toSeq
        sums.foreach(s => require(hasCollision(s, round), s"Incorrect sum $s at round $round from $ints"))
      } else {
        require(ints.sum.mod(p) < finalH)
      }
    }

    check(solution.numbers, 1)
  }

  private def distinctIndices(a: Seq[Int], b: Seq[Int]): Boolean = !a.exists(v => b.contains(v))

  private def log(str: String): Unit = logger.debug(str)

}
