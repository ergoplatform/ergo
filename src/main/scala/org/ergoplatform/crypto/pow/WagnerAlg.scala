package org.ergoplatform.crypto.pow

import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class WagnerAlg(val k: Int,
                val p: BigInt) extends ScorexLogging {

  val halfP: BigInt = p / 2
  val n: Int = bitLength(p)

  /**
    * Filter initial elements, to ensure uniqueness and prove
    */
  def filterAndProve(elements: Seq[BigInt],
                     finalH: BigInt): Seq[PrivateSolution] = {
    prove(elements.zipWithIndex.toMap.map(_.swap), finalH)
  }

  def prove(initialMap: Map[Int, BigInt],
            finalH: BigInt): Seq[PrivateSolution] = {
    val h = bitLength(p - finalH)
    log(s"Going to find solutions form ${initialMap.size} elements for $k rounds, $n bits elements, " +
      s"hardness $h and final difficulty $finalH in group $p")

    @tailrec
    def wagnerStep(round: Int, sortedX: Seq[(BigInt, Seq[Int])]): Seq[(BigInt, Seq[Int])] = {
      val X: mutable.ArrayBuffer[(BigInt, Seq[Int])] = mutable.ArrayBuffer[(BigInt, Seq[Int])]()
      val atMost: BigInt = if (round != k) BigInt(2).pow(n - round * (h / (k + 1))) else finalH
      val atLeast: BigInt = if (round != k) p - atMost else p
      log(s"Round $round: search sums 0-$atMost || $atLeast-$p from ${sortedX.size} elements")

      @tailrec
      def loop(left: Int, right: Int): Unit = if (left < right) {
        val x = sortedX(left)
        var r: Int = left + 1
        var continue: Boolean = true
        var firstValidRight: Option[Int] = None
        if (x._1 < atMost) {
          // element itself is in the valid position, should search for close elements
          do {
            val y = sortedX(r)
            val sum = (x._1 + y._1).mod(p)
            if (distinctIndices(x._2, y._2) && sum <= atMost) {
              X.append(sum -> (x._2 ++ y._2))
              if (r < right) {
                r = r + 1
              } else {
                continue = false
              }
            } else {
              continue = false
            }
          } while (continue)
        }

        // from right till sum is too small
        continue = true
        r = right
        var previouSum = halfP
        do {
          val y = sortedX(r)
          join(x, y, atMost, atLeast) match {
            case (_, Some(jr)) =>
              // found some solution, try less element
              if (firstValidRight.isEmpty) firstValidRight = Some(r)
              X.append(jr)
              r = r - 1
            case (sum, _) if firstValidRight.isEmpty && sum < previouSum && r > left =>
              // we might start from too high right,
              previouSum = sum
              r = r - 1
            case (sum, _) =>
              continue = false
          }
        } while (continue)

        loop(left + 1, firstValidRight.getOrElse(right))
      }

      loop(0, sortedX.size - 1)

      if (round < k) {
        wagnerStep(round + 1, X.sortBy(_._1))
      } else {
        X
      }
    }

    val sols = wagnerStep(1, initialMap.toSeq.map(e => (e._2, Seq(e._1))).sortBy(_._1))
    sols.foreach(s => log(s"Solution with indexes ${s._2} found"))
    log(s"${sols.length} solutions found")

    sols.map { i =>
      PrivateSolution(i._2.map(e => (e, initialMap(e))))
    }
  }

  def validate(solution: PrivateSolution,
               finalH: BigInt): Try[Unit] = Try {
    val h = bitLength(p - finalH)
    require(solution.numbers.size == BigInt(2).pow(k), s"${solution.numbers.size} != ${BigInt(2).pow(k)}")

    def check(ints: Seq[BigInt], round: Int): Unit = {
      val atMost: BigInt = if (round != k) BigInt(2).pow(n - round * (h / (k + 1))) else finalH
      val atLeast: BigInt = if (round != k) p - atMost else p

      if (round < k) {
        val sums: Seq[BigInt] = ints.grouped(2).map(p => p.head + p.last).map(_.mod(p)).toSeq
        sums.foreach(sum => require(sum < atMost || sum > atLeast, s"Incorrect sum $sum at round $round from $ints"))
      } else {
        require(ints.sum.mod(p) < finalH)
      }
    }

    check(solution.numbers.map(_._2), 1)
  }

  private def join(x: (BigInt, Seq[Int]), y: (BigInt, Seq[Int]), atMost: BigInt, atLeast: BigInt): (BigInt, Option[(BigInt, Seq[Int])]) = {
    val sum = (x._1 + y._1).mod(p)
    if (distinctIndices(x._2, y._2) && (sum >= atLeast || sum <= atMost)) {
      sum -> Some(sum -> (x._2 ++ y._2))
    } else {
      sum -> None
    }
  }

  private def distinctIndices(a: Seq[Int], b: Seq[Int]): Boolean = !a.exists(v => b.contains(v))

  private def log(str: String): Unit = logger.debug(str)

  private def bitLength(bi: BigInt): Int = bi.toByteArray.length * 8
}
