package org.ergoplatform.crypto.pow

case class PublicSolution(pairs: Seq[(SecretsSum, PublicKey)])

object PublicSolution {
  def apply(pr: PrivateSolution): PublicSolution = {
    assert(pr.numbers.length % 2 == 0, s"Incorrect incoming size: ${pr.numbers.length}")
    val pairs = pr.numbers.grouped(2).map { s =>
      val secretsSum = s.head + s.last
      val pk = genPk(s.head)
      (secretsSum, pk)
    }.toSeq
    PublicSolution(pairs)
  }
}
