package org.ergoplatform.mining

import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.ksum.hashBinding.{HKSumNonce, HKSumSolution}

case class AutoleakusSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: HKSumNonce, d: BigInt) {
  def solution: HKSumSolution = HKSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: HKSumNonce, d: BigInt)
}

object AutoleakusSolution {
  def apply(s: HKSumSolution): AutoleakusSolution = AutoleakusSolution(s.m, s.pk, s.w, s.n, s.d)

  implicit val jsonEncoder: Encoder[AutoleakusSolution] = { b: AutoleakusSolution =>
    ???
  }

  implicit val jsonDecoder: Decoder[AutoleakusSolution] = { c: HCursor =>
    ???
  }
}
