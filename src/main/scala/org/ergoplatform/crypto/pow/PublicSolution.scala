package org.ergoplatform.crypto.pow

import org.bouncycastle.math.ec.ECPoint

case class PublicSolution(pairs: Seq[(SecretsSum, ECPoint)], message: Array[Byte], finalH: BigInt)