package org.ergoplatform.crypto

import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.BcDlogFp
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

package object pow {

  type PrivateKey = BigInt
  type SecretsSum = BigInt

  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup

  def genPk(s: PrivateKey): ECPoint = group.exponentiate(group.generator, s.bigInteger)

}