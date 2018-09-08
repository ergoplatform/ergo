package org.ergoplatform.crypto

import scapi.sigma.BcDlogFp
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

package object pow {

  type PrivateKey = BigInt
  type PublicKey = Array[Byte]
  type SecretsSum = BigInt

  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup

  def genPk(s: PrivateKey): PublicKey = group.exponentiate(group.generator, s.bigInteger).getEncoded(true)

}