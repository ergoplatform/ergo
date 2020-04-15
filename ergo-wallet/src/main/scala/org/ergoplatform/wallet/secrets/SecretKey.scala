package org.ergoplatform.wallet.secrets

import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

trait SecretKey {
  def key: SigmaProtocolPrivateInput[_, _]
}

sealed trait PrimitiveSecretKey extends SecretKey

object PrimitiveSecretKey {
  def apply(sigmaPrivateInput: SigmaProtocolPrivateInput[_, _]): PrimitiveSecretKey = sigmaPrivateInput match {
    case dls: DLogProverInput => DlogSecretWrapper(dls)
    case dhts: DiffieHellmanTupleProverInput => DhtSecretWrapper(dhts)
  }
}

case class DlogSecretWrapper(override val key: DLogProverInput) extends PrimitiveSecretKey

case class DhtSecretWrapper(override val key: DiffieHellmanTupleProverInput) extends PrimitiveSecretKey
