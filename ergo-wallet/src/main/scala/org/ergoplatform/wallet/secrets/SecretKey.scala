package org.ergoplatform.wallet.secrets

import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

trait SecretKey {
  def key: SigmaProtocolPrivateInput[_, _]
}

object SecretKey {
  def apply(sigmaPrivateInput: SigmaProtocolPrivateInput[_, _]): SecretKey = sigmaPrivateInput match {
    case dls: DLogProverInput => DlogSecretWrapper(dls)
    case dhts: DiffieHellmanTupleProverInput => DhtSecretWrapper(dhts)
  }
}


case class DlogSecretWrapper(override val key: DLogProverInput) extends SecretKey

case class DhtSecretWrapper(override val key: DiffieHellmanTupleProverInput) extends SecretKey
