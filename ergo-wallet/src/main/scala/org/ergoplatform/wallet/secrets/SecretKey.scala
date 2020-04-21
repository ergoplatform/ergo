package org.ergoplatform.wallet.secrets

import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

/**
  * Basic trait for secret data, encapsulating a corresponding private inputs for a Sigma protocol.
  */
trait SecretKey {
  def key: SigmaProtocolPrivateInput[_, _]
}

/**
  * Basic trait for a secret which does not have a derivation scheme.
  */
sealed trait PrimitiveSecretKey extends SecretKey

object PrimitiveSecretKey {
  def apply(sigmaPrivateInput: SigmaProtocolPrivateInput[_, _]): PrimitiveSecretKey = sigmaPrivateInput match {
    case dls: DLogProverInput => DlogSecretWrapper(dls)
    case dhts: DiffieHellmanTupleProverInput => DhtSecretWrapper(dhts)
  }
}

/**
  * Secret exponent of a group element, i.e. secret w such as h = g^^w, where g is group generator, h is a public key.
  * @param key - secret (in form of a sigma-protocol private input)
  */
case class DlogSecretWrapper(override val key: DLogProverInput) extends PrimitiveSecretKey

/**
  * Secret exponent of a Diffie-Hellman tuple, i.e. secret w such as u = g^^w and v = h^^w, where g and h are group
  * generators, (g,h,u,v) is a public input (public key).
  * @param key - secret (in form of a sigma-protocol private input)
  */
case class DhtSecretWrapper(override val key: DiffieHellmanTupleProverInput) extends PrimitiveSecretKey
