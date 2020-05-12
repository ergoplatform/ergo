package org.ergoplatform.wallet.secrets

import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

/**
  * Basic trait for secret data, encapsulating a corresponding private inputs for a Sigma protocol.
  */
trait SecretKey {
  /**
    * Private (secret) input of a sigma protocol
    */
  def privateInput: SigmaProtocolPrivateInput[_, _]
}

/**
  * Basic trait for a secret which does not have a derivation scheme.
  */
sealed trait PrimitiveSecretKey extends SecretKey

object PrimitiveSecretKey {
  def apply(sigmaPrivateInput: SigmaProtocolPrivateInput[_, _]): PrimitiveSecretKey = sigmaPrivateInput match {
    case dls: DLogProverInput => DlogSecretKey(dls)
    case dhts: DiffieHellmanTupleProverInput => DhtSecretKey(dhts)
  }
}

/**
  * Secret exponent of a group element, i.e. secret w such as h = g^^w, where g is group generator, h is a public key.
  * @param privateInput - secret (in form of a sigma-protocol private input)
  */
case class DlogSecretKey(override val privateInput: DLogProverInput) extends PrimitiveSecretKey

/**
  * Secret exponent of a Diffie-Hellman tuple, i.e. secret w such as u = g^^w and v = h^^w, where g and h are group
  * generators, (g,h,u,v) is a public input (public key).
  * @param privateInput - secret (in form of a sigma-protocol private input)
  */
case class DhtSecretKey(override val privateInput: DiffieHellmanTupleProverInput) extends PrimitiveSecretKey
