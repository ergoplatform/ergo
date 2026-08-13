package org.ergoplatform.wallet.crypto

import org.ergoplatform.SigmaPropVerifier
import sigma.data.SigmaBoolean

import java.nio.charset.StandardCharsets
import java.security.SecureRandom

/**
  * How a message is wrapped before being signed with a wallet secret, and how such a signature is
  * checked afterwards. Signer and verifier have to agree on this, so it lives here rather than in
  * the node's API layer.
  *
  * A sigma proof over an arbitrary message is the very same object as the proof which spends a box:
  * both are a Fiat-Shamir transcript over some byte string. If a wallet signed the bytes it is given
  * verbatim, whoever asked for the signature could hand it the `messageToSign` of a transaction
  * spending the wallet's own boxes, and get back a proof which makes that transaction valid. The
  * bytes given are therefore never the bytes signed:
  *
  *   signed = [[MessageSigning.Prefix]] ++ salt ++ message
  *
  * with a fresh random `salt` per signature. The prefix says what the transcript is for, and the
  * salt makes the signed string unpredictable to whoever supplied the message, so it cannot be
  * steered onto a chosen byte string such as a transaction. `EIP-0028` (ErgoAuth) prescribes the
  * same thing for wallet applications: the wallet adds its own bytes and reports back what it
  * actually signed.
  *
  * [[MessageSigning.verify]] enforces the wrapping as well. Accepting an unwrapped message would
  * give the separation away, since a transaction input proof would then pass as a message signature.
  */
object MessageSigning {

  /** Says what a signed byte string is for, so that it cannot be read as anything else */
  val Prefix: Array[Byte] = "Ergo signed message:\n".getBytes(StandardCharsets.UTF_8)

  /** How many random bytes the signer puts between the prefix and the message */
  val SaltLength: Int = 32

  private val secureRandom = new SecureRandom()

  /** Fresh salt for one signature */
  def freshSalt(): Array[Byte] = {
    val salt = new Array[Byte](SaltLength)
    secureRandom.nextBytes(salt)
    salt
  }

  /** The byte string actually signed when `message` is signed with `salt` */
  def wrap(message: Array[Byte], salt: Array[Byte]): Array[Byte] = {
    require(salt.length == SaltLength, s"Salt must be $SaltLength bytes long, got ${salt.length}")
    Prefix ++ salt ++ message
  }

  /**
    * The message a signed byte string carries, if it is wrapped as [[wrap]] produces.
    *
    * @return the message, or None if `signedMessage` is not wrapped at all
    */
  def unwrap(signedMessage: Array[Byte]): Option[Array[Byte]] = {
    val headerLength = Prefix.length + SaltLength
    if (signedMessage.length >= headerLength &&
      java.util.Arrays.equals(signedMessage.take(Prefix.length), Prefix)) {
      Some(signedMessage.drop(headerLength))
    } else {
      None
    }
  }

  /**
    * Whether `proof` is a signature of `signedMessage` under `sigmaBoolean`.
    *
    * Unwrapped byte strings are rejected without looking at the proof, see the note on this object.
    */
  def verify(sigmaBoolean: SigmaBoolean, signedMessage: Array[Byte], proof: Array[Byte]): Boolean = {
    unwrap(signedMessage).isDefined &&
      new SigmaPropVerifier().verifySignature(sigmaBoolean, signedMessage, proof)(null)
  }

}
