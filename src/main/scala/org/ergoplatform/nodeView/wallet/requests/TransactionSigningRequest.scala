package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import org.ergoplatform.wallet.secrets.{DhtSecretKey, DlogSecretKey}

/**
  * A request to sign a transaction
  *
  * @param unsignedTx - unsigned transaction
  * @param hints      - hints for interpreter (such as additional one-time secrets)
  * @param externalSecrets - externally provided secrets
  * @param inputs     - hex-encoded input boxes bytes for the unsigned transaction (optional)
  * @param dataInputs - hex-encoded data-input boxes bytes for the unsigned transaction (optional)
  */
case class TransactionSigningRequest(unsignedTx: UnsignedErgoTransaction,
                                     hints: TransactionHintsBag,
                                     externalSecrets: Seq[ExternalSecret],
                                     inputs: Option[Seq[String]],
                                     dataInputs: Option[Seq[String]]) {

  lazy val dlogs: Seq[DlogSecretKey] = externalSecrets.collect { case ExternalSecret(d: DlogSecretKey) => d }

  lazy val dhts: Seq[DhtSecretKey] = externalSecrets.collect { case ExternalSecret(d: DhtSecretKey) => d }

}

/**
  * A request to sign a message
  *
  * @param unsignedTx - unsigned transaction ?? is this needed; perhaps just message?
  * @param hints      - hints for interpreter (such as additional one-time secrets)
  * @param externalSecrets - externally provided secrets
  * @param inputs     - hex-encoded input boxes bytes for the unsigned transaction (optional)
  * @param dataInputs - hex-encoded data-input boxes bytes for the unsigned transaction (optional)
  * @param message    - message to sign
  */
case class MessageSigningRequest(unsignedTx: UnsignedErgoTransaction,
                                 hints: TransactionHintsBag,
                                 externalSecrets: Seq[ExternalSecret],
                                 inputs: Option[Seq[String]],
                                 dataInputs: Option[Seq[String]],
                                 message: Option[Seq[String]]) {

  lazy val dlogs: Seq[DlogSecretKey] = externalSecrets.collect { case ExternalSecret(d: DlogSecretKey) => d }

  lazy val dhts: Seq[DhtSecretKey] = externalSecrets.collect { case ExternalSecret(d: DhtSecretKey) => d }

}

/**
  * A request to verify a message
  *
  * @param signedMessage - unsigned transaction ?? is this needed; perhaps just message?
  * @param message       - hints for interpreter (such as additional one-time secrets)
  */
case class MessageVerifyRequest(unsignedTx: UnsignedErgoTransaction,
                                hints: TransactionHintsBag,
                                externalSecrets: Seq[ExternalSecret],
                                signedMessage: Option[String],
                                message: Option[Seq[String]]) { }
