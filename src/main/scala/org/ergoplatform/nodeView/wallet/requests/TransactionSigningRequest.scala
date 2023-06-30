package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag

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

