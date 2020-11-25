package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.wallet.secrets.{DhtSecretKey, DlogSecretKey}

/**
  * A request to generate commitments for unsigned transaction, useful for multi-party signing.
  *
  * @param unsignedTx - unsigned transaction
  * @param externalSecretsOpt - optionally, externally provided secrets
  */
case class GenerateCommitmentsRequest(unsignedTx: UnsignedErgoTransaction,
                                      externalSecretsOpt: Option[Seq[ExternalSecret]]) {

  lazy val externalSecrets: Seq[ExternalSecret] = externalSecretsOpt.getOrElse(Seq.empty)

  lazy val dlogs: Seq[DlogSecretKey] = externalSecrets.collect { case ExternalSecret(d: DlogSecretKey) => d }

  lazy val dhts: Seq[DhtSecretKey] = externalSecrets.collect { case ExternalSecret(d: DhtSecretKey) => d }
}
