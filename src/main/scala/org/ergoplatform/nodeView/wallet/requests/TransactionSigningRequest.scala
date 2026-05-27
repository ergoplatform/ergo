package org.ergoplatform.nodeView.wallet.requests

import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.wallet.interpreter.TransactionHintsBag
import sigma.crypto.EcPointType

/**
  * A request to sign a transaction
  *
  * @param unsignedTx - unsigned transaction
  * @param hints      - hints for interpreter (such as additional one-time secrets)
  * @param externalSecrets - externally provided secrets
  * @param inputs     - hex-encoded input boxes bytes for the unsigned transaction (optional)
  * @param dataInputs - hex-encoded data-input boxes bytes for the unsigned transaction (optional)
  * @param minerPk    - optional forged miner public key for the upcoming preHeader.
  *                     When present, the prover signs against an upcoming state context that
  *                     uses this pk instead of the placeholder one in `simplifiedUpcoming`.
  *                     A tx signed under a forged minerPk only validates inside that same
  *                     synthetic context and will not be accepted by the real network.
  */
case class TransactionSigningRequest(unsignedTx: UnsignedErgoTransaction,
                                     hints: TransactionHintsBag,
                                     externalSecrets: Seq[ExternalSecret],
                                     inputs: Option[Seq[String]],
                                     dataInputs: Option[Seq[String]],
                                     minerPk: Option[EcPointType] = None) {

  lazy val dlogs: Seq[DlogSecretKey] = externalSecrets.collect { case ExternalSecret(d: DlogSecretKey) => d }

  lazy val dhts: Seq[DhtSecretKey] = externalSecrets.collect { case ExternalSecret(d: DhtSecretKey) => d }

}
