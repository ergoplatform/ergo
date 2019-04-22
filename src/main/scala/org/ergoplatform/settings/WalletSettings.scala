package org.ergoplatform.settings

import org.ergoplatform.wallet.settings.SecretStorageSettings

import scala.concurrent.duration.FiniteDuration

case class WalletSettings(secretStorageSettings: SecretStorageSettings,
                          seedStrengthBits: Int,
                          mnemonicPhraseLanguage: String,
                          scanningInterval: FiniteDuration,
                          defaultTransactionFee: Long = 100000L)
