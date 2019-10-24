package org.ergoplatform.settings

import org.ergoplatform.wallet.settings.SecretStorageSettings

import scala.concurrent.duration.FiniteDuration

case class WalletSettings(secretStorage: SecretStorageSettings,
                          seedStrengthBits: Int,
                          mnemonicPhraseLanguage: String,
                          scanningInterval: FiniteDuration,
                          keepSpentBoxes: Boolean = false,
                          defaultTransactionFee: Long = 1000000L,
                          testMnemonic: Option[String] = None,
                          testKeysQty: Option[Int] = None)
