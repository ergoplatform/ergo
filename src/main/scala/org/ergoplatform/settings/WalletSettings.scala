package org.ergoplatform.settings

import org.ergoplatform.wallet.settings.SecretStorageSettings


case class WalletSettings(secretStorage: SecretStorageSettings,
                          seedStrengthBits: Int,
                          mnemonicPhraseLanguage: String,
                          usePreEip3Derivation: Boolean = false,
                          keepSpentBoxes: Boolean = false,
                          defaultTransactionFee: Long = 1000000L,
                          maxInputs: Int = 100,
                          optimalInputs: Int = 3,
                          testMnemonic: Option[String] = None,
                          testKeysQty: Option[Int] = None)
