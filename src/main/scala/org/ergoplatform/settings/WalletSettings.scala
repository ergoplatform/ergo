package org.ergoplatform.settings

import org.ergoplatform.wallet.settings.SecretStorageSettings


case class WalletSettings(secretStorage: SecretStorageSettings,
                          seedStrengthBits: Int,
                          mnemonicPhraseLanguage: String,
                          keepSpentBoxes: Boolean = false,
                          defaultTransactionFee: Long = 1000000L,
                          testMnemonic: Option[String] = None,
                          testKeysQty: Option[Int] = None,
                          keysFilter: CuckooFilterSettings)
