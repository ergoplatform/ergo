package org.ergoplatform.settings

import org.ergoplatform.nodeView.wallet.WalletProfile
import org.ergoplatform.wallet.settings.SecretStorageSettings

case class WalletSettings(secretStorage: SecretStorageSettings,
                          seedStrengthBits: Int,
                          mnemonicPhraseLanguage: String,
                          usePreEip3Derivation: Boolean = false,
                          keepSpentBoxes: Boolean = false,
                          defaultTransactionFee: Long = 1000000L,
                          dustLimit: Option[Long] = None,
                          maxInputs: Int = 100,
                          optimalInputs: Int = 3,
                          testMnemonic: Option[String] = None,
                          testKeysQty: Option[Int] = None,
                          // Some(Seq(x)) burns all except x, Some(Seq.empty) burns all, None ignores that feature
                          tokensWhitelist: Option[Seq[String]] = None,
                          checkEIP27: Boolean = false,
                          profile: String = WalletProfile.User.label) {

  val walletProfile: WalletProfile = WalletProfile.fromLabel(profile)

}
