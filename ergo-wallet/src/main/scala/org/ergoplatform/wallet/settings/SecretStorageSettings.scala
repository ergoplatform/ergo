package org.ergoplatform.wallet.settings

import org.ergoplatform.sdk.wallet.settings.EncryptionSettings

final case class SecretStorageSettings(secretDir: String, encryption: EncryptionSettings)
