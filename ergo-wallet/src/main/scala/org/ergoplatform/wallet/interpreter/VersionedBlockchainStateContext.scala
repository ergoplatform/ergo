package org.ergoplatform.wallet.interpreter

import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import sigma.validation.SigmaValidationSettings

abstract class VersionedBlockchainStateContext extends BlockchainStateContext {
  val sigmaValidationSettings: SigmaValidationSettings
}
