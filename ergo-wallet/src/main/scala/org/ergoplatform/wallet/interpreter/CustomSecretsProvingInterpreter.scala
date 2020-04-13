package org.ergoplatform.wallet.interpreter

import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.wallet.secrets.SecretKey
import sigmastate.basics.SigmaProtocolPrivateInput
import sigmastate.eval.IRContext
import sigmastate.interpreter.ProverInterpreter


class CustomSecretsProvingInterpreter(params: ErgoLikeParameters,
                                      extSecrets: IndexedSeq[SecretKey])(implicit IR: IRContext)
  extends ErgoProvingInterpreter(extSecrets, params) with ProverInterpreter {

  override val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = extSecrets.map(_.key)
}
