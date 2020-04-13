package org.ergoplatform

import org.ergoplatform.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.secrets.SecretKey
import sigmastate.eval.IRContext
import sigmastate.interpreter.ProverInterpreter


class CustomSecretsProvingInterpreter(params: ErgoLikeParameters,
                                      extSecrets: IndexedSeq[SecretKey])(implicit IR: IRContext)
  extends ErgoProvingInterpreter(extSecrets, params) with ProverInterpreter {

  override val secrets = extSecrets.map(_.key)
}
