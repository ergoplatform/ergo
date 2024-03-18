package org.ergoplatform.nodeView.wallet

import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter

class WalletVarsSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  property(".withProver init") {
    val prover = ErgoProvingInterpreter(defaultRootSecret, parameters)
    val walletVars = WalletVars(None, Seq.empty, None)
    val wp = walletVars.withProver(prover)

    wp.trackedPubKeys.length shouldBe 1
    wp.trackedBytes.length shouldBe 1

    defaultRootSecret.publicKey shouldBe wp.trackedPubKeys.head
  }

}
