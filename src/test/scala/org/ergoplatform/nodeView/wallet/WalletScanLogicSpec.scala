package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.wallet.ErgoWalletActor.WalletVars
import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.utils.WalletTestOps
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.scalatest.PropSpec
import WalletScanLogic.{extractWalletOutputs, scanBlockTransactions}

class WalletScanLogicSpec extends PropSpec with WalletTestOps {

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(LaunchParameters)
  private val prover = defaultProver
  private val walletVars = WalletVars(Some(prover), Seq.empty)(settings)

  property("extractWalletOutputs") {

  }

  property("scanBlockTransactions") {

  }

}