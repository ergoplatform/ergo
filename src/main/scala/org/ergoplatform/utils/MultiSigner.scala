package org.ergoplatform.utils

import org.ergoplatform.settings.LaunchParameters
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import sigmastate.interpreter.HintsBag
import sigmastate.eval.{IRContext, RuntimeIRContext}

object MultiSigner extends App {
  implicit lazy val context: IRContext = new RuntimeIRContext

  val seedStrA = "edge talent poet tortoise trumpet dose"
  val seedStrB = "ergo talent poet tortoise trumpet rose"

  val secretsA = IndexedSeq(ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(seedStrA)))
  val secretsB = IndexedSeq(ExtendedSecretKey.deriveMasterKey(Mnemonic.toSeed(seedStrB)))

  val proverA = new ErgoProvingInterpreter(secretsA, LaunchParameters, HintsBag.empty)
  val proverB = new ErgoProvingInterpreter(secretsB, LaunchParameters, HintsBag.empty)



}
