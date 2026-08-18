package org.ergoplatform.nodeView.wallet

import org.ergoplatform.P2PKAddress
import org.ergoplatform.sdk.wallet.secrets.{DlogSecretKey, ExtendedSecretKey}
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

  property(".ownsAddress requires both tracking and an active signing key") {
    val otherSecret = ExtendedSecretKey.deriveMasterKey(
      Array.fill(32)(1: Byte),
      usePre1627KeyDerivation = false
    )
    val otherAddress = P2PKAddress(otherSecret.publicKey.key)(settings.addressEncoder)
    val rootProver = ErgoProvingInterpreter(defaultRootSecret, parameters)
    val bothProver = ErgoProvingInterpreter(
      IndexedSeq(defaultRootSecret, otherSecret),
      parameters
    )
    val primitiveProver = ErgoProvingInterpreter(
      IndexedSeq(defaultRootSecret, DlogSecretKey(otherSecret.privateInput)),
      parameters
    )
    val rootCache = WalletCache(Seq(defaultRootSecret.publicKey), settings)
    val bothCache = WalletCache(
      Seq(defaultRootSecret.publicKey, otherSecret.publicKey),
      settings
    )

    WalletVars(Some(rootProver), Seq.empty, Some(bothCache))
      .ownsAddress(otherAddress) shouldBe false
    WalletVars(Some(bothProver), Seq.empty, Some(rootCache))
      .ownsAddress(otherAddress) shouldBe false
    WalletVars(Some(bothProver), Seq.empty, Some(bothCache))
      .ownsAddress(otherAddress) shouldBe true
    WalletVars(Some(primitiveProver), Seq.empty, Some(bothCache))
      .ownsAddress(otherAddress) shouldBe true
  }

}
