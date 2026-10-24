package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils._
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.scalatest.concurrent.Eventually
import sigma.ast.ErgoTree

import scala.concurrent.duration._

/**
  * Start height of a wallet after /wallet/init and /wallet/restore (#1722), through the wallet actor and a real node view.
  * Archive node (blocksToKeep = -1), the regime in which #1722 occurs. Chain: b1 = genesis (pays the default miner
  * key), each later block moves the miner's coins to the miner key again (signed), b2 optionally paying `payTo`.
  */
class WalletStartHeightSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private implicit val patience: PatienceConfig = PatienceConfig(10.seconds, 100.millis)

  private def walletPass: SecretString = SecretString.create("init-scan-pass") // a fresh one per call: the wallet erases it

  private def mnemonic: SecretString = {
    val ws = settings.walletSettings
    new Mnemonic(ws.mnemonicPhraseLanguage, ws.seedStrengthBits)
      .toMnemonic(Array.fill(ws.seedStrengthBits / 8)(7: Byte)).get
  }

  private def uninitialized(tag: String): ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(blocksToKeep = -1),
    walletSettings = settings.walletSettings.copy(
      testMnemonic = None,
      secretStorage = settings.walletSettings.secretStorage.copy(
        secretDir = java.nio.file.Files.createTempDirectory(s"wallet-$tag-").toFile.getAbsolutePath
      )
    )
  )

  private val minerTree: ErgoTree = ErgoTree.fromSigmaBoolean(defaultMinerPk)

  /** Spends `in` (a miner-key box) with the miner's key: `payTo` gets its amount, the miner key the rest. */
  private def minerSpend(in: ErgoBox, payTo: Option[(ErgoAddress, Long)])(implicit w: WalletFixture): ErgoTransaction = {
    val h = getUtxoState.stateContext.currentHeight
    val paid = payTo.map { case (a, amt) => new ErgoBoxCandidate(amt, a.script, h) }.toIndexedSeq
    val change = new ErgoBoxCandidate(in.value - paid.map(_.value).sum, minerTree, h)
    val utx = UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(in.id)), paid :+ change)
    ErgoTransaction(defaultProver.sign(utx, IndexedSeq(in), IndexedSeq.empty, getUtxoState.stateContext).get)
  }

  private def minerOut(tx: ErgoTransaction): ErgoBox = tx.outputs.find(_.ergoTree == minerTree).get

  /** Applies a block holding `tx` and waits until the state is at `height`. */
  private def applyNext(tx: ErgoTransaction, height: Int)(implicit w: WalletFixture): ErgoFullBlock = {
    val b = makeNextBlock(getUtxoState, Seq(tx))
    applyBlock(b) shouldBe 'success
    eventually { getCurrentState.stateContext.currentHeight shouldBe height }
    b
  }

  /** Applies b1..b3; returns the blocks and the miner-key box left at b3. */
  private def threeBlocks(payTo: Option[(ErgoAddress, Long)])(implicit w: WalletFixture): (Seq[ErgoFullBlock], ErgoBox) = {
    val b1 = makeGenesisBlock(defaultMinerPk)
    applyBlock(b1) shouldBe 'success
    eventually { getCurrentState.stateContext.currentHeight shouldBe 1 }
    val minerBox = boxesAvailable(b1, defaultMinerPk).maxBy(_.value)
    val tx2 = minerSpend(minerBox, payTo)
    val b2 = applyNext(tx2, 2)
    val tx3 = minerSpend(minerOut(tx2), None)
    val b3 = applyNext(tx3, 3)
    (Seq(b1, b2, b3), minerOut(tx3))
  }

  /** First listed address of the wallet restored from `mnemonic`, read from a throwaway node view. */
  private def restoredFirstAddress(): P2PKAddress = {
    implicit val w0: WalletFixture = new WalletFixture(uninitialized("probe"), parameters, getCurrentView(_).vault)
    try {
      await(wallet.getWalletStatus).initialized shouldBe false
      await(wallet.restoreWallet(walletPass, mnemonic, None, usePre1627KeyDerivation = false)).isSuccess shouldBe true
      eventually { getPublicKeys should not be empty }
      getPublicKeys.head
    } finally w0.stop()
  }

  property("a restored wallet scans history and finds a box paid before the restore") {
    val addr = restoredFirstAddress()
    implicit val w: WalletFixture = new WalletFixture(uninitialized("restore"), parameters, getCurrentView(_).vault)
    try {
      await(wallet.getWalletStatus).initialized shouldBe false
      val amount = 1000000000L
      val (_, minerBox) = threeBlocks(Some(addr -> amount))
      await(wallet.restoreWallet(walletPass, mnemonic, None, usePre1627KeyDerivation = false)).isSuccess shouldBe true
      eventually { getPublicKeys.head shouldBe addr }
      val tx4 = minerSpend(minerBox, None)
      applyNext(tx4, 4)
      applyNext(minerSpend(minerOut(tx4), None), 5)
      eventually {
        getConfirmedBalances.walletBalance shouldBe amount
        await(wallet.getWalletStatus).height shouldBe 5
      }
    } finally w.stop()
  }

  property("a rescan from height 0 right after a restore finds a box paid before the restore") {
    val addr = restoredFirstAddress()
    implicit val w: WalletFixture = new WalletFixture(uninitialized("rescan-a"), parameters, getCurrentView(_).vault)
    try {
      val amount = 1000000000L
      threeBlocks(Some(addr -> amount))
      await(wallet.restoreWallet(walletPass, mnemonic, None, usePre1627KeyDerivation = false)).isSuccess shouldBe true
      eventually { getPublicKeys.head shouldBe addr }
      await(wallet.rescanWallet(0)).isSuccess shouldBe true
      eventually {
        getConfirmedBalances.walletBalance shouldBe amount
        await(wallet.getWalletStatus).height shouldBe 3
      }
    } finally w.stop()
  }

  property("after init and two scanned blocks, a rollback to the block that was the tip at init succeeds") {
    implicit val w: WalletFixture = new WalletFixture(uninitialized("rollback"), parameters, getCurrentView(_).vault)
    try {
      await(wallet.getWalletStatus).initialized shouldBe false
      val (blocks, minerBox) = threeBlocks(None)
      await(wallet.initWallet(walletPass, None)).isSuccess shouldBe true
      eventually { await(wallet.getWalletStatus).initialized shouldBe true }
      val tx4 = minerSpend(minerBox, None)
      applyNext(tx4, 4)
      applyNext(minerSpend(minerOut(tx4), None), 5)
      eventually { await(wallet.getWalletStatus).height shouldBe 5 }
      getCurrentView.vault.rollback(idToVersion(blocks(2).id)).isSuccess shouldBe true
      eventually {
        val st = await(wallet.getWalletStatus)
        st.error shouldBe None
        st.height shouldBe 3
      }
    } finally w.stop()
  }

}
