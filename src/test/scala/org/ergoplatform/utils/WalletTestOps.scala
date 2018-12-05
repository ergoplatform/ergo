package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, TokenId}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.{BalancesSnapshot, ErgoWallet}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform._
import org.ergoplatform.local.ErgoMiner
import org.ergoplatform.mining.emission.EmissionRules
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId}
import sigmastate.Values.{EvaluatedValue, LongConstant, TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.{SBoolean, SLong}

import scala.concurrent.blocking

trait WalletTestOps extends NodeViewBaseOps {

  def newAssetIdStub: TokenId = Digest32 @@ Array.emptyByteArray

  def withFixture[T](test: WalletFixture => T): T = {
    new WalletFixture(settings, getCurrentView(_).vault).apply(test)
  }

  def wallet(implicit w: WalletFixture): ErgoWallet = w.wallet

  def getPublicKeys(implicit w: WalletFixture): Seq[P2PKAddress] =
    await(w.wallet.publicKeys(0, Int.MaxValue))

  def getConfirmedBalances(implicit w: WalletFixture): BalancesSnapshot =
    await(w.wallet.confirmedBalances())

  def getBalancesWithUnconfirmed(implicit w: WalletFixture): BalancesSnapshot =
    await(w.wallet.balancesWithUnconfirmed())

  def scanningInterval(implicit ctx: Ctx): Long = ctx.settings.walletSettings.scanningInterval.toMillis

  def waitForScanning(block: ErgoFullBlock)(implicit ctx: Ctx): Unit = {
    blocking(Thread.sleep(scanTime(block)))
  }

  def scanTime(block: ErgoFullBlock)(implicit ctx: Ctx): Long = {
    val boxes = block.transactions.flatMap(_.outputs)
    val tokens = boxes.flatMap(_.additionalTokens)
    scanTime(boxes.size, tokens.size)
  }

  def scanTime(boxCount: Int, tokenCount: Int)(implicit ctx: Ctx): Long = {
    boxCount * scanningInterval + tokenCount * scanningInterval * 2 + 1000
  }

  def waitForOffchainScanning(tx: ErgoTransaction): Unit = {
    blocking(Thread.sleep(offchainScanTime(tx)))
  }

  def offchainScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

  def balanceAmount(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum

  def boxesAvailable(block: ErgoFullBlock, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    block.transactions.flatMap(boxesAvailable(_, script))
  }

  def boxesAvailable(tx: ErgoTransaction, script: Value[SBoolean.type]): Seq[ErgoBox] = {
    tx.outputs.filter(_.proposition == script)
  }

  def assetAmount(boxes: Seq[ErgoBoxCandidate]): Map[ModifierId, Long] = {
    assetsByTokenId(boxes).map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
  }

  def toAssetMap(assetSeq: Seq[(TokenId, Long)]): Map[ModifierId, Long] = {
    assetSeq
      .map { case (tokenId, sum) => (bytesToId(tokenId), sum) }
      .toMap
  }

  def assetsByTokenId(boxes: Seq[ErgoBoxCandidate]): Map[TokenId, Long] = {
    boxes
      .flatMap { _.additionalTokens }
      .groupBy { case (tokenId, _) => tokenId }
      .map { case (id, pairs) => id -> pairs.map(_._2).sum }
  }

  def getUtxoState(implicit ctx: Ctx): UtxoState = getCurrentState.asInstanceOf[UtxoState]

  def getHeightOf(state: ErgoState[_])(implicit ctx: Ctx): Option[Int] = {
    getHistory.heightOf(scorex.core.versionToId(state.version))
  }

  def makeGenesisBlock(script: ProveDlog, assets: Seq[(TokenId, Long)] = Seq.empty)
                      (implicit ctx: Ctx): ErgoFullBlock = {
    makeNextBlock(getUtxoState, Seq(makeGenesisTx(script, assets)))
  }

  def makeGenesisTx(publicKey: ProveDlog, assetsIn: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
    val assets: Seq[(TokenId, Long)] = replaceNewAssetStub(assetsIn, inputs)
    ErgoMiner.collectRewards(Some(genesisEmissionBox),
      ErgoHistory.EmptyHistoryHeight,
      Seq.empty,
      publicKey,
      settings.emission,
      assets).head
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     addressToSpend: ErgoAddress,
                     balanceToReturn: Long = 0,
                     assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    makeTx(boxesToSpend, emptyProverResult, balanceToReturn, addressToSpend.script, assets)
  }

  def makeTx(boxesToSpend: Seq[ErgoBox],
             proofToSpend: ProverResult,
             balanceToReturn: Long,
             scriptToReturn: Value[SBoolean.type],
             assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = boxesToSpend.map(box => Input(box.id, proofToSpend))
    val balanceToSpend = boxesToSpend.map(_.value).sum - balanceToReturn
    def creatingCandidate = new ErgoBoxCandidate(balanceToReturn, scriptToReturn, startHeight, replaceNewAssetStub(assets, inputs))
    val spendingOutput = if (balanceToSpend > 0) Some(new ErgoBoxCandidate(balanceToSpend, TrueLeaf, creationHeight = startHeight)) else None
    val creatingOutput = if (balanceToReturn > 0) Some(creatingCandidate) else None
    ErgoTransaction(inputs.toIndexedSeq, spendingOutput.toIndexedSeq ++ creatingOutput.toIndexedSeq)
  }

  private def replaceNewAssetStub(assets: Seq[(TokenId, Long)], inputs: Seq[Input]): Seq[(TokenId, Long)] = {
    def isNewAsset(tokenId: TokenId, value: Long): Boolean =  java.util.Arrays.equals(tokenId, newAssetIdStub)
    val (newAsset, spentAssets) = assets.partition((isNewAsset _).tupled)
    newAsset.map(Digest32 @@ inputs.head.boxId -> _._2) ++ spentAssets
  }

  def randomNewAsset: Seq[(TokenId, Long)] = Seq(newAssetIdStub -> assetGen.sample.value._2)
  def assetsWithRandom(boxes: Seq[ErgoBox]): Seq[(TokenId, Long)] = randomNewAsset ++ assetsByTokenId(boxes)
  def badAssets: Seq[(TokenId, Long)] = additionalTokensGen.sample.value
}
