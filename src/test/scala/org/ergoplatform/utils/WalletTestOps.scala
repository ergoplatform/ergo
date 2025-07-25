package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.sdk.wallet.TokensMap
import org.ergoplatform.utils.fixtures.WalletFixture
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import sigma.Colls
import sigmastate.eval.Extensions._
import sigma.Extensions._
import sigma.ast.ErgoTree
import sigma.data.ProveDlog
import sigma.interpreter.ProverResult
import org.ergoplatform.settings.Constants.TrueTree

trait WalletTestOps extends NodeViewBaseOps {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  def newAssetIdStub: TokenId = Blake2b256.hash("new_asset").toTokenId

  def withFixture[T](test: WalletFixture => T): T =
    new WalletFixture(settings, parameters, getCurrentView(_).vault).apply(test)

  def wallet(implicit w: WalletFixture): ErgoWallet = w.wallet

  def getPublicKeys(implicit w: WalletFixture): Seq[P2PKAddress] =
    await(w.wallet.publicKeys(0, Int.MaxValue))

  def getConfirmedBalances(implicit w: WalletFixture): WalletDigest =
    await(w.wallet.confirmedBalances)

  def getBalancesWithUnconfirmed(implicit w: WalletFixture): WalletDigest =
    await(w.wallet.balancesWithUnconfirmed)

  def offchainScanTime(tx: ErgoTransaction): Long = tx.outputs.size * 100 + 300

  def balanceAmount(boxes: Seq[ErgoBox]): Long = boxes.map(_.value).sum

  def boxesAvailable(block: ErgoFullBlock, pk: ProveDlog): Seq[ErgoBox] =
    block.transactions.flatMap(boxesAvailable(_, pk))

  def boxesAvailable(tx: ErgoTransaction, pk: ProveDlog): Seq[ErgoBox] =
    tx.outputs.filter(_.propositionBytes.containsSlice(org.ergoplatform.mining.groupElemToBytes(pk.value)))

  def assetAmount(boxes: Seq[ErgoBoxCandidate]): Seq[(ModifierId, Long)] =
    assetsByTokenId(boxes).map { case (tokenId, sum) => (tokenId.toModifierId, sum) }.toArray[(ModifierId, Long)]

  def toAssetMap(assetSeq: Seq[(TokenId, Long)]): TokensMap =
    assetSeq.map { case (tokenId, sum) => (tokenId.toModifierId, sum) }.toMap

  def assetsByTokenId(boxes: Seq[ErgoBoxCandidate]): Map[TokenId, Long] = {
    boxes
      .flatMap(_.additionalTokens.toArray)
      .foldLeft(Map.empty[EncodedTokenId, Long]) { case (acc, (id, amt)) =>
        acc.updated(encodedTokenId(id), acc.getOrElse(encodedTokenId(id), 0L) + amt)
      }
      .map(x => decodedTokenId(x._1) -> x._2)
  }

  def getUtxoState(implicit ctx: Ctx): UtxoState = getCurrentState.asInstanceOf[UtxoState]

  def getHeightOf(state: ErgoState[_])(implicit ctx: Ctx): Option[Int] =
    getHistory.heightOf(org.ergoplatform.core.versionToId(state.version))

  def makeGenesisBlock(script: ProveDlog, assets: Seq[(TokenId, Long)] = Seq.empty)
                      (implicit ctx: Ctx): ErgoFullBlock = {
    makeNextBlock(getUtxoState, Seq(makeGenesisTx(script, assets)))
  }

  def makeGenesisTxWithAsset(publicKey: ProveDlog, issueAsset: Boolean): ErgoTransaction = {
    val inputs = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
    val assets: Seq[(TokenId, Long)] = if (issueAsset) {
      Seq(inputs.head.boxId.toTokenId -> 1L)
    } else {
      Seq.empty
    }

    CandidateGenerator.collectRewards(Some(genesisEmissionBox),
      EmptyHistoryHeight,
      Seq.empty,
      publicKey,
      emptyStateContext,
      Colls.fromArray(assets.toArray)).head
  }

  def makeGenesisTx(publicKey: ProveDlog, assetsIn: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
    val assets: Seq[(TokenId, Long)] = replaceNewAssetStub(assetsIn, inputs)
    CandidateGenerator.collectRewards(Some(genesisEmissionBox),
      EmptyHistoryHeight,
      Seq.empty,
      publicKey,
      emptyStateContext,
      Colls.fromArray(assets.toArray)).head
  }

  def makeSpendingTx(boxesToSpend: Seq[ErgoBox],
                     addressToReturn: ErgoAddress,
                     balanceToReturn: Long = 0,
                     assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    makeTx(boxesToSpend, emptyProverResult, balanceToReturn, addressToReturn.script, assets)
  }

  def makeTx(boxesToSpend: Seq[ErgoBox],
             proofToSpend: ProverResult,
             balanceToReturn: Long,
             scriptToReturn: ErgoTree,
             assets: Seq[(TokenId, Long)] = Seq.empty): ErgoTransaction = {
    val inputs = boxesToSpend.map(box => Input(box.id, proofToSpend))
    val balanceToSpend = boxesToSpend.map(_.value).sum - balanceToReturn

    def creatingCandidate = new ErgoBoxCandidate(balanceToReturn, scriptToReturn, startHeight, replaceNewAssetStub(assets, inputs).toColl)

    val spendingOutput = if (balanceToSpend > 0) Some(new ErgoBoxCandidate(balanceToSpend, TrueTree, creationHeight = startHeight)) else None
    val creatingOutput = if (balanceToReturn > 0) Some(creatingCandidate) else None
    ErgoTransaction(inputs.toIndexedSeq, spendingOutput.toIndexedSeq ++ creatingOutput.toIndexedSeq)
  }

  private def replaceNewAssetStub(assets: Seq[(TokenId, Long)], inputs: Seq[Input]): Array[(TokenId, Long)] = {
    def isNewAsset(tokenId: TokenId, value: Long): Boolean = tokenId == newAssetIdStub

    val (newAsset, spentAssets) = assets.partition((isNewAsset _).tupled)
    (newAsset.map(inputs.head.boxId.toTokenId -> _._2) ++ spentAssets).toArray
  }

  def randomNewAsset: Array[(TokenId, Long)] = Array(newAssetIdStub -> randomLong())

  def assetsWithRandom(boxes: Seq[ErgoBox]): Array[(TokenId, Long)] = randomNewAsset ++ assetsByTokenId(boxes)

  val fakeInputs: IndexedSeq[Input] = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), emptyProverResult))

}
