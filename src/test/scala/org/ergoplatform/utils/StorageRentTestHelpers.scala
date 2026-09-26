package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{R4, TokenId}
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, ErgoStateContext, UtxoState}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.protocol.{Constants => WalletConstants}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.{ErgoTree, EvaluatedValue, LongConstant, SType, ShortConstant}
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}

/**
  * Fixtures for the storage rent position tests (EIP draft "Storage Rent Claims Restricted to the First
  * Transaction of a Block", rule `bsStorageRentPosition`).
  *
  * A rent claim input is built as in `ExpirationSpecification`: empty proof, and context variable #127
  * holding the index of the output which recreates the box.
  */
trait StorageRentTestHelpers extends ErgoStateContextHelpers with TestFileUtils {

  import ErgoNodeTestConstants.{genesisEmissionBox, settings}

  /** Block version from which rule `bsStorageRentPosition` is enforced */
  val RentPositionVersion: Byte = 5

  val StoragePeriod: Int = WalletConstants.StoragePeriod

  val BoxValue: Long = 10L * 1000000000L

  /**
    * Test settings with the EIP-27 activation height moved above every height used in these tests.
    * The test chain activates EIP-27 at height 777217, below any height at which a box can be
    * `StoragePeriod` blocks old, and its genesis emission box carries no re-emission tokens, so an
    * emission transaction could not be built there otherwise. `checkReemissionRules` stays false.
    */
  val rentSettings: ErgoSettings = {
    val cs = settings.chainSettings
    settings.copy(chainSettings = cs.copy(reemission = cs.reemission.copy(activationHeight = Int.MaxValue)))
  }

  /** A box with a register (R4), so that a correct rent claim has to preserve it */
  def boxAt(tree: ErgoTree, creationHeight: Int, seed: Int, value: Long = BoxValue): ErgoBox =
    new ErgoBox(
      value,
      tree,
      Colls.emptyColl[(TokenId, Long)],
      Map(R4 -> LongConstant(seed.toLong)),
      bytesToId(Array.fill(32)(seed.toByte)),
      0.toShort,
      creationHeight
    )

  /** Input with an empty proof and context variable #127 set to `indexVar` */
  def rentInput(box: ErgoBox, indexVar: EvaluatedValue[_ <: SType] = ShortConstant(0)): Input =
    Input(box.id, ProverResult(Array.emptyByteArray,
      ContextExtension(Map(WalletConstants.StorageIndexVarId -> indexVar))))

  /** `box` recreated unchanged at `height` */
  def recreated(box: ErgoBox, height: Int): ErgoBoxCandidate =
    new ErgoBoxCandidate(box.value, box.ergoTree, height, box.additionalTokens, box.additionalRegisters)

  /**
    * Spends `box` through a rent-claim-shaped input and recreates it unchanged at `height`.
    * Such a transaction is valid under the rent branch (`checkExpiredBox`), and also under the box's own
    * script when that script is `true`, so its validity does not depend on which branch is taken.
    */
  def rentShapedTx(box: ErgoBox, height: Int, indexVar: EvaluatedValue[_ <: SType] = ShortConstant(0)): ErgoTransaction =
    ErgoTransaction(IndexedSeq(rentInput(box, indexVar)), IndexedSeq(recreated(box, height)))

  /** A real emission transaction (spending the genesis emission box) for a block at `height` mined by `minerPk` */
  def emissionTxAt(height: Int, minerPk: ProveDlog, ctx: ErgoStateContext): ErgoTransaction =
    CandidateGenerator.collectRewards(Some(genesisEmissionBox), height - 1, Seq.empty, minerPk, ctx).head

  /**
    * UTXO state holding `boxes`, with `ctx` stored as its state context. Same construction as
    * `UtxoState.fromBoxHolder`, which always stores an empty (genesis) context, while these tests need a state
    * at a height of at least `StoragePeriod`.
    */
  def utxoStateAt(boxes: Seq[ErgoBox],
                  emissionBoxOpt: Option[ErgoBox],
                  ctx: ErgoStateContext,
                  s: ErgoSettings): UtxoState = {
    val p = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
    BoxHolder(boxes).sortedBoxes.foreach { b =>
      p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).get
    }
    val store = new LDBVersionedStore(createTempDir, initialKeepVersions = s.nodeSettings.keepVersions)
    val storage = new VersionedLDBAVLStorage(store)
    val prover = PersistentBatchAVLProver.create(
      p,
      storage,
      UtxoState.metadata(ErgoState.genesisStateVersion, p.digest, emissionBoxOpt, ctx),
      paranoidChecks = true
    ).get
    new UtxoState(prover, ErgoState.genesisStateVersion, store, s)
  }

}
