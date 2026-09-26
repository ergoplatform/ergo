package org.ergoplatform.utils

import org.ergoplatform.ErgoBox.{R4, TokenId}
import org.ergoplatform.mining.CandidateGenerator
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{BoxHolder, ErgoState, ErgoStateContext, UtxoState}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.protocol.{Constants => WalletConstants}
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore
import scorex.util.bytesToId
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ErgoTree, EvaluatedValue, LongConstant, SType, ShortConstant}
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}

/**
  * Fixtures for the storage rent attestation tests (EIP-0052, miner attestation of rent-claim transactions in the
  * block extension, rule `bsStorageRentAttestation`).
  *
  * A rent claim input is built as in `ExpirationSpecification`: empty proof, and context variable #127
  * holding the index of the output which recreates the box.
  */
trait StorageRentTestHelpers extends ErgoStateContextHelpers with TestFileUtils {

  import ErgoNodeTestConstants.{genesisEmissionBox, settings}

  /** Block version from which rule `bsStorageRentAttestation` is enforced */
  val RentAttestationVersion: Byte = 5

  /**
    * Key of the attestation field, written out as the EIP specifies it (key space 0x03, index 0x00), independently
    * of `Extension.storageRentClaimsKey`
    */
  val AttestationKey: Array[Byte] = Array[Byte](0x03, 0x00)

  /**
    * Attestation value for `txs`, written out as the EIP specifies it, independently of
    * `Extension.storageRentClaimsDigest`: Blake2b256 of the concatenated 32-byte transaction ids, in the given order
    */
  def attestationDigest(txs: ErgoTransaction*): Array[Byte] =
    Blake2b256(txs.flatMap(tx => Base16.decode(tx.id).get).toArray)

  /** Header id for extensions built in these tests: block validation against the state does not check it */
  private val AnyHeaderId = bytesToId(Array.fill(32)(0: Byte))

  /** Block extension with the given fields */
  def extensionWith(fields: (Array[Byte], Array[Byte])*): Extension = Extension(AnyHeaderId, fields)

  /** Block extension with no fields */
  val noExtension: Extension = extensionWith()

  /** Block extension attesting to `claimTxs`, listed in the given order */
  def attesting(claimTxs: ErgoTransaction*): Extension = extensionWith(AttestationKey -> attestationDigest(claimTxs: _*))

  /** Values of the attestation fields of `ext` */
  def attestationValues(ext: ExtensionCandidate): Seq[Array[Byte]] =
    ext.fields.collect { case (k, v) if java.util.Arrays.equals(k, AttestationKey) => v }

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
