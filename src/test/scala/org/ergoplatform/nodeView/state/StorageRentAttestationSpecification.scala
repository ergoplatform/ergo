package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.settings.ValidationRules.bsStorageRentAttestation
import org.ergoplatform.settings.{ErgoValidationSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import org.ergoplatform.validation.ValidationResult
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import sigma.ast.{ByteConstant, ErgoTree, IntConstant, ShortConstant, SigmaPropConstant}
import sigma.data.{ProveDlog, TrivialProp}
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.util.{Failure, Success}

/**
  * Rule `bsStorageRentAttestation` (308), EIP-0052 on miner attestation of rent-claim transactions: from block
  * version 5 on, let C be the ids, in block order, of the transactions of a block with at least one storage rent
  * claim input at the block's height. If C is non-empty, the block extension carries exactly one field with key
  * 0x0300 and value Blake2b256 of the concatenated ids of C; if C is empty, it carries no field with that key.
  * The position of a claim in the block is irrelevant.
  *
  * Most cases drive `ErgoState.execTransactions`, the block transactions validation shared by `UtxoState`
  * and `DigestState`, with a state context at the chosen height and block version (built with
  * `ErgoStateContextHelpers.stateContext`, which sets `Parameters.BlockVersion`). The cases marked
  * "end to end" apply a full block, with its extension, to a `UtxoState` via `applyModifier`.
  *
  * The expected field is built by `StorageRentTestHelpers.attesting`, which writes the key and the digest out as
  * the EIP specifies them, independently of `Extension.storageRentClaimsKey` / `storageRentClaimsDigest`.
  */
class StorageRentAttestationSpecification extends ErgoCorePropertyTest with StorageRentTestHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox

  /** Height of the block under test: above `StoragePeriod`, and not the first block of a voting epoch */
  private val H: Int = StoragePeriod + 1001

  private val V4: Byte = Header.Interpreter60Version

  private val RuleText = "Rent-claim transactions must be attested in the extension by the block producer"

  private def ctxAt(version: Byte,
                    vs: ErgoValidationSettings = ErgoValidationSettings.initial): ErgoStateContext =
    stateContext(H, version, rentSettings, vs)

  /** The same context (same headers, so the same miner key) at another block version */
  private def withVersion(ctx: ErgoStateContext, version: Byte): ErgoStateContext = {
    val p = ctx.currentParameters
    val params = Parameters(p.height, p.parametersTable.updated(Parameters.BlockVersion, version), p.proposedUpdate)
    new ErgoStateContext(ctx.lastHeaders.map(_.copy(version = version)), ctx.lastExtensionOpt, ctx.genesisStateDigest,
      params, ctx.validationSettings, ctx.votingData)(ctx.chainSettings)
  }

  /** Miner key of the block the context was built for, as used by the emission contract */
  private def minerOf(ctx: ErgoStateContext): ProveDlog = ProveDlog(ctx.lastHeaderOpt.get.powSolution.pk)

  private def exec(txs: Seq[ErgoTransaction],
                   boxes: Seq[ErgoBox],
                   ctx: ErgoStateContext,
                   ext: Extension = noExtension): ValidationResult[Long] =
    ErgoState.execTransactions(txs, ctx, rentSettings.nodeSettings, ext) { id =>
      boxes.find(b => java.util.Arrays.equals(b.id, id)) match {
        case Some(b) => Success(b)
        case None => Failure(new Exception("box not found"))
      }
    }

  private def rejectedBy308(r: ValidationResult[_]): Boolean =
    !r.isValid && r.errors.exists(_.message.contains(RuleText))

  /** Spends a `true`-script `box` with an empty proof and no context variables, recreating it at `height` */
  private def plainSpend(box: ErgoBox, height: Int): ErgoTransaction =
    ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)), IndexedSeq(recreated(box, height)))

  // end-to-end helpers

  /** UTXO state at height H - 1 holding the genesis emission box and `boxes`, at the given block version */
  private def stateBelowH(version: Byte, boxes: Seq[ErgoBox]): UtxoState = {
    val ctx = stateContext(H - 1, version, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(genesisEmissionBox +: boxes, Some(genesisEmissionBox), ctx, rentSettings)
    us.stateContext.currentHeight shouldBe H - 1
    us.stateContext.blockVersion shouldBe version
    us
  }

  /**
    * Full block at height H with `txs` and extension fields `ext`, correct state root and AD proofs, mined by
    * `defaultMinerPk`. The header commits to the extension (extensionRoot = digest of `ext`), as for a mined block.
    */
  private def blockAtH(us: UtxoState, version: Byte, txs: Seq[ErgoTransaction], ext: ExtensionCandidate): ErgoFullBlock = {
    val parent = us.stateContext.lastHeaderOpt.get
    val (adProofBytes, digest) = us.proofsForTransactions(txs).get
    val fb = powScheme.proveBlock(Some(parent), version, rentSettings.chainSettings.initialNBits, digest, adProofBytes,
      txs, parent.timestamp + 1, ext, Array.fill(3)(0: Byte), defaultMinerSecretNumber).get
    fb.header.height shouldBe H
    fb.header.votingStarts(rentSettings.chainSettings.voting.votingLength) shouldBe false
    java.util.Arrays.equals(fb.header.extensionRoot, fb.extension.digest) shouldBe true
    fb.extension.fields.map(_._1.toSeq) shouldBe ext.fields.map(_._1.toSeq)
    fb
  }

  private def applyAtH(version: Byte,
                       boxes: Seq[ErgoBox],
                       txs: UtxoState => Seq[ErgoTransaction],
                       ext: Seq[ErgoTransaction] => ExtensionCandidate): (Seq[ErgoTransaction], scala.util.Try[UtxoState]) = {
    val us = stateBelowH(version, boxes)
    val blockTxs = txs(us)
    blockTxs -> us.applyModifier(blockAtH(us, version, blockTxs, ext(blockTxs)), None)(_ => ())
  }

  private def emissionE2E(us: UtxoState): ErgoTransaction = emissionTxAt(H, defaultMinerPk, us.stateContext)

  property("predicateBoundary.age: at block version 5, an input exactly StoragePeriod old makes its transaction a " +
    "rent claim which must be attested, while one block younger is not a claim and must not be attested") {
    val ctx = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)

    // `true` scripts, recreated unchanged: both transactions are valid whichever branch the interpreter takes
    val young = boxAt(TrueTree, H - (StoragePeriod - 1), seed = 1)
    val old = boxAt(TrueTree, H - StoragePeriod, seed = 2)
    val youngTx = rentShapedTx(young, H)
    val oldTx = rentShapedTx(old, H)

    ErgoTransaction.hasStorageRentClaim(youngTx, IndexedSeq(young), H) shouldBe false
    ErgoTransaction.hasStorageRentClaim(oldTx, IndexedSeq(old), H) shouldBe true

    val youngBoxes = Seq(genesisEmissionBox, young)
    exec(Seq(emission, youngTx), youngBoxes, ctx, noExtension).isValid shouldBe true
    rejectedBy308(exec(Seq(emission, youngTx), youngBoxes, ctx, attesting(youngTx))) shouldBe true

    val oldBoxes = Seq(genesisEmissionBox, old)
    rejectedBy308(exec(Seq(emission, oldTx), oldBoxes, ctx, noExtension)) shouldBe true
    exec(Seq(emission, oldTx), oldBoxes, ctx, attesting(oldTx)).isValid shouldBe true
  }

  property("predicateBoundary.typeAndRange: at block version 5, an expired box with a true script spent with " +
    "variable 127 not a Short, or a Short out of range, is not a rent claim: accepted without the field, rejected " +
    "with it (the box's script is evaluated)") {
    val ctx = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val expired = boxAt(TrueTree, H - StoragePeriod - 10, seed = 3)
    val boxes = Seq(genesisEmissionBox, expired)

    // Control: variable 127 = Short 0 makes it a rent claim, which needs the field
    val claim = rentShapedTx(expired, H)
    rejectedBy308(exec(Seq(emission, claim), boxes, ctx, noExtension)) shouldBe true
    exec(Seq(emission, claim), boxes, ctx, attesting(claim)).isValid shouldBe true

    // The transaction has one output, so Short 1 and Short -1 are out of range
    val notClaims = Seq(IntConstant(0), ByteConstant(0), ShortConstant(1), ShortConstant(-1))
    notClaims.foreach { v =>
      val tx = rentShapedTx(expired, H, v)
      withClue(s"variable 127 = $v: ") {
        ErgoTransaction.hasStorageRentClaim(tx, IndexedSeq(expired), H) shouldBe false
        exec(Seq(emission, tx), boxes, ctx, noExtension).isValid shouldBe true
        rejectedBy308(exec(Seq(emission, tx), boxes, ctx, attesting(tx))) shouldBe true
      }
    }

    // The same shapes spending a `false`-script box fail on the script (not on rule 308): the box's own
    // script is what gets evaluated for them
    val expiredFalse = boxAt(FalseTree, H - StoragePeriod - 10, seed = 4)
    notClaims.foreach { v =>
      val res = exec(Seq(emission, rentShapedTx(expiredFalse, H, v)), Seq(genesisEmissionBox, expiredFalse), ctx)
      withClue(s"variable 127 = $v: ") {
        res.isValid shouldBe false
        rejectedBy308(res) shouldBe false
      }
    }
  }

  property("predicateBoundary.isStorageRentClaim: predicate conditions, never throws") {
    RentAttestationVersion shouldBe Header.StorageRentAttestationVersion
    val box = boxAt(FalseTree, H - StoragePeriod, seed = 9)
    val tx = rentShapedTx(box, H)
    val in = tx.inputs.head

    ErgoTransaction.isStorageRentClaim(in, box, tx, H) shouldBe true
    ErgoTransaction.isStorageRentClaim(in, box, tx, H - 1) shouldBe false

    // non-empty proof: a signed spend
    val signed = in.copy(spendingProof = ProverResult(Array.fill(56)(1: Byte), in.spendingProof.extension))
    ErgoTransaction.isStorageRentClaim(signed, box, tx, H) shouldBe false

    // variable 127 absent
    val noVar = in.copy(spendingProof = ProverResult(Array.emptyByteArray, ContextExtension.empty))
    ErgoTransaction.isStorageRentClaim(noVar, box, tx, H) shouldBe false

    // wrong type or out of range, including a Short beyond the outputs and the Short extremes
    Seq(IntConstant(0), ByteConstant(0), ShortConstant(1), ShortConstant(-1), ShortConstant(Short.MaxValue),
      ShortConstant(Short.MinValue)).foreach { v =>
      val t = rentShapedTx(box, H, v)
      ErgoTransaction.isStorageRentClaim(t.inputs.head, box, t, H) shouldBe false
    }
  }

  property("attestedClaimAnyPosition: at block version 5, a block with rent claims at transaction indices 1 and 3 " +
    "and the field over their ids in block order is accepted, through execTransactions and end to end") {
    // `false` scripts: only the rent branch can accept the spends
    val expiredA = boxAt(FalseTree, H - StoragePeriod, seed = 11)
    val expiredB = boxAt(FalseTree, H - StoragePeriod - 100, seed = 12)
    val plain = boxAt(TrueTree, H - 10, seed = 13)
    val claimA = rentShapedTx(expiredA, H)
    val claimB = rentShapedTx(expiredB, H)
    val plainTx = plainSpend(plain, H)

    // the production helpers agree with the EIP definition written out in the test helpers
    java.util.Arrays.equals(Extension.storageRentClaimsKey, AttestationKey) shouldBe true
    java.util.Arrays.equals(Extension.storageRentClaimsDigest(Seq(claimA.id, claimB.id)),
      attestationDigest(claimA, claimB)) shouldBe true
    attestationDigest(claimA, claimB).length shouldBe 32

    val ctx = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val boxes = Seq(genesisEmissionBox, expiredA, expiredB, plain)
    val txs = Seq(emission, claimA, plainTx, claimB)
    txs.indexOf(claimA) shouldBe 1
    txs.indexOf(claimB) shouldBe 3
    exec(txs, boxes, ctx, attesting(claimA, claimB)).isValid shouldBe true

    // a claim at index 0 (and the emission transaction after it) is attested the same way
    exec(Seq(claimA, emission, plainTx, claimB), boxes, ctx, attesting(claimA, claimB)).isValid shouldBe true

    // the attestation field coexists with other extension fields (here an interlinks-space field)
    val withOther = extensionWith(Array[Byte](0x01, 0x00) -> Array.fill(33)(7: Byte), AttestationKey -> attestationDigest(claimA, claimB))
    exec(txs, boxes, ctx, withOther).isValid shouldBe true

    // end to end: full block applied to a UTXO state, with a real emission transaction at index 0
    val (blockTxs, applied) = applyAtH(RentAttestationVersion, Seq(expiredA, expiredB, plain),
      us => Seq(emissionE2E(us), claimA, plainTx, claimB),
      _ => ExtensionCandidate(Seq(AttestationKey -> attestationDigest(claimA, claimB))))
    applied shouldBe a[Success[_]]
    applied.get.boxById(claimA.outputs.head.id).isDefined shouldBe true
    applied.get.boxById(claimB.outputs.head.id).isDefined shouldBe true
    applied.get.boxById(expiredA.id) shouldBe None
    applied.get.emissionBoxOpt.map(_.id).map(scorex.util.bytesToId) shouldBe
      Some(scorex.util.bytesToId(blockTxs.head.outputs.head.id))
  }

  property("attestedClaimDigestState: a DigestState checks rule 308 against the block's own extension: the " +
    "block of attestedClaimAnyPosition is accepted with the field and rejected by rule 308 without it") {
    val expiredA = boxAt(FalseTree, H - StoragePeriod, seed = 21)
    val expiredB = boxAt(FalseTree, H - StoragePeriod - 100, seed = 22)
    val plain = boxAt(TrueTree, H - 10, seed = 23)
    val claimA = rentShapedTx(expiredA, H)
    val claimB = rentShapedTx(expiredB, H)
    val plainTx = plainSpend(plain, H)
    val blockBoxes = Seq(expiredA, expiredB, plain)

    /**
      * Digest state at height H - 1 with the same stored context and root digest as `us` (the construction of
      * `DigestState.recover`: the context under `ErgoStateReader.ContextKey`, the root hash under the version),
      * and the full block at H built from `us` as in `applyAtH`. The context is the one `us` was built with
      * (`stateBelowH`): `stateContext` samples a fresh last header on every call, and the block's parent is
      * this one.
      */
    def digestStateAndBlock(ext: ExtensionCandidate): (DigestState, ErgoFullBlock) = {
      val us = stateBelowH(RentAttestationVersion, blockBoxes)
      val fb = blockAtH(us, RentAttestationVersion, Seq(emissionE2E(us), claimA, plainTx, claimB), ext)
      fb.adProofs shouldBe defined
      val ds = DigestState.recover(us.version, us.rootDigest, us.stateContext, createTempDir, rentSettings).get
      ds.stateContext.currentHeight shouldBe H - 1
      ds.stateContext.blockVersion shouldBe RentAttestationVersion
      java.util.Arrays.equals(ds.rootDigest, us.rootDigest) shouldBe true
      ds -> fb
    }

    val (dsAttested, attested) = digestStateAndBlock(
      ExtensionCandidate(Seq(AttestationKey -> attestationDigest(claimA, claimB))))
    val applied = dsAttested.applyModifier(attested, None)(_ => ())
    applied shouldBe a[Success[_]]
    java.util.Arrays.equals(applied.get.rootDigest, attested.header.stateRoot) shouldBe true
    applied.get.stateContext.currentHeight shouldBe H

    val (dsUnattested, unattested) = digestStateAndBlock(emptyExtension)
    dsUnattested.applyModifier(unattested, None)(_ => ()) match {
      case Failure(e) => e.getMessage should include(RuleText)
      case Success(_) => fail("digest state applied a block with unattested rent claims at block version 5")
    }
  }

  property("unattestedClaimRejectedV5AcceptedV4: the same block without the field is rejected by rule 308 at " +
    "block version 5 and accepted at block version 4") {
    val expiredA = boxAt(FalseTree, H - StoragePeriod, seed = 14)
    val expiredB = boxAt(FalseTree, H - StoragePeriod - 100, seed = 15)
    val plain = boxAt(TrueTree, H - 10, seed = 16)
    val claimA = rentShapedTx(expiredA, H)
    val claimB = rentShapedTx(expiredB, H)
    val plainTx = plainSpend(plain, H)

    // identical transactions, height and miner; only the block version differs
    val ctx5 = ctxAt(RentAttestationVersion)
    val ctx4 = withVersion(ctx5, V4)
    ctx5.blockVersion shouldBe RentAttestationVersion
    ctx4.blockVersion shouldBe V4
    val emission = emissionTxAt(H, minerOf(ctx5), ctx5)

    val txs = Seq(emission, claimA, plainTx, claimB)
    val boxes = Seq(genesisEmissionBox, expiredA, expiredB, plain)
    rejectedBy308(exec(txs, boxes, ctx5, noExtension)) shouldBe true
    exec(txs, boxes, ctx4, noExtension).isValid shouldBe true
    // a single claim, alone in its block, the same
    rejectedBy308(exec(Seq(claimA), Seq(expiredA), ctx5, noExtension)) shouldBe true
    exec(Seq(claimA), Seq(expiredA), ctx4, noExtension).isValid shouldBe true

    // end to end: the same transactions in a full block with an empty extension applied to a UTXO state
    val blockBoxes = Seq(expiredA, expiredB, plain)
    val blockTxs = (us: UtxoState) => Seq(emissionE2E(us), claimA, plainTx, claimB)
    applyAtH(RentAttestationVersion, blockBoxes, blockTxs, _ => emptyExtension)._2 match {
      case Failure(e) => e.getMessage should include(RuleText)
      case Success(_) => fail("block with unattested rent claims applied at block version 5")
    }
    applyAtH(V4, blockBoxes, blockTxs, _ => emptyExtension)._2 shouldBe a[Success[_]]
  }

  property("wrongDigestRejected: at block version 5, a field over the claim ids in the wrong order, over one of two " +
    "claims, over the claims and another transaction, truncated, or present twice is rejected") {
    val expiredA = boxAt(FalseTree, H - StoragePeriod, seed = 17)
    val expiredB = boxAt(FalseTree, H - StoragePeriod - 100, seed = 18)
    val plain = boxAt(TrueTree, H - 10, seed = 19)
    val claimA = rentShapedTx(expiredA, H)
    val claimB = rentShapedTx(expiredB, H)
    val plainTx = plainSpend(plain, H)

    val ctx = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val txs = Seq(emission, claimA, plainTx, claimB)
    val boxes = Seq(genesisEmissionBox, expiredA, expiredB, plain)

    // control
    exec(txs, boxes, ctx, attesting(claimA, claimB)).isValid shouldBe true

    val correct = attestationDigest(claimA, claimB)
    val wrong: Seq[(String, Extension)] = Seq(
      "wrong order" -> attesting(claimB, claimA),
      "only the first claim" -> attesting(claimA),
      "only the second claim" -> attesting(claimB),
      "claims and a non-claim transaction" -> attesting(claimA, plainTx, claimB),
      "truncated value" -> extensionWith(AttestationKey -> correct.take(31)),
      "field present twice" -> extensionWith(AttestationKey -> correct, AttestationKey -> correct),
      "correct value under another key of the key space" -> extensionWith(Array[Byte](0x03, 0x01) -> correct)
    )
    wrong.foreach { case (what, ext) =>
      withClue(s"$what: ") {
        rejectedBy308(exec(txs, boxes, ctx, ext)) shouldBe true
      }
    }

    // end to end: the wrong order
    applyAtH(RentAttestationVersion, Seq(expiredA, expiredB, plain),
      us => Seq(emissionE2E(us), claimA, plainTx, claimB),
      _ => ExtensionCandidate(Seq(AttestationKey -> attestationDigest(claimB, claimA))))._2 match {
      case Failure(e) => e.getMessage should include(RuleText)
      case Success(_) => fail("block with a wrong attestation digest applied at block version 5")
    }
  }

  property("spuriousFieldRejected: a block with no rent claims but the field present is rejected by rule 308 at " +
    "block version 5 and accepted at block version 4") {
    val plain = boxAt(TrueTree, H - 10, seed = 20)
    val plainTx = plainSpend(plain, H)

    val ctx5 = ctxAt(RentAttestationVersion)
    val ctx4 = withVersion(ctx5, V4)
    val emission = emissionTxAt(H, minerOf(ctx5), ctx5)
    val txs = Seq(emission, plainTx)
    val boxes = Seq(genesisEmissionBox, plain)

    // control: no claims, no field
    exec(txs, boxes, ctx5, noExtension).isValid shouldBe true

    val spurious: Seq[(String, Extension)] = Seq(
      "field over a non-claim transaction" -> attesting(plainTx),
      "field over no transactions" -> attesting(),
      "field with arbitrary value" -> extensionWith(AttestationKey -> Array.fill(32)(1: Byte))
    )
    spurious.foreach { case (what, ext) =>
      withClue(s"$what: ") {
        rejectedBy308(exec(txs, boxes, ctx5, ext)) shouldBe true
        exec(txs, boxes, ctx4, ext).isValid shouldBe true
      }
    }

    // end to end
    val blockTxs = (us: UtxoState) => Seq(emissionE2E(us), plainTx)
    val spuriousExt = (btxs: Seq[ErgoTransaction]) => ExtensionCandidate(Seq(AttestationKey -> attestationDigest(btxs: _*)))
    applyAtH(RentAttestationVersion, Seq(plain), blockTxs, spuriousExt)._2 match {
      case Failure(e) => e.getMessage should include(RuleText)
      case Success(_) => fail("block with a spurious attestation field applied at block version 5")
    }
    applyAtH(V4, Seq(plain), blockTxs, spuriousExt)._2 shouldBe a[Success[_]]
  }

  property("thirdPartyClaimAttested: at block version 5, a fee-paying rent claim listed in the field by the block " +
    "producer is accepted (the rule is decided by the producer, not restricted to the producer's own claims)") {
    val ctx = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val expired = boxAt(FalseTree, H - StoragePeriod, seed = 6)

    // The claimer takes the storage fee, pays part of it as a transaction fee and keeps the rest
    val storageFee = parameters.storageFeeFactor.toLong * expired.bytes.length
    val txFee = storageFee / 2
    val outs = IndexedSeq(
      new ErgoBoxCandidate(expired.value - storageFee, expired.ergoTree, H,
        expired.additionalTokens, expired.additionalRegisters),
      new ErgoBoxCandidate(txFee, feeProp, H),
      new ErgoBoxCandidate(storageFee - txFee, TrueTree, H)
    )
    val claim = ErgoTransaction(IndexedSeq(rentInput(expired)), outs)

    ErgoTransaction.hasStorageRentClaim(claim, IndexedSeq(expired), H) shouldBe true
    exec(Seq(emission, claim), Seq(genesisEmissionBox, expired), ctx, attesting(claim)).isValid shouldBe true
    exec(Seq(claim), Seq(expired), ctx, attesting(claim)).isValid shouldBe true
    // control: the same claim, not attested
    rejectedBy308(exec(Seq(emission, claim), Seq(genesisEmissionBox, expired), ctx, noExtension)) shouldBe true
  }

  property("disabledRuleAccepts: with rule 308 disabled via validation settings, an unattested rent claim and a " +
    "spurious field are accepted at block version 5") {
    val expired = boxAt(FalseTree, H - StoragePeriod, seed = 8)
    val plain = boxAt(TrueTree, H - 10, seed = 7)
    val claim = rentShapedTx(expired, H)
    val plainTx = plainSpend(plain, H)

    // The soft-fork rule deactivation travels in the block extension; the disabled rule has to parse back,
    // which requires mayBeDisabled = true
    val disabled = ErgoValidationSettings.initial.updated(ErgoValidationSettingsUpdate(Seq(bsStorageRentAttestation), Seq()))
    val parsed = ErgoValidationSettings.parseExtension(disabled.toExtensionCandidate).get
    parsed shouldBe disabled
    parsed.isActive(bsStorageRentAttestation) shouldBe false

    val ctxDisabled = ctxAt(RentAttestationVersion, parsed)
    val ctxEnabled = ctxAt(RentAttestationVersion)
    val emission = emissionTxAt(H, minerOf(ctxDisabled), ctxDisabled)
    val emissionEnabled = emissionTxAt(H, minerOf(ctxEnabled), ctxEnabled)
    val boxes = Seq(genesisEmissionBox, expired, plain)

    exec(Seq(emission, claim), boxes, ctxDisabled, noExtension).isValid shouldBe true
    exec(Seq(emission, plainTx), boxes, ctxDisabled, attesting(plainTx)).isValid shouldBe true
    // control: the same shapes with the rule active
    rejectedBy308(exec(Seq(emissionEnabled, claim), boxes, ctxEnabled, noExtension)) shouldBe true
    rejectedBy308(exec(Seq(emissionEnabled, plainTx), boxes, ctxEnabled, attesting(plainTx))) shouldBe true
  }

  // Documents sigma 6.0.6 soft-fork passthrough at activated script version 4; the EIP requires block version 5
  // to ship with an interpreter supporting script version 4, after which this test must be inverted.
  //
  // At block version 5, `ErgoContext` sets activatedScriptVersion = 4, above the interpreter's
  // MaxSupportedScriptVersion (3), and `Interpreter.checkSoftForkCondition` then treats an ErgoTree of
  // version 4 as passing without evaluating it. The box below is not a rent claim (not expired, no variable 127).
  property("v4TreePassthroughAtV5: a box with a version-4 ErgoTree and proposition sigmaProp(false), spent with an " +
    "empty proof, is rejected at block version 4 and (currently) accepted at block version 5") {
    val v4False = ErgoTree.withoutSegregation(ErgoTree.headerWithVersion(ErgoTree.ZeroHeader, 4),
      SigmaPropConstant(TrivialProp.FalseProp))
    v4False.version shouldBe 4

    val box = boxAt(v4False, H - 10, seed = 10)
    val spend = ErgoTransaction(IndexedSeq(Input(box.id, ProverResult.empty)),
      IndexedSeq(new ErgoBoxCandidate(box.value, TrueTree, H)))
    ErgoTransaction.hasStorageRentClaim(spend, IndexedSeq(box), H) shouldBe false

    val ctx5 = ctxAt(RentAttestationVersion)
    val ctx4 = withVersion(ctx5, V4)

    // transaction-level (as in the probe the EIP cites)
    spend.statefulValidity(IndexedSeq(box), IndexedSeq.empty, ctx4)(ErgoInterpreter(ctx4.currentParameters)) match {
      case Failure(e) => e.getMessage should include("ErgoTree version 4 is higher than activated 3")
      case Success(_) => fail("v4-tree false-script box spent with an empty proof accepted at block version 4")
    }
    spend.statefulValidity(IndexedSeq(box), IndexedSeq.empty, ctx5)(ErgoInterpreter(ctx5.currentParameters)) shouldBe
      a[Success[_]]

    // block-level, through the same validation as `UtxoState` and `DigestState`
    exec(Seq(spend), Seq(box), ctx4).isValid shouldBe false
    exec(Seq(spend), Seq(box), ctx5).isValid shouldBe true
  }

}
