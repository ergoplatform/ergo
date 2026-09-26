package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.settings.ValidationRules.bsStorageRentPosition
import org.ergoplatform.settings.{ErgoValidationSettings, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import org.ergoplatform.validation.ValidationResult
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import sigma.ast.{ByteConstant, IntConstant, ShortConstant}
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.util.{Failure, Success}

/**
  * Rule `bsStorageRentPosition` (308), EIP draft "Storage Rent Claims Restricted to the First Transaction of
  * a Block": from block version 5 on, a storage rent claim is valid only in the first transaction of a block.
  *
  * Most cases drive `ErgoState.execTransactions`, the block transactions validation shared by `UtxoState`
  * and `DigestState`, with a state context at the chosen height and block version (built with
  * `ErgoStateContextHelpers.stateContext`, which sets `Parameters.BlockVersion`). The cases marked
  * "end to end" apply a full block to a `UtxoState` via `applyModifier`.
  */
class StorageRentPositionSpecification extends ErgoCorePropertyTest with StorageRentTestHelpers {

  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants.genesisEmissionBox

  /** Height of the block under test: above `StoragePeriod`, and not the first block of a voting epoch */
  private val H: Int = StoragePeriod + 1001

  private val V4: Byte = Header.Interpreter60Version

  private val RuleText = "Storage rent claims are allowed only in the first transaction of a block"

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

  private def exec(txs: Seq[ErgoTransaction], boxes: Seq[ErgoBox], ctx: ErgoStateContext): ValidationResult[Long] =
    ErgoState.execTransactions(txs, ctx, rentSettings.nodeSettings) { id =>
      boxes.find(b => java.util.Arrays.equals(b.id, id)) match {
        case Some(b) => Success(b)
        case None => Failure(new Exception("box not found"))
      }
    }

  private def rejectedBy308(r: ValidationResult[_]): Boolean =
    !r.isValid && r.errors.exists(_.message.contains(RuleText))

  // end-to-end helpers

  /** UTXO state at height H - 1 holding the genesis emission box and `boxes`, at the given block version */
  private def stateBelowH(version: Byte, boxes: Seq[ErgoBox]): UtxoState = {
    val ctx = stateContext(H - 1, version, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(genesisEmissionBox +: boxes, Some(genesisEmissionBox), ctx, rentSettings)
    us.stateContext.currentHeight shouldBe H - 1
    us.stateContext.blockVersion shouldBe version
    us
  }

  /** Full block at height H with `txs`, correct state root and AD proofs, mined by `defaultMinerPk` */
  private def blockAtH(us: UtxoState, version: Byte, txs: Seq[ErgoTransaction]): ErgoFullBlock = {
    val parent = us.stateContext.lastHeaderOpt.get
    val (adProofBytes, digest) = us.proofsForTransactions(txs).get
    val fb = powScheme.proveBlock(Some(parent), version, rentSettings.chainSettings.initialNBits, digest, adProofBytes,
      txs, parent.timestamp + 1, emptyExtension, Array.fill(3)(0: Byte), defaultMinerSecretNumber).get
    fb.header.height shouldBe H
    fb.header.votingStarts(rentSettings.chainSettings.voting.votingLength) shouldBe false
    fb
  }

  property("t1AgeBoundary: at block version 5, a rent-claim-shaped input at t_1 is accepted one block before " +
    "StoragePeriod and rejected by rule 308 at StoragePeriod") {
    val ctx = ctxAt(RentPositionVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)

    // `true` scripts, recreated unchanged: both transactions are valid whichever branch the interpreter takes
    val young = boxAt(TrueTree, H - (StoragePeriod - 1), seed = 1)
    val old = boxAt(TrueTree, H - StoragePeriod, seed = 2)
    val youngTx = rentShapedTx(young, H)
    val oldTx = rentShapedTx(old, H)

    ErgoTransaction.hasStorageRentClaim(youngTx, IndexedSeq(young), H) shouldBe false
    ErgoTransaction.hasStorageRentClaim(oldTx, IndexedSeq(old), H) shouldBe true

    exec(Seq(emission, youngTx), Seq(genesisEmissionBox, young), ctx).isValid shouldBe true

    val res = exec(Seq(emission, oldTx), Seq(genesisEmissionBox, old), ctx)
    rejectedBy308(res) shouldBe true
  }

  property("t1WrongTypeOrRange: at block version 5, an expired box with a true script at t_1 with variable 127 " +
    "not a Short, or a Short out of range, is accepted (the box's script is evaluated)") {
    val ctx = ctxAt(RentPositionVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val expired = boxAt(TrueTree, H - StoragePeriod - 10, seed = 3)

    // Control: variable 127 = Short 0 makes it a rent claim, rejected at t_1
    rejectedBy308(exec(Seq(emission, rentShapedTx(expired, H)), Seq(genesisEmissionBox, expired), ctx)) shouldBe true

    // The transaction has one output, so Short 1 and Short -1 are out of range
    val notClaims = Seq(IntConstant(0), ByteConstant(0), ShortConstant(1), ShortConstant(-1))
    notClaims.foreach { v =>
      val tx = rentShapedTx(expired, H, v)
      withClue(s"variable 127 = $v: ") {
        ErgoTransaction.hasStorageRentClaim(tx, IndexedSeq(expired), H) shouldBe false
        exec(Seq(emission, tx), Seq(genesisEmissionBox, expired), ctx).isValid shouldBe true
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

  property("t0ClaimEmissionAtT1: at block version 5, a block with a rent claim at t_0 and the emission " +
    "transaction at t_1 is accepted") {
    // `false` script: only the rent branch can accept the spend
    val expired = boxAt(FalseTree, H - StoragePeriod, seed = 5)

    val ctx = ctxAt(RentPositionVersion)
    val emission = emissionTxAt(H, minerOf(ctx), ctx)
    val claim = rentShapedTx(expired, H)
    exec(Seq(claim, emission), Seq(genesisEmissionBox, expired), ctx).isValid shouldBe true

    // end to end: full block applied to a UTXO state; the node finds the emission transaction at t_1
    val us = stateBelowH(RentPositionVersion, Seq(expired))
    val emissionE2E = emissionTxAt(H, defaultMinerPk, us.stateContext)
    val fb = blockAtH(us, RentPositionVersion, Seq(claim, emissionE2E))
    val applied = us.applyModifier(fb, None)(_ => ())
    applied shouldBe a[Success[_]]
    applied.get.emissionBoxOpt.map(_.id).map(scorex.util.bytesToId) shouldBe
      Some(scorex.util.bytesToId(emissionE2E.outputs.head.id))
    applied.get.boxById(claim.outputs.head.id).isDefined shouldBe true
  }

  property("t0ThirdPartyClaim: at block version 5, a fee-paying rent claim at t_0 is accepted") {
    val ctx = ctxAt(RentPositionVersion)
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
    exec(Seq(claim, emission), Seq(genesisEmissionBox, expired), ctx).isValid shouldBe true
    exec(Seq(claim), Seq(expired), ctx).isValid shouldBe true
  }

  property("t1ClaimRejectedV5AcceptedV4: the same block with a rent claim at t_1 is rejected at block " +
    "version 5 and accepted at block version 4") {
    val expired = boxAt(FalseTree, H - StoragePeriod, seed = 7)
    val claim = rentShapedTx(expired, H)

    // identical transactions, height and miner; only the block version differs
    val ctx5 = ctxAt(RentPositionVersion)
    val ctx4 = withVersion(ctx5, V4)
    ctx5.blockVersion shouldBe RentPositionVersion
    ctx4.blockVersion shouldBe V4
    val emission = emissionTxAt(H, minerOf(ctx5), ctx5)

    val txs = Seq(emission, claim)
    val boxes = Seq(genesisEmissionBox, expired)
    rejectedBy308(exec(txs, boxes, ctx5)) shouldBe true
    exec(txs, boxes, ctx4).isValid shouldBe true

    // end to end: the same transactions in a full block applied to a UTXO state
    val us5 = stateBelowH(RentPositionVersion, Seq(expired))
    val us4 = stateBelowH(V4, Seq(expired))
    val txsE2E = Seq(emissionTxAt(H, defaultMinerPk, us5.stateContext), claim)

    us5.applyModifier(blockAtH(us5, RentPositionVersion, txsE2E), None)(_ => ()) match {
      case Failure(e) => e.getMessage should include(RuleText)
      case Success(_) => fail("block with a rent claim at t_1 applied at block version 5")
    }
    us4.applyModifier(blockAtH(us4, V4, txsE2E), None)(_ => ()) shouldBe a[Success[_]]
  }

  property("disabledRuleAccepts: with rule 308 disabled via validation settings, a rent claim at t_1 is " +
    "accepted at block version 5") {
    val expired = boxAt(FalseTree, H - StoragePeriod, seed = 8)
    val claim = rentShapedTx(expired, H)

    // The soft-fork rule deactivation travels in the block extension; the disabled rule has to parse back,
    // which requires mayBeDisabled = true
    val disabled = ErgoValidationSettings.initial.updated(ErgoValidationSettingsUpdate(Seq(bsStorageRentPosition), Seq()))
    val parsed = ErgoValidationSettings.parseExtension(disabled.toExtensionCandidate).get
    parsed shouldBe disabled
    parsed.isActive(bsStorageRentPosition) shouldBe false

    val ctxDisabled = ctxAt(RentPositionVersion, parsed)
    val ctxEnabled = ctxAt(RentPositionVersion)
    val emission = emissionTxAt(H, minerOf(ctxDisabled), ctxDisabled)
    val emissionEnabled = emissionTxAt(H, minerOf(ctxEnabled), ctxEnabled)
    val boxes = Seq(genesisEmissionBox, expired)

    exec(Seq(emission, claim), boxes, ctxDisabled).isValid shouldBe true
    // control: the same shape with the rule active
    rejectedBy308(exec(Seq(emissionEnabled, claim), boxes, ctxEnabled)) shouldBe true
  }

  property("isStorageRentClaim: predicate conditions, never throws") {
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

}
