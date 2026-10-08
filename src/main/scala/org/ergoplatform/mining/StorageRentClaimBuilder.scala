package org.ergoplatform.mining

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigma.{Coll, Colls}
import sigma.Extensions.CollBytesOps
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.collection.mutable.ArrayBuffer

/**
  * Miner-side storage-rent self-claim transaction builder, a port of the Rust reference node's
  * `build_rent_claim` (`ergo-mining/src/storage_rent_claim.rs`).
  *
  * A mining node may sweep storage-rent-eligible boxes (unspent for at least the storage
  * period) directly into a zero-fee transaction paying the freed rent to the miner's P2PK,
  * included into its own block candidate. The miner needs no transaction fee to incentivize
  * inclusion because it controls the block.
  *
  * Behavior, mirroring the consensus storage-rent rule:
  * - a box which can cover its storage fee is recreated: the output preserves
  *   script/tokens/registers, sits at the current height, and carries the box value minus
  *   the storage fee; the miner keeps the fee;
  * - a box which can not cover its storage fee is fully consumed (destroyed): its tokens
  *   are burned with it, except tokens from the configured whitelist, which are salvaged
  *   into the box's proceeds output;
  * - every claimed box names its own output in var-127: output i belongs to input i.
  *   Recreated boxes name their recreation, burned boxes name a P2PK output carrying the
  *   box's value. Rent fees go to a separate, unnamed P2PK output appended last.
  *
  * The builder is pure (no I/O): callers pass resolved eligible boxes and get back a
  * transaction ready for block-assembly validation. A box is silently skipped when claiming
  * it would produce an output the consensus validator rejects: box too young, box value at
  * or below the minimum allowed value, storage fee wrapping to non-positive in 32-bit
  * arithmetic (consensus-uncollectable), recreated output below the dust floor or past the
  * box size cap, or (EIP-27 networks) a box still carrying re-emission tokens, which is
  * consensus-unclaimable outright. If recreation fees do not
  * clear the dust floor, the recreations are dropped; if nothing remains, no claim is
  * produced.
  */
object StorageRentClaimBuilder extends ScorexLogging {

  /**
    * Max boxes one claim transaction may sweep. Keeps the claim's cost and size well under
    * block limits
    */
  val MaxClaims: Int = 100

  private val DummyTxId: ModifierId = bytesToId(Array.fill(32)(0.toByte))

  /**
    * Serialized length of an output box built from `candidate` at `outputIndex`
    * (candidate body + 32-byte transaction id + VLQ output index).
    */
  private def boxSize(candidate: ErgoBoxCandidate, outputIndex: Short): Int =
    candidate.toBox(DummyTxId, outputIndex).bytes.length

  private def dustLimit(candidate: ErgoBoxCandidate, outputIndex: Short, parameters: Parameters): Long =
    parameters.minValuePerByte.toLong * boxSize(candidate, outputIndex)

  /**
    * Proceeds outputs go to the miner's plain P2PK box, not
    * `ErgoTreePredef.rewardOutputScript(minerRewardDelay, minerPk)` used for emission and
    * fee outputs. The delay exists because emission/fee outputs must be locked by
    * consensus for a period (reward accounting rules). Rent proceeds have no such rule:
    * the claim is an ordinary spend of expired boxes, and the proceeds box exists only if
    * the block carrying it stays in the best chain - an orphaned block simply drops both
    * the claim and its outputs, leaving the expired boxes for the next miner. So a delay
    * would only lock the miner's own funds without buying any reorg safety.
    */
  private def p2pkCandidate(value: Long, minerTree: ErgoTree, currentHeight: Int): ErgoBoxCandidate =
    new ErgoBoxCandidate(value, minerTree, currentHeight, Colls.emptyColl, Map.empty)

  /**
    * Build a zero-fee storage-rent claim sweeping up to [[MaxClaims]] of the `eligible` boxes
    * to the miner's P2PK. Returns `None` when no box is claimable, or when the claim would
    * consist only of recreations whose fees do not clear the dust floor.
    *
    * @param eligible             - candidate boxes (e.g. from the storage-rent eligibility index,
    *                             resolved against the UTXO state)
    * @param currentHeight        - height of the block being assembled
    * @param parameters           - consensus parameters of the block being assembled
    * @param minerPk              - miner's public key; rent proceeds go to its plain P2PK box
    *                             (not the delayed miner-reward script: the delay is an
    *                             emission/fee rule, not a rent rule)
    * @param reemissionTokenIdOpt - re-emission token id on EIP-27 networks; boxes still
    *                             carrying it are skipped (consensus-unclaimable)
    * @param tokenWhitelist       - tokens which are never burned: when a box carrying them
    *                             is fully consumed, they are carried over to the miner's
    *                             proceeds output
    * @param onPermanentlySkipped - called for every examined box that can never be claimed
    *                             (re-emission token, min value or fee wrapping, value at or
    *                             below the minimum, undustable output, oversize box); the
    *                             caller uses it to evict such boxes from the eligibility
    *                             index so the scan window keeps advancing. Too-young boxes
    *                             and recreations dropped for dusty aggregate fees are
    *                             transient and are NOT reported.
    */
  def buildClaim(eligible: Seq[ErgoBox],
                 currentHeight: Int,
                 parameters: Parameters,
                 minerPk: ProveDlog,
                 reemissionTokenIdOpt: Option[ModifierId],
                 tokenWhitelist: Set[ModifierId],
                 onPermanentlySkipped: ErgoBox => Unit = _ => ()): Option[ErgoTransaction] = {

    val minerTree = ErgoTree.fromSigmaBoolean(minerPk)
    // every claimed box, in order, with the output it will name in var #127: output i
    // belongs to input i, so no index bookkeeping is needed
    val claimed = ArrayBuffer.empty[(ErgoBox, Boolean, ErgoBoxCandidate)]
    // recreation fees collected so far (burned values go to per-box outputs directly)
    var sweptFees = 0L
    // boxes looked at so far; the cap applies to CLAIMED boxes, not examined ones,
    // so unclaimable boxes can not starve later claimable ones out of the claim
    var examined = 0

    eligible.iterator.takeWhile(_ => claimed.length < MaxClaims).foreach { box =>
      examined += 1
      val age = currentHeight - box.creationHeight
      val oldEnough = age >= Constants.StoragePeriod
      val carriesReemissionToken = reemissionTokenIdOpt.exists(box.tokens.contains(_))
      // minimum allowed value in 32-bit arithmetic; like the storage fee below, it can
      // wrap to non-positive for huge boxes - such boxes can not be charged safely
      // (the recreated output's dust floor would be mispriced), so they are skipped
      val minValue = parameters.minValuePerByte * box.bytes.length
      // a box at or below the minimum allowed value can neither be charged nor recreated;
      // it is skipped (the caller drops such broken eligibility entries from the index)
      val aboveMinValue = minValue > 0 && box.value > minValue.toLong
      if (!oldEnough) {
        // transient (can not even arrive from the caller's cutoff-bounded scan): not
        // reported as permanently skipped
      } else if (carriesReemissionToken || !aboveMinValue) {
        onPermanentlySkipped(box)
      } else {
        // storage fee in 32-bit arithmetic, exactly as the consensus interpreter computes it;
        // a non-positive fee means the box is consensus-uncollectable and must be skipped
        val storageFee = parameters.storageFeeFactor * box.bytes.length
        if (storageFee > 0) {
          val afterFee = box.value - storageFee
          if (afterFee > 0) {
            // recreate branch: output preserves script/tokens/registers, sits at the current
            // height, carries the value minus the storage fee; the miner keeps the fee
            val outputIndex = claimed.length.toShort
            // the recreated box may need a few nanoERG more than the after-fee value when
            // the dust rule prices its serialization; bumping the value can lengthen the
            // VLQ encoding of the value itself, so iterate to the fixpoint
            var recreatedBox = new ErgoBoxCandidate(afterFee, box.ergoTree,
              currentHeight, box.additionalTokens, box.additionalRegisters)
            var dust = dustLimit(recreatedBox, outputIndex, parameters)
            while (recreatedBox.value < dust) {
              recreatedBox = new ErgoBoxCandidate(dust, box.ergoTree, currentHeight,
                box.additionalTokens, box.additionalRegisters)
              dust = dustLimit(recreatedBox, outputIndex, parameters)
            }
            if (recreatedBox.value <= box.value && boxSize(recreatedBox, outputIndex) <= ErgoBox.MaxBoxSize) {
              sweptFees += box.value - recreatedBox.value
              claimed += ((box, true, recreatedBox))
            } else {
              onPermanentlySkipped(box)
            }
          } else {
            // full-consume branch: the box can not cover its storage fee - it is destroyed
            // and its non-whitelisted tokens are burned with it; whitelisted tokens are
            // salvaged into the proceeds output. The box gets its own proceeds output
            // carrying exactly its value (plus the salvaged tokens) to the miner's P2PK.
            // A box whose value does not clear the dust floor for such an output can not
            // be claimed at all and is left behind.
            val salvagedTokens: Coll[(ErgoBox.TokenId, Long)] =
              if (tokenWhitelist.isEmpty) {
                Colls.emptyColl // nothing can be salvaged - skip filtering altogether
              } else {
                box.additionalTokens.filter(t => tokenWhitelist.contains(t._1.toModifierId))
              }
            val burnOutput = new ErgoBoxCandidate(box.value, minerTree, currentHeight, salvagedTokens, Map.empty)
            if (box.value >= dustLimit(burnOutput, claimed.length.toShort, parameters)) {
              claimed += ((box, false, burnOutput))
            } else {
              onPermanentlySkipped(box)
            }
          }
        } else {
          onPermanentlySkipped(box)
        }
      }
    }

    if (claimed.isEmpty) {
      log.debug(s"Storage-rent claim not built: no claimable box among $examined eligible")
      None
    } else {
      // Recreation fees collect in a separate, unnamed P2PK output appended after the
      // per-box outputs. When they do not clear the dust floor they can not be paid out,
      // so the recreations producing them are not claimed either (they stay eligible for
      // a later candidate with more proceeds); burns are unaffected, their outputs carry
      // the burned values themselves.
      val feeDust = dustLimit(p2pkCandidate(0L, minerTree, currentHeight), 0, parameters)
      val dropRecreations = sweptFees > 0 && sweptFees < feeDust
      val finalClaimed = if (dropRecreations) claimed.filter(!_._2) else claimed

      if (finalClaimed.isEmpty) {
        log.debug(s"Storage-rent claim not built: no claimable box among $examined eligible")
        None
      } else {
        val feeOutputs =
          if (!dropRecreations && sweptFees > 0) IndexedSeq(p2pkCandidate(sweptFees, minerTree, currentHeight))
          else IndexedSeq.empty
        val outputCandidates = finalClaimed.toIndexedSeq.map(_._3) ++ feeOutputs

        val inputs = finalClaimed.toIndexedSeq.zipWithIndex.map { case ((box, _, _), outputIndex) =>
          Input(box.id, ProverResult(
            Array.emptyByteArray,
            ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(outputIndex.toShort)))
          ))
        }
        val burnedValue = finalClaimed.filter(!_._2).map(_._3.value).sum
        val collected = burnedValue + (if (dropRecreations) 0L else sweptFees)
        log.info(s"Storage-rent claim built: ${finalClaimed.length} boxes claimed " +
          s"(${finalClaimed.count(_._2)} recreated, ${finalClaimed.count(!_._2)} burned), " +
          s"${examined - claimed.length} skipped, " +
          s"collecting $collected nanoERG")
        Some(ErgoTransaction(inputs, IndexedSeq.empty, outputCandidates))
      }
    }
  }

}
