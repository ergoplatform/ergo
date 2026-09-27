package org.ergoplatform.mining

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigma.Colls
import sigma.ast.{ErgoTree, ShortConstant}
import sigma.data.ProveDlog
import sigma.interpreter.{ContextExtension, ProverResult}

import scala.collection.mutable.ArrayBuffer

/**
  * Miner-side storage-rent self-claim transaction builder, a port of the Rust reference node's
  * `build_rent_claim` (`ergo-mining/src/storage_rent_claim.rs`), extended to respect the
  * storage-rent grace period rules (token/register-bearing boxes).
  *
  * A mining node may sweep storage-rent-eligible boxes (unspent for at least the storage
  * period) directly into a zero-fee transaction paying the freed rent to the miner's P2PK,
  * included into its own block candidate. The miner needs no transaction fee to incentivize
  * inclusion because it controls the block.
  *
  * Grace-period-aware behavior (claims built this way are valid both before and after the
  * grace-period flag-day activation):
  * - recreation charges a box only towards the minimum allowed value
  *   (`minValuePerByte * box bytes`), topping up is never done by the miner - a box below the
  *   minimum is left for sponsors;
  * - full consumption (burning) of a box happens only after the grace period
  *   (`StorageGracePeriod` past the storage period), and never when the box carries a token
  *   from the configured whitelist - tokens of burned boxes are burned with the box;
  *
  * The builder is pure (no I/O): callers pass resolved eligible boxes and get back a
  * transaction ready for block-assembly validation. A box is silently skipped when claiming
  * it would produce an output the consensus validator rejects: box too young, storage fee
  * wrapping to non-positive in 32-bit arithmetic (consensus-uncollectable), recreated output
  * below the dust floor or past the box size cap, or (EIP-27 networks) a box still carrying
  * re-emission tokens, which is consensus-unclaimable outright. If the total proceeds cannot
  * clear the dust floor, no claim is produced.
  */
object StorageRentClaimBuilder extends ScorexLogging {

  /**
    * Grace period for storage-rent claims: a box which cannot be charged (its value is below
    * the minimum allowed value) may be fully consumed only this long after the storage period
    * is over. 2 weeks. Merge note: use `ErgoInterpreter.StorageGracePeriod` once the
    * grace-period branch is merged.
    */
  val StorageGracePeriod: Int = 2 * Constants.BlocksPerWeek

  /**
    * Max boxes one claim transaction may sweep. Keeps the claim's cost and size well under
    * block limits (a 100-input claim costs well below 1M cost units).
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
    * Build a zero-fee storage-rent claim sweeping up to [[MaxClaims]] of the `eligible` boxes
    * to the miner's P2PK. Returns `None` when no box is claimable or the total proceeds do not
    * clear the dust floor.
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
    * @param tokenWhitelist       - tokens which may never be burned: boxes carrying them are
    *                             never fully consumed (left for sponsors)
    */
  def buildClaim(eligible: Seq[ErgoBox],
                 currentHeight: Int,
                 parameters: Parameters,
                 minerPk: ProveDlog,
                 reemissionTokenIdOpt: Option[ModifierId],
                 tokenWhitelist: Set[ModifierId]): Option[ErgoTransaction] = {

    val recreated = ArrayBuffer.empty[ErgoBoxCandidate]
    // claimed box -> Some(recreated output index) or None (full-consume into aggregate output)
    val claimed = ArrayBuffer.empty[(ErgoBox, Option[Int])]
    var sweptValue = 0L

    eligible.take(MaxClaims).foreach { box =>
      val age = currentHeight - box.creationHeight
      val oldEnough = age >= Constants.StoragePeriod
      val gracePeriodPassed = age >= Constants.StoragePeriod + StorageGracePeriod
      val carriesReemissionToken = reemissionTokenIdOpt.exists(box.tokens.contains(_))
      val carriesWhitelistedToken = tokenWhitelist.exists(box.tokens.contains(_))
      if (oldEnough && !carriesReemissionToken) {
        // storage fee in 32-bit arithmetic, exactly as the consensus interpreter computes it;
        // a non-positive fee means the box is consensus-uncollectable and must be skipped
        val storageFee = parameters.storageFeeFactor * box.bytes.length
        // the grace-period floor: the box keeps at least the minimum allowed value
        val minValue = parameters.minValuePerByte.toLong * box.bytes.length
        if (storageFee > 0) {
          val floorValue = math.max(box.value - storageFee, minValue)
          if (floorValue <= box.value) {
            // recreate branch: output preserves script/tokens/registers, sits at the current
            // height, carries at least the floor value; the miner keeps the rest
            val outputIndex = recreated.length.toShort
            val recreatedBox = new ErgoBoxCandidate(floorValue, box.ergoTree,
              currentHeight, box.additionalTokens, box.additionalRegisters)
            val dust = dustLimit(recreatedBox, outputIndex, parameters)
            // the recreated box may need a few nanoERG more than the floor when the dust rule
            // prices its (possibly longer) serialization
            val recreatedValue = math.max(floorValue, dust)
            if (recreatedValue <= box.value && boxSize(recreatedBox, outputIndex) <= ErgoBox.MaxBoxSize) {
              val finalBox =
                if (recreatedValue == floorValue) recreatedBox
                else new ErgoBoxCandidate(recreatedValue, box.ergoTree, currentHeight,
                  box.additionalTokens, box.additionalRegisters)
              recreated += finalBox
              sweptValue += box.value - recreatedValue
              claimed += ((box, Some(outputIndex.toInt)))
            }
          } else if (gracePeriodPassed && !carriesWhitelistedToken) {
            // full-consume branch: the box can not be charged any more (its value is below the
            // minimum) and the grace period is over - the box is destroyed, its value goes to
            // the miner and its tokens are burned with it
            sweptValue += box.value
            claimed += ((box, None))
          }
        }
      }
    }

    if (claimed.isEmpty) {
      None
    } else {
      // aggregate P2PK output sits right after every recreated box, so recreate inputs'
      // var-127 indices stay valid and full-consume inputs name this index
      val p2pkIndex = recreated.length.toShort
      val p2pkBox = new ErgoBoxCandidate(sweptValue, ErgoTree.fromSigmaBoolean(minerPk),
        currentHeight, Colls.emptyColl, Map.empty)

      if (p2pkBox.value < dustLimit(p2pkBox, p2pkIndex, parameters) ||
        boxSize(p2pkBox, p2pkIndex) > ErgoBox.MaxBoxSize) {
        None
      } else {
        val outputCandidates = recreated.toIndexedSeq :+ p2pkBox
        val inputs = claimed.toIndexedSeq.map { case (box, recreatedIndexOpt) =>
          val outputIndex = recreatedIndexOpt.getOrElse(p2pkIndex.toInt).toShort
          Input(box.id, ProverResult(
            Array.emptyByteArray,
            ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(outputIndex)))
          ))
        }
        Some(ErgoTransaction(inputs, IndexedSeq.empty, outputCandidates))
      }
    }
  }

}
