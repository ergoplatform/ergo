package org.ergoplatform.modifiers.mempool

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{Constants, ErgoValidationSettingsUpdate, Parameters}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.Assertion
import scorex.util.encode.Base16
import sigma.Colls
import sigma.ast.{ByteArrayConstant, ErgoTree, ShortConstant}
import sigma.data.Digest32Coll
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ErgoTreeSerializer
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}

class ExpirationSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._
  import org.ergoplatform.utils.NodeViewTestOps._

  type Height = Int

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)

  def injectScript(box: ErgoBox, script: ErgoTree): ErgoBox = {
    testBox(box.value,
      script,
      box.creationHeight,
      box.additionalTokens.toArray.toSeq,
      box.additionalRegisters,
      transactionId = box.transactionId,
      boxIndex = box.index)
  }

  def constructTest(from: ErgoBox,
                    heightDelta: Int,
                    outsConstructor: Height => IndexedSeq[ErgoBoxCandidate],
                    expectedValidity: Boolean)
                   (implicit verifier: ErgoInterpreter): Assertion = {
    // We are filtering out certain heights to avoid problems with improperly generated extension
    // at the beginning of a voting epoch
    whenever((from.creationHeight + Constants.StoragePeriod + heightDelta) % votingSettings.votingLength != 0) {
      val in = Input(from.id,
        ProverResult(Array.emptyByteArray, ContextExtension(Map(Constants.StorageIndexVarId -> ShortConstant(0)))))

      val h: Int = from.creationHeight + Constants.StoragePeriod + heightDelta

      val oc = outsConstructor(h).map(c => updateHeight(c, h))
      val tx = ErgoTransaction(inputs = IndexedSeq(in), dataInputs = IndexedSeq(), outputCandidates = oc)

      val fb0 = invalidErgoFullBlockGen.sample.get
      val fakeHeader = fb0.header.copy(height = h - 1)
      val fb = fb0.copy(fb0.header.copy(height = h, parentId = fakeHeader.id))

      val updContext = {
        val inContext = new ErgoStateContext(Seq(fakeHeader), None, genesisStateDigest, parameters, validationSettingsNoIl,
          VotingData.empty)(settings.chainSettings)
        inContext.appendFullBlock(fb).get
      }

      //serialization roundtrip
      val bs = ErgoTransactionSerializer.toBytes(tx)
      ErgoTransactionSerializer.parseBytes(bs) shouldBe tx

      tx.statelessValidity().isSuccess shouldBe true
      tx.statefulValidity(IndexedSeq(from), emptyDataBoxes, updContext).isSuccess shouldBe expectedValidity
    }
  }

  property("successful spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, _ => IndexedSeq(from), expectedValidity = true)
    }
  }

  property("successful spending w. invalid ergotree") {
    forAll(unspendableErgoBoxGen()) { from =>
      // invalid (unparseable) ergo tree
      val etString = "0e1631393039303063646462363930366462363530336665"
      val et = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(etString).get)
      val modified = injectScript(from, et)
      constructTest(modified, 0, _ => IndexedSeq(modified), expectedValidity = true)
    }
  }

  property("successful spending w. max spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = true)
    }
  }

  property("unsuccessful spending due too big storage fee charged") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 0, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("unsuccessful spending when more time passed than storage period and charged more than K*storagePeriod") {
    forAll(unspendableErgoBoxGen(parameters.storageFeeFactor * 100 + 1, Long.MaxValue)) { from =>
      constructTest(from, 1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length + 1, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)

        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("too early spending") {
    forAll(unspendableErgoBoxGen()) { from =>
      constructTest(from, -1, h => {
        val fee = Math.min(parameters.storageFeeFactor * from.bytes.length, from.value)
        val feeBoxCandidate = new ErgoBoxCandidate(fee, TrueTree, creationHeight = h)
        IndexedSeq(changeValue(from, -fee), Some(feeBoxCandidate)).flatten
      }, expectedValidity = false)
    }
  }

  property("script changed spending w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      val out = new ErgoBoxCandidate(from.value, TrueTree, from.creationHeight + 1, from.additionalTokens)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
    }
  }

  property("script changed tokens w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalTokens.nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.ergoTree, from.creationHeight + 1, Colls.emptyColl)
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  property("script changed register w. same value") {
    forAll(unspendableErgoBoxGen()) { from =>
      whenever(from.additionalRegisters.get(ErgoBox.R4).nonEmpty) {
        val out = new ErgoBoxCandidate(from.value, from.ergoTree, from.creationHeight + 1, from.additionalTokens)
        constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = false)
      }
    }
  }

  property("spending of whole coin when its value no more than storage fee") {
    val out2 = ergoBoxGenNoProp.sample.get
    val minValue = out2.value + 1

    forAll(unspendableErgoBoxGen(minValue, Long.MaxValue)) { from =>
      val outcome = from.value <= from.bytes.length * parameters.storageFeeFactor
      val out1 = new ErgoBoxCandidate(from.value - minValue, TrueTree, creationHeight = from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out1, out2), expectedValidity = outcome)
    }
  }

  property("destructing the whole box when its value no more than storage fee") {
    forAll(unspendableErgoBoxGen(maxValue = parameters.storageFeeFactor)) { from =>
      val out = new ErgoBoxCandidate(from.value, TrueTree, creationHeight = from.creationHeight + 1)
      constructTest(from, 0, _ => IndexedSeq(out), expectedValidity = true)
    }
  }

  // Storage-rent repairs (block version Header.Interpreter70Version+):
  // 64-bit storage-fee arithmetic + the EIP-27 re-emission carve-out.

  private val repairedParameters: Parameters = Parameters(
    0,
    parameters.parametersTable.updated(Parameters.BlockVersion, Header.Interpreter70Version.toInt),
    ErgoValidationSettingsUpdate.empty)

  // A well-formed 32-byte token id standing in for the chain's re-emission
  // token (the test application.conf leaves reemissionTokenId empty).
  private val reemissionTokenIdBytes: sigma.Coll[Byte] =
    Colls.fromArray(Array.fill[Byte](32)(0x2a))

  private val repairedVerifier: ErgoInterpreter =
    ErgoInterpreter(repairedParameters, Some(reemissionTokenIdBytes))

  property("storage-rent repairs: fee-overflowed box uncollectable before, fully consumable after") {
    // A box big enough that `storageFeeFactor * bytes.length` wraps Int-negative.
    // Legacy rules then demand a recreated value ABOVE the box's own value
    // (impossible without a subsidy); from the repairs the true 64-bit fee
    // exceeds the box value, so the whole box is consumable.
    val bigPayload = ByteArrayConstant(Colls.fromArray(Array.fill[Byte](1800)(0x7f.toByte)))
    forAll(unspendableErgoBoxGen(1000000000L, 2000000000L)) { base =>
      val from = testBox(base.value, FalseTree, base.creationHeight,
        Seq.empty, Map(ErgoBox.R4 -> bigPayload), base.transactionId, base.index)
      val wrappedFee = parameters.storageFeeFactor * from.bytes.length
      val trueFee = parameters.storageFeeFactor.toLong * from.bytes.length
      whenever(wrappedFee < 0 && trueFee >= from.value) {
        val outs = (h: Height) => IndexedSeq(new ErgoBoxCandidate(from.value, TrueTree, h))
        constructTest(from, 0, outs, expectedValidity = false)
        constructTest(from, 0, outs, expectedValidity = true)(repairedVerifier)
      }
    }
  }

  property("storage-rent repairs: second-wrap box charged its true fee after activation") {
    // A box so big the fee wraps PAST Int range back to a small positive
    // number. Legacy rules accept a claim charging only the tiny wrapped fee;
    // from the repairs the box owes its true 64-bit fee.
    val hugePayload = ByteArrayConstant(Colls.fromArray(Array.fill[Byte](3500)(0x11.toByte)))
    forAll(unspendableErgoBoxGen(5000000000L, 6000000000L)) { base =>
      val from = testBox(base.value, FalseTree, base.creationHeight,
        Seq.empty, Map(ErgoBox.R4 -> hugePayload), base.transactionId, base.index)
      val wrappedFee = parameters.storageFeeFactor * from.bytes.length
      val trueFee = parameters.storageFeeFactor.toLong * from.bytes.length
      whenever(wrappedFee > 0 && trueFee > wrappedFee && from.value > trueFee) {
        val outs = (h: Height) => {
          val recreated = new ErgoBoxCandidate(from.value - trueFee, from.ergoTree, h,
            from.additionalTokens, from.additionalRegisters)
          val collector = new ErgoBoxCandidate(trueFee, TrueTree, h)
          IndexedSeq(recreated, collector)
        }
        // legacy floor is `value - wrappedFee` (tiny fee), so charging the
        // true fee under-recreates and is rejected
        constructTest(from, 0, outs, expectedValidity = false)
        constructTest(from, 0, outs, expectedValidity = true)(repairedVerifier)
      }
    }
  }

  property("storage-rent repairs: re-emission token box claimable with the token dropped") {
    // Pre-repair rules: dropping the token violates register preservation, so
    // the box is unclaimable (the EIP-27 deadlock). Post-repair rules: the
    // token MUST be dropped and its nanoErg equivalent (1 per token) is
    // charged on top of the storage fee, funding the pay-to-reemission burn.
    val reemToken = (Digest32Coll @@ reemissionTokenIdBytes) -> 12L
    forAll(unspendableErgoBoxGen(1000000000L, Long.MaxValue)) { base =>
      val from = testBox(base.value, FalseTree, base.creationHeight,
        Seq(reemToken), Map.empty, base.transactionId, base.index)
      val fee = parameters.storageFeeFactor.toLong * from.bytes.length
      whenever(fee > 0 && from.value > fee + 12L) {
        val outs = (h: Height) => {
          val recreated = new ErgoBoxCandidate(from.value - fee - 12L, from.ergoTree, h)
          val collector = new ErgoBoxCandidate(fee + 12L, TrueTree, h)
          IndexedSeq(recreated, collector)
        }
        constructTest(from, 0, outs, expectedValidity = false)
        constructTest(from, 0, outs, expectedValidity = true)(repairedVerifier)
      }
    }
  }

  property("storage-rent repairs: recreated box keeping the re-emission token is rejected") {
    // The mirror case: preserving the token satisfies the LEGACY register
    // rule (this is exactly the half of the deadlock that verifyReemissionSpending
    // then kills on mainnet), but the repaired rule requires it dropped.
    val reemToken = (Digest32Coll @@ reemissionTokenIdBytes) -> 12L
    forAll(unspendableErgoBoxGen(1000000000L, Long.MaxValue)) { base =>
      val from = testBox(base.value, FalseTree, base.creationHeight,
        Seq(reemToken), Map.empty, base.transactionId, base.index)
      val fee = parameters.storageFeeFactor.toLong * from.bytes.length
      whenever(fee > 0 && from.value > fee + 12L) {
        val outs = (h: Height) => {
          val recreated = new ErgoBoxCandidate(from.value - fee, from.ergoTree, h, from.additionalTokens)
          val collector = new ErgoBoxCandidate(fee, TrueTree, h)
          IndexedSeq(recreated, collector)
        }
        constructTest(from, 0, outs, expectedValidity = true)
        constructTest(from, 0, outs, expectedValidity = false)(repairedVerifier)
      }
    }
  }

}
