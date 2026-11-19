package org.ergoplatform.nodeView.mempool

import org.ergoplatform.mining.emission.EmissionRules.CoinsInOneErgo
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.BoxHolder
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.{ErgoStateContextHelpers, ErgoTestHelpers}
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.wallet.utils.TestFileUtils
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.{EvaluatedValue, LongConstant, SType}
import sigma.interpreter.{ContextExtension, ProverResult}


/**
  * Transactions with a register or context extension value that is serialized differently below ErgoTree
  * version 3 and from it, as the mempool processes them.
  */
class ErgoMemPoolVersionDependentValuesSpec extends AnyFlatSpec
  with Matchers
  with ErgoTestHelpers
  with ErgoStateContextHelpers
  with TestFileUtils {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val inputBox = new ErgoBox(10L * CoinsInOneErgo, Constants.TrueTree, Colls.emptyColl, Map.empty,
    bytesToId(Array.fill(32)(1.toByte)), 0, creationHeight = 0)

  private def spend(register: Option[EvaluatedValue[SType]],
                    extension: Option[EvaluatedValue[SType]]): ErgoTransaction = atV6 {
    val input = Input(inputBox.id,
      ProverResult(Array.emptyByteArray, ContextExtension(extension.map(0.toByte -> _).toMap)))
    val output = new ErgoBoxCandidate(inputBox.value, Constants.TrueTree, 0,
      additionalRegisters = register.map(ErgoBox.R4 -> _).toMap)
    ErgoTransaction(IndexedSeq(input), IndexedSeq.empty, IndexedSeq(output))
  }

  private def process(tx: ErgoTransaction): ProcessingOutcome = {
    val state = WrappedUtxoState(BoxHolder(Seq(inputBox)), createTempDir, settings.launchParameters, settings)
    // as a transaction relayed by a peer arrives: with the bytes it was read from
    val (_, outcome) = ErgoMemPool.empty(settings).process(UnconfirmedTransaction(tx, atV6(tx.bytes), None), state)
    outcome
  }

  it should "accept a transaction with a version-dependent value in an output register" in {
    process(spend(register = Some(upcastValue), extension = None)) shouldBe a[ProcessingOutcome.Accepted]
  }

  // control: under a script that is evaluated, as here, verification fails on such a value in an extension,
  // with or without this change
  it should "invalidate a transaction with a version-dependent value in an input extension" in {
    process(spend(register = None, extension = Some(upcastValue))) shouldBe a[ProcessingOutcome.Invalidated]
  }

  it should "accept the same spend with constants" in {
    val outcome = process(spend(register = Some(LongConstant(1L)), extension = Some(LongConstant(1L))))
    outcome shouldBe a[ProcessingOutcome.Accepted]
  }

  it should "relay a block transaction returned to the pool under its own id" in {
    val blockTx = readFromBlock(spend(register = Some(upcastValue), extension = Some(upcastValue)))
    val relayed = UnconfirmedTransaction(blockTx, None).transactionBytes.get
    atV6(ErgoTransactionSerializer.parseBytes(relayed)).id shouldBe blockTx.id // as a peer parses it
  }

}
