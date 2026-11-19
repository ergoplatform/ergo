package org.ergoplatform.nodeView.state

import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Algos
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.VersionDependentValues._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import scorex.crypto.authds.ADKey
import sigma.interpreter.{ContextExtension, ProverResult}

/**
  * Block application keys an output by the id it computes at the default VersionContext. Rendered to JSON, the
  * same output must have that id.
  */
class OutputIdVersionsSpec extends ErgoCorePropertyTest with ApiCodecs {

  private val tx = atV6 {
    val input = Input(ADKey @@ Array.fill(32)(2.toByte), ProverResult(Array.emptyByteArray, ContextExtension.empty))
    val output = new ErgoBoxCandidate(1000000000L, TrueTree, 0, additionalRegisters = Map(ErgoBox.R4 -> upcastValue))
    ErgoTransaction(IndexedSeq(input), IndexedSeq(output))
  }

  // the key block application inserts for the output, on a copy read from the block on its own
  private def keyInUtxoSet: String = {
    val (_, inserted) = ErgoState.boxChanges(Seq(readFromBlock(tx))).get
    Algos.encode(inserted.head.key)
  }

  property("an output's id in transaction json is the UTXO set's key") {
    readFromBlock(tx).asJson.hcursor.downField("outputs").downN(0).downField("boxId")
      .as[String] shouldBe Right(keyInUtxoSet)
  }

}
