package org.ergoplatform.modifiers.history

import org.ergoplatform.nodeView.history.HistorySpecification
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers
import scala.util.Random

class PoPoWProofSpec extends PropSpec
  with HistorySpecification
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  private def genHistory() =
    generateHistory(verify = false, adState = true, popow = false, blocksToKeep = 0, nonce = Random.nextLong(), epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private lazy val popowHistory = ensureMinimalHeight(genHistory(), 200)

  //todo: why .map not .get?
  property("Valid PoPoWProof generation") {
    popowHistory.constructPoPoWProof(5, 5).map{proof =>
      PoPoWProof.validate(proof) shouldBe 'success
    }
  }

  property("Valid PoPoWProof serialization") {
    popowHistory.constructPoPoWProof(5, 5).map { proof =>
      val recovered = PoPoWProofSerializer.parseBytes(PoPoWProofSerializer.toBytes(proof)).get
      PoPoWProofSerializer.toBytes(proof) shouldEqual PoPoWProofSerializer.toBytes(recovered)
    }
  }
}
