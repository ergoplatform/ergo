package org.ergoplatform.modifiers.history

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.utils.{ChainGenerator, ErgoGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.TestkitHelpers

import scala.annotation.tailrec
import scala.util.Try

class PoPoWProofSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with TestkitHelpers
  with ChainGenerator {

  def generatePoPoWProof(m: Int, k: Int): PoPoWProof = {
    constructPoPoWProof(m, k, genHeaderChain(100, Seq()).headers).get
  }

  property("Valid PoPoWProof generation") {
    PoPoWProof.validate(generatePoPoWProof(5, 5)) shouldBe 'success
  }

  property("Valid PoPoWProof serialization") {
    val proof = generatePoPoWProof(5, 5)
    val recovered = PoPoWProofSerializer.parseBytes(PoPoWProofSerializer.toBytes(proof)).get
    PoPoWProofSerializer.toBytes(proof) shouldEqual PoPoWProofSerializer.toBytes(recovered)
  }

  //todo: interlink check test

  /**
    * Constructs SPV Proof from KLS16 paper
    *
    * @param m          - parameter "m" from the paper (minimal length of innerchain to include)
    * @param k          - parameter "k" from the paper (chain suffix)
    * @param blockchain - chain of headers to construct a proof from
    * @return
    */
  def constructPoPoWProof(m: Int, k: Int, blockchain: Seq[Header]): Try[PoPoWProof] = Try {
    require(m > 0 && m < blockchain.length, s"$m > 0 && $m < ${blockchain.length}")
    require(k > 0 && k < blockchain.length, s"$k > 0 && $k < ${blockchain.length}")

    val (_, suffix: Seq[Header]) = blockchain.splitAt(blockchain.length - k)
    val firstSuffix = suffix.head

    //TODO make efficient
    val blockchainMap: Map[ByteArrayWrapper, Header] = blockchain.map(b => ByteArrayWrapper(b.id) -> b).toMap

    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    @tailrec
    def constructProof(i: Int): (Int, Seq[Header]) = {
      @tailrec
      def loop(acc: Seq[Header]): Seq[Header] = {
        val interHeader = acc.head
        if (interHeader.interlinks.length > i && i >= 0) {
          val header = headerById(interHeader.interlinks(i))
          loop(header +: acc)
        } else {
          acc.dropRight(1)
        }
      }

      val innerchain = loop(Seq(firstSuffix))
      if (innerchain.length >= m) (i, innerchain) else constructProof(i - 1)
    }

    val (depth, innerchain) = constructProof(firstSuffix.interlinks.length)

    PoPoWProof(m.toByte, k.toByte, depth.toByte, innerchain, suffix)
  }
}
