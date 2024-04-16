package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Ints, Longs}
import org.ergoplatform.mining.{AutolykosPowScheme, q, randomSecret}
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.powScheme
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId

class HeadersSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val chain: HeaderChain = genHeaderChain(50, diffBitsOpt = None, useRealTs = false)
  val genesisId: ModifierId = chain.head.id

  private def mutateNonce(nonce: Array[Byte]): Array[Byte] = {
    Longs.toByteArray(Longs.fromByteArray(nonce) + 1)
  }

  property("Any field change should lead to different id") {
    forAll(defaultHeaderGen) { header =>
      val initialId = header.id
      header.copy(version = (header.version + 1).toByte).id should not equal initialId
      header.copy(parentId = initialId).id should not equal initialId
      header.copy(ADProofsRoot = Blake2b256(header.ADProofsRoot)).id should not equal initialId
      header.copy(transactionsRoot = Blake2b256(header.transactionsRoot)).id should not equal initialId
      header.copy(timestamp = header.timestamp + 1).id should not equal initialId
      header.copy(nBits = header.nBits + 1).id should not equal initialId
      header.copy(height = header.height + 1).id should not equal initialId
      header.copy(extensionRoot = Blake2b256(header.extensionRoot)).id should not equal initialId
      header.copy(powSolution = header.powSolution.copy(n = mutateNonce(header.powSolution.n))).id should not equal initialId
      if(header.version == 1) {
        header.copy(powSolution = header.powSolution.copy(d = header.powSolution.d + 1)).id should not equal initialId
      }
    }
  }

  property("bytes can be added to the header") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)

    val difficulty = 10
    val ver = (Header.Interpreter50Version + 1).toByte
    val nBits = DifficultySerializer.encodeCompactBits(difficulty)
    val header = defaultHeaderGen.sample.get.copy(version = ver, nBits = nBits)
    val bytesWithoutPow = HeaderSerializer.bytesWithoutPow(header)
    bytesWithoutPow(bytesWithoutPow.length - 1) = 5
    val updByteWithoutPow: Array[Byte] = bytesWithoutPow ++ Array.fill(5)(5.toByte)
    val msg = Blake2b256(updByteWithoutPow)

    val sk = randomSecret()
    val x = randomSecret()
    val b = pow.getB(header.nBits)
    val hbs = Ints.toByteArray(header.height)
    val N = pow.calcN(header)
    val newHeader = pow.checkNonces(ver, hbs, msg, sk, x, b, N, 0, 1000)
      .map(s => header.copy(powSolution = s)).get
    pow.validate(newHeader) shouldBe 'success
  }

}
