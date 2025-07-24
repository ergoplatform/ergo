package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import cats.syntax.either._
import org.ergoplatform.OrderingSolutionFound

class AutolykosPowSchemeSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    forAll(invalidHeaderGen,
            Gen.choose(100, 120),
            Gen.choose[Byte](1, 2)) { (inHeader, difficulty, ver) =>
      val nBits = DifficultySerializer.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits, version = ver)
      val sk = randomSecret()
      val x = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      val hbs = Ints.toByteArray(h.height)
      val N = pow.calcN(h)
      pow.checkNonces(ver, hbs, msg, sk, x, b, N, 0, 1000) match {
        case OrderingSolutionFound(as) =>
          val nh = h.copy(powSolution = as)
          pow.validate(nh) shouldBe 'success

          if (ver > Header.InitialVersion) {
            // We remove last byte of "msg", perform PoW and check that it fails validation
            require(HeaderSerializer.bytesWithoutPow(h).last == 0)
            val msg2 = Blake2b256(HeaderSerializer.bytesWithoutPow(h).dropRight(1))

            pow.checkNonces(ver, hbs, msg2, sk, x, b, N, 0, 1000) match {
              case OrderingSolutionFound(as2) =>
                val nh2 = h.copy(powSolution = as2)
                pow.validate(nh2) shouldBe 'failure
              case _ =>
            }
          }
        case _ =>
      }
    }
  }

  property("calcN test vectors") {
    // mainnet parameters
    val k = 32
    val n = 26

    val pow = new AutolykosPowScheme(k, n)

    // N is always the same in Autolykos v1
    pow.calcN(1, 700000) shouldBe pow.NBase
    pow.calcN(1, 100000) shouldBe pow.NBase
    pow.calcN(1, 70000000) shouldBe pow.NBase

    pow.calcN(2, 500000) shouldBe pow.NBase
    pow.calcN(2, 600000) shouldBe pow.NBase
    pow.calcN(2, 600 * 1024) shouldBe 70464240
    pow.calcN(2, 650 * 1024) shouldBe 73987410
    pow.calcN(2, 700000) shouldBe 73987410
    pow.calcN(2, 788400) shouldBe 81571035 // 3 years
    pow.calcN(2, 1051200) shouldBe 104107290 // 4 years
    pow.calcN(2, 4198400) shouldBe 2143944600 // max height
    pow.calcN(2, 41984000) shouldBe 2143944600
  }

  property("test vectors for first increase in N value (height 614,400)") {
    import io.circe.parser._
    val pow = new AutolykosPowScheme(32, 26)

    val headerJson =
      """
        |{
        |  "extensionId" : "00cce45975d87414e8bdd8146bc88815be59cd9fe37a125b5021101e05675a18",
        |  "difficulty" : "16384",
        |  "votes" : "000000",
        |  "timestamp" : 4928911477310178288,
        |  "size" : 223,
        |  "stateRoot" : "5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780",
        |  "height" : 614400,
        |  "nBits" : 37748736,
        |  "version" : 2,
        |  "id" : "5603a937ec1988220fc44fb5022fb82d5565b961f005ebb55d85bd5a9e6f801f",
        |  "adProofsRoot" : "5d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcff",
        |  "transactionsRoot" : "f17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e",
        |  "extensionHash" : "1480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c0",
        |  "powSolutions" : {
        |    "pk" : "03bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c",
        |    "n" : "0000000000003105"
        |   },
        |  "adProofsId" : "dec129290a763f4de41f04e87e2b661dd59758af6bdd00dd51f5d97c3a8cb9b5",
        |  "transactionsId" : "eba1dd82cf51147232e09c1f72b37c554c30f63274d5093bff36849a83472a42",
        |  "parentId" : "ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d0"
        |}
      """.stripMargin

    val header = Header.jsonDecoder.decodeJson(parse(headerJson).toOption.get).toOption.get

    header.height shouldBe 614400

    val msg = Base16.encode(pow.msgByHeader(header))
    msg shouldBe "548c3e602a8f36f8f2738f5f643b02425038044d98543a51cabaa9785e7e864f"

    pow.calcN(header) shouldBe 70464240

    // vector got from a miner dev
    pow.hitForVersion2(header) shouldBe toBigInt(Base16.decode("0002fcb113fe65e5754959872dfdbffea0489bf830beb4961ddc0e9e66a1412a").get)

    pow.getB(header.nBits) shouldBe BigInt("7067388259113537318333190002971674063283542741642755394446115914399301849")

    Base16.encode(groupElemToBytes(header.powSolution.pk)) shouldBe "03bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c"

    Base16.encode(header.powSolution.n) shouldBe "0000000000003105"

    pow.validate(header) shouldBe 'success
  }

  // testing an invalid header got from a mining pool
  property("test vector - invalid solution") {
    import io.circe.parser._

    val headerJson = "{\"extensionId\":\"277907e4e5e42f27e928e6101cc4fec173bee5d7728794b73d7448c339c380e5\",\"difficulty\":\"1325481984\",\"votes\":\"000000\",\"timestamp\":1611225263165,\"size\":219,\"stateRoot\":\"c0d0b5eafd07b22487dac66628669c42a242b90bef3e1fcdc76d83140d58b6bc0e\",\"height\":2870,\"nBits\":72286528,\"version\":2,\"id\":\"5b0ce6711de6b926f60b67040cc4512804517785df375d063f1bf1d75588af3a\",\"adProofsRoot\":\"49453875a43035c7640dee2f905efe06128b00d41acd2c8df13691576d4fd85c\",\"transactionsRoot\":\"770cbb6e18673ed025d386487f15d3252115d9a6f6c9b947cf3d04731dd6ab75\",\"extensionHash\":\"9bc7d54583c5d44bb62a7be0473cd78d601822a626afc13b636f2cbff0d87faf\",\"powSolutions\":{\"pk\":\"0288114b0586efea9f86e4587f2071bc1c85fb77e15eba96b2769733e0daf57903\",\"w\":\"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798\",\"n\":\"000100000580a91b\",\"d\":0},\"adProofsId\":\"4fc36d59bf26a672e01fbfde1445bd66f50e0f540f24102e1e27d0be1a99dfbf\",\"transactionsId\":\"d196ef8a7ef582ab1fdab4ef807715183705301c6ae2ff0dcbe8f1d577ba081f\",\"parentId\":\"ab19e6c7a4062979dddb534df83f236d1b949c7cef18bcf434a67e87c593eef9\"}"

    val json = parse(headerJson).toOption.get

    val header = Header.jsonDecoder.decodeJson(json).toOption.get

    val pow = new AutolykosPowScheme(32, 26)

    pow.validate(header).isSuccess shouldBe false
  }

}
