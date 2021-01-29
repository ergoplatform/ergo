package org.ergoplatform.network

import org.ergoplatform.mining.{AutolykosSolution, groupElemFromBytes}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.interpreter.CryptoConstants.EcPointType

class ModifiersSerializationSpecification extends ErgoPropertyTest with DecodingUtils {

  private def base16ToEcPoint(pointEncoded: String): EcPointType = {
    val bytes = Algos.decode(pointEncoded).get
    groupElemFromBytes(bytes)
  }

  property("Header simple parsing") {
    // real header from mainnet, at 414,474, https://explorer.ergoplatform.com/en/blocks/8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f
    val version = 1 : Byte
    val height = 414474
    val parentId = ModifierId @@ "8bdd043dab20aa690afc9a18fc4797de4f02f049f5c16f9657646c753d69582e"
    val adProofsRoot = Digest32 @@ Base16.decode("4527a2a7bcee7f77b5697f505e5effc5342750f58a52dddfe407a3ce3bd3abd0").get
    val stateRoot = ADDigest @@ Base16.decode("6c06d6277d40aeb958c5631515dc3ec3d11d8504e62de77df024d0ca67242fb512").get
    val transactionsRoot = Digest32 @@ Base16.decode("722f9306300d0d96fe8c10de830216d700131614f9e6ce2496e8dba1cbb45951").get
    val timestamp = 1611874199636L
    val nBits = 118039443L
    val extensionRoot = Digest32 @@ Base16.decode("a1a3933312467ce53d41fdc20e38c603e8fd89999371c60d7537c5d5760ef7c4").get

    val pk = base16ToEcPoint("02bb8eb301ab3d5d14515e33760d0dfb4f7191312a640db64a3a1aeeac9703f2d3")
    val w = base16ToEcPoint("026d7b267c33120d15c267664081a6b77a6dcae6b35147db2c3e1195573119cb14")
    val n = Base16.decode("0008a1d103880117").get
    val d = BigInt("35863003992655055679291741607273543535646500642591973829915050")
    val powSolution = AutolykosSolution(pk, w, n, d)
    val votes = Array[Byte](4, 3, 0)

    val h = Header(version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
                    height, extensionRoot, powSolution, votes)

    h.id shouldBe "8cf6dca6b9505243e36192fa107735024c0000cf4594b1daa2dc4e13ee86f26f"
  }

}
