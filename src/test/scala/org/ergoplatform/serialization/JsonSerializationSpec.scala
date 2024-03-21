package org.ergoplatform.serialization

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.http.api.{ApiCodecs, ApiExtraCodecs, ApiRequestsCodecs}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.ErgoSettingsReader
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalatest.{EitherValues, Inspectors}

import scala.util.Random

class JsonSerializationSpec extends ErgoCorePropertyTest
  with ApiCodecs
  with ApiRequestsCodecs
  with ApiExtraCodecs
  with EitherValues {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  property("PopowProof roundtrip"){
    forAll(poPowProofGen){ pp =>
      val json = pp.asJson
      implicit val decoder: Decoder[NipopowProof] = NipopowProof.nipopowProofDecoder(nipopowAlgos)
      val parsedProof = json.as[NipopowProof].toOption.get
      parsedProof shouldEqual pp
    }
  }


  property("unsignedErgoTransaction roundtrip") {
    forAll(validUnsignedErgoTransactionGen) { case (_, tx) =>
      val json = tx.asJson
      val parsedTx = json.as[UnsignedErgoTransaction].toOption.get
      parsedTx shouldBe tx
    }
  }

  property("ErgoFullBlock should be encoded into JSON and decoded back correctly") {

    val (st, bh) = createUtxoState(settings)
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)

    val blockJson: Json = block.asJson
    val blockDecoded: ErgoFullBlock = blockJson.as[ErgoFullBlock].toTry.get

    blockDecoded shouldEqual block
  }

  property("PaymentRequest should be serialized to json") {
    val ergoSettings = ErgoSettingsReader.read()
    implicit val requestEncoder: Encoder[PaymentRequest] = new PaymentRequestEncoder(ergoSettings)
    implicit val requestDecoder: Decoder[PaymentRequest] = new PaymentRequestDecoder(ergoSettings)
    forAll(paymentRequestGen) { request =>
      val json = request.asJson
      val parsingResult = json.as[PaymentRequest]
      parsingResult.isRight shouldBe true
      val restored = parsingResult.value
      restored.address shouldEqual request.address
      restored.value shouldEqual request.value
      restored.registers shouldEqual request.registers
      Inspectors.forAll(restored.assets.zip(request.assets)) {
        case ((restoredToken, restoredValue), (requestToken, requestValue)) =>
          restoredToken shouldEqual requestToken
          restoredValue shouldEqual requestValue
      }
    }
  }

  property("BurnTokensRequest should be serialized to json") {
    implicit val requestEncoder: Encoder[BurnTokensRequest] = new BurnTokensRequestEncoder()
    implicit val requestDecoder: Decoder[BurnTokensRequest] = new BurnTokensRequestDecoder()
    forAll(burnTokensRequestGen) { request =>
      val json = request.asJson
      val parsingResult = json.as[BurnTokensRequest]
      parsingResult.isRight shouldBe true
      val restored = parsingResult.value
      Inspectors.forAll(restored.assetsToBurn.zip(request.assetsToBurn)) {
        case ((restoredToken, restoredValue), (requestToken, requestValue)) =>
          restoredToken shouldEqual requestToken
          restoredValue shouldEqual requestValue
      }
    }
  }

  property("AssetIssueRequest should be serialized to json") {
    val ergoSettings = ErgoSettingsReader.read()
    implicit val requestEncoder: Encoder[AssetIssueRequest] = new AssetIssueRequestEncoder(ergoSettings)
    implicit val requestDecoder: Decoder[AssetIssueRequest] = new AssetIssueRequestDecoder(ergoSettings)
    forAll(assetIssueRequestGen) { request =>
      val json = request.asJson
      val parsingResult = json.as[AssetIssueRequest]
      parsingResult.isRight shouldBe true
      val restored = parsingResult.value
      restored.addressOpt shouldEqual request.addressOpt
      restored.amount shouldEqual request.amount
      restored.name shouldEqual request.name
      restored.description shouldEqual request.description
      restored.decimals shouldEqual request.decimals
    }
  }

  property("transactionSigningRequest roundtrip") {
    forAll(transactionSigningRequestGen(Random.nextBoolean)) { request =>
      val json = request.asJson
      val parsedRequest = json.as[TransactionSigningRequest].toOption.get
      parsedRequest shouldBe request
    }
  }

}
