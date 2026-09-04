package org.ergoplatform.serialization

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.http.api.{ApiCodecs, ApiExtraCodecs, ApiRequestsCodecs}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.Constants.FalseTree
import org.ergoplatform.settings.{Constants, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalatest.{EitherValues, Inspectors}
import scorex.util.encode.Base16

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

  property("wallet request token ids should enforce the canonical length") {
    val ergoSettings = ErgoSettingsReader.read()
    implicit val paymentRequestDecoder: Decoder[PaymentRequest] = new PaymentRequestDecoder(ergoSettings)
    implicit val burnTokensRequestDecoder: Decoder[BurnTokensRequest] = new BurnTokensRequestDecoder()
    val address = Pay2SAddress(FalseTree)(ergoSettings.addressEncoder).toString

    def boxesRequest(tokenId: String): Decoder.Result[BoxesRequest] =
      Json.obj(
        "targetBalance" -> 1L.asJson,
        "targetAssets" -> Json.obj(tokenId -> 1L.asJson)
      ).as[BoxesRequest]

    def paymentRequest(tokenId: String): Decoder.Result[PaymentRequest] =
      Json.obj(
        "address" -> address.asJson,
        "value" -> 1L.asJson,
        "assets" -> Json.arr(Json.obj("tokenId" -> tokenId.asJson, "amount" -> 1L.asJson))
      ).as[PaymentRequest]

    def burnRequest(tokenId: String): Decoder.Result[BurnTokensRequest] =
      Json.obj(
        "assetsToBurn" -> Json.arr(Json.obj("tokenId" -> tokenId.asJson, "amount" -> 1L.asJson))
      ).as[BurnTokensRequest]

    val invalidTokenIds = Seq(1, 31, 33)
      .map(length => Base16.encode(Array.fill(length)(length.toByte))) :+ "not-hex"

    invalidTokenIds.foreach { tokenId =>
      boxesRequest(tokenId).isLeft shouldBe true
      paymentRequest(tokenId).isLeft shouldBe true
      burnRequest(tokenId).isLeft shouldBe true
    }

    val validTokenId = Base16.encode(Array.fill(Constants.ModifierIdSize)(1.toByte))
    boxesRequest(validTokenId).isRight shouldBe true
    paymentRequest(validTokenId).isRight shouldBe true
    burnRequest(validTokenId).isRight shouldBe true
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

  property("TransactionRequestDecoder should roundtrip every request subtype") {
    val ergoSettings = ErgoSettingsReader.read()
    val encoder = new TransactionRequestEncoder(ergoSettings)
    val decoder = new TransactionRequestDecoder(ergoSettings)
    forAll(paymentRequestGen, assetIssueRequestGen, burnTokensRequestGen) { (payment, issuance, burn) =>
      Seq(payment, issuance, burn).foreach { request =>
        val restored = decoder.decodeJson(encoder(request)).value
        restored.getClass shouldEqual request.getClass
        encoder(restored) shouldEqual encoder(request)
      }
    }
  }

  property("TransactionRequestDecoder should preserve optional payment assets and burn arrays") {
    val ergoSettings = ErgoSettingsReader.read()
    val decoder = new TransactionRequestDecoder(ergoSettings)
    val address = Pay2SAddress(FalseTree)(ergoSettings.addressEncoder)
    val payment = PaymentRequest(address, 1000000L, Array.empty, Map.empty)
    val paymentJson = new PaymentRequestEncoder(ergoSettings)(payment)
    val paymentForms = Seq(
      paymentJson.mapObject(_.remove("assets")),
      paymentJson.mapObject(_.add("assets", Json.Null)),
      paymentJson
    )
    paymentForms.foreach { json =>
      val restored = decoder.decodeJson(json).value.asInstanceOf[PaymentRequest]
      restored.address shouldEqual payment.address
      restored.value shouldEqual payment.value
      restored.assets shouldBe empty
      restored.registers shouldBe empty
    }
    Seq(Json.Null, Json.arr()).foreach { assets =>
      val restored = decoder.decodeJson(Json.obj("assetsToBurn" -> assets)).value.asInstanceOf[BurnTokensRequest]
      restored.assetsToBurn shouldBe empty
    }
  }

  property("TransactionRequestDecoder should preserve optional issuance fields") {
    val ergoSettings = ErgoSettingsReader.read()
    val decoder = new TransactionRequestDecoder(ergoSettings)
    val encoder = new AssetIssueRequestEncoder(ergoSettings)
    val issuance = AssetIssueRequest(None, None, 100L, "Example", "Example asset", 2, None)
    val encoded = encoder(issuance)
    val omitted = encoded.mapObject(_.remove("address").remove("ergValue").remove("registers"))
    Seq(encoded, omitted).foreach { json =>
      decoder.decodeJson(json).value shouldEqual issuance
    }
    val populated = issuance.copy(
      addressOpt = Some(Pay2SAddress(FalseTree)(ergoSettings.addressEncoder)),
      valueOpt = Some(1000000L),
      registers = Some(Map.empty)
    )
    decoder.decodeJson(encoder(populated)).value shouldEqual populated
  }

  property("TransactionRequestDecoder should retain mixed RequestsHolder content and order") {
    val ergoSettings = ErgoSettingsReader.read()
    val encoder = new RequestsHolderEncoder(ergoSettings)
    val decoder = new RequestsHolderDecoder(ergoSettings)
    forAll(paymentRequestGen, assetIssueRequestGen, burnTokensRequestGen) { (payment, issuance, burn) =>
      val holder = RequestsHolder(
        Seq(issuance, burn, payment, issuance), Some(1000000L), Seq("input"), Seq("data-input"),
        ergoSettings.chainSettings.monetary.minerRewardDelay
      )(ergoSettings.addressEncoder)
      val restored = decoder.decodeJson(encoder(holder)).value
      restored.requests.map(_.getClass) shouldEqual holder.requests.map(_.getClass)
      encoder(restored) shouldEqual encoder(holder)
      restored.minerRewardDelay shouldEqual holder.minerRewardDelay
    }
  }

  property("TransactionRequestDecoder should select by key presence and priority") {
    val ergoSettings = ErgoSettingsReader.read()
    val address = Pay2SAddress(FalseTree)(ergoSettings.addressEncoder)
    val payment = PaymentRequest(address, 1000000L, Array.empty, Map.empty)
    val issuance = AssetIssueRequest(None, None, 100L, "Example", "Example asset", 2, None)
    val burn = BurnTokensRequest(Array.empty)
    var calls = Vector.empty[String]
    val decoder = new TransactionRequestDecoder(ergoSettings) {
      override val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings) {
        override def apply(cursor: HCursor): Decoder.Result[PaymentRequest] = {
          calls :+= "payment"
          Right(payment)
        }
      }
      override val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings) {
        override def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
          calls :+= "issuance"
          Right(issuance)
        }
      }
      override val burnTokensRequestDecoder: BurnTokensRequestDecoder = new BurnTokensRequestDecoder {
        override def apply(cursor: HCursor): Decoder.Result[BurnTokensRequest] = {
          calls :+= "burn"
          Right(burn)
        }
      }
    }
    val encoder = new TransactionRequestEncoder(ergoSettings)
    val burnJson = encoder(burn)
    val issuanceJson = encoder(issuance)
    val markers = Seq(
      ("value", 1000000L.asJson, payment),
      ("assets", Json.arr(), payment),
      ("amount", 100L.asJson, issuance),
      ("name", "Example".asJson, issuance),
      ("description", "Example asset".asJson, issuance),
      ("decimals", 2.asJson, issuance),
      ("ergValue", 1000000L.asJson, issuance),
      ("assetsToBurn", Json.arr(), burn)
    )
    def check(json: Json, expected: TransactionGenerationRequest, selected: String): Unit = {
      calls = Vector.empty
      decoder.decodeJson(json) shouldEqual Right(expected)
      calls shouldEqual Vector(selected)
    }
    markers.foreach { case (key, value, expected) =>
      val selected = if (expected == payment) "payment" else if (expected == issuance) "issuance" else "burn"
      Seq(value, Json.Null).foreach { markerValue =>
        check(burnJson.mapObject(_.add(key, markerValue)), expected, selected)
      }
    }
    Seq("value" -> 1000000L.asJson, "assets" -> Json.arr()).foreach { case (key, value) =>
      Seq(value, Json.Null).foreach { markerValue =>
        check(issuanceJson.mapObject(_.add("assetsToBurn", Json.arr()).add(key, markerValue)), payment, "payment")
      }
    }
    Seq(
      Json.obj("address" -> address.toString.asJson, "registers" -> Json.obj()),
      Json.obj("address" -> Json.Null, "registers" -> Json.Null)
    ).foreach { sharedFields =>
      check(burnJson.deepMerge(sharedFields), burn, "burn")
    }
    Seq(Json.obj(), Json.obj("address" -> address.toString.asJson, "registers" -> Json.obj())).foreach { json =>
      calls = Vector.empty
      decoder.decodeJson(json).isLeft shouldBe true
      calls shouldBe empty
    }
  }

  Seq("payment", "issuance", "burn").foreach { selected =>
    property(s"TransactionRequestDecoder should propagate the selected $selected failure through RequestsHolder") {
      val ergoSettings = ErgoSettingsReader.read()
      val address = Pay2SAddress(FalseTree)(ergoSettings.addressEncoder)
      val fixtures: Map[String, TransactionGenerationRequest] = Map(
        "payment" -> PaymentRequest(address, 1000000L, Array.empty, Map.empty),
        "issuance" -> AssetIssueRequest(None, None, 100L, "Example", "Example asset", 2, None),
        "burn" -> BurnTokensRequest(Array.empty)
      )
      val encoder = new TransactionRequestEncoder(ergoSettings)
      val json = encoder(fixtures(selected))
      var calls = Vector.empty[String]
      def failure(cursor: HCursor): DecodingFailure =
        DecodingFailure(s"Synthetic $selected decoder failure", cursor.downField("synthetic").history)

      val decoder = new TransactionRequestDecoder(ergoSettings) {
        override val paymentRequestDecoder: PaymentRequestDecoder = new PaymentRequestDecoder(ergoSettings) {
          override def apply(cursor: HCursor): Decoder.Result[PaymentRequest] = {
            calls :+= "payment"
            if (selected == "payment") Left(failure(cursor)) else super.apply(cursor)
          }
        }
        override val assetIssueRequestDecoder: AssetIssueRequestDecoder = new AssetIssueRequestDecoder(ergoSettings) {
          override def apply(cursor: HCursor): Decoder.Result[AssetIssueRequest] = {
            calls :+= "issuance"
            if (selected == "issuance") Left(failure(cursor)) else super.apply(cursor)
          }
        }
        override val burnTokensRequestDecoder: BurnTokensRequestDecoder = new BurnTokensRequestDecoder {
          override def apply(cursor: HCursor): Decoder.Result[BurnTokensRequest] = {
            calls :+= "burn"
            if (selected == "burn") Left(failure(cursor)) else super.apply(cursor)
          }
        }
      }

      decoder.decodeJson(json) shouldEqual Left(failure(json.hcursor))
      calls shouldEqual Vector(selected)

      val preceding = encoder(fixtures(if (selected == "payment") "issuance" else "payment"))
      val holderJson = Json.obj("requests" -> Json.arr(preceding, json))
      val holderDecoder = new RequestsHolderDecoder(ergoSettings) {
        override implicit val transactionRequestDecoder: TransactionRequestDecoder = decoder
      }
      val itemCursor = holderJson.hcursor.downField("requests").downArray.right.success.get
      holderDecoder.decodeJson(holderJson) shouldEqual Left(failure(itemCursor))
    }
  }

}
