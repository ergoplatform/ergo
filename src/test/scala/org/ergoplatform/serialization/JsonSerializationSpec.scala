package org.ergoplatform.serialization

import io.circe.syntax._
import io.circe.{ACursor, Decoder, Encoder, Json}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.api.ApiEncoderOption.HideDetails.implicitValue
import org.ergoplatform.api.ApiEncoderOption.{Detalization, ShowDetails}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.requests._
import org.ergoplatform.settings.{Algos, Args, ErgoSettings}
import org.ergoplatform.utils.ErgoPropertyTest
import org.ergoplatform.utils.generators.WalletGenerators
import org.ergoplatform.wallet.boxes.TrackedBox
import org.scalatest.Inspectors
import sigmastate.SType
import sigmastate.Values.{ErgoTree, EvaluatedValue}

class JsonSerializationSpec extends ErgoPropertyTest with WalletGenerators with ApiCodecs {

  property("ErgoFullBlock should be encoded into JSON and decoded back correctly") {

    val (st, bh) = createUtxoState()
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)

    val blockJson: Json = block.asJson
    val blockDecoded: ErgoFullBlock = blockJson.as[ErgoFullBlock].toTry.get

    blockDecoded shouldEqual block
  }

  property("ErgoBox should be converted into json correctly") {
    forAll(ergoBoxGen) { box =>
      checkErgoBox(box.asJson.hcursor, box)
    }
  }

  property("TrackedBox should be serialized to json") {
    forAll(trackedBoxGen) { b =>
      checkTrackedBox(b.asJson.hcursor, b)
      import ShowDetails.implicitValue
      checkTrackedBox(b.asJson.hcursor, b)
    }
  }

  property("PaymentRequest should be serialized to json") {
    val ergoSettings = ErgoSettings.read()
    implicit val requestEncoder: Encoder[PaymentRequest] = new PaymentRequestEncoder(ergoSettings)
    implicit val requestDecoder: Decoder[PaymentRequest] = new PaymentRequestDecoder(ergoSettings)
    forAll(paymentRequestGen) { request =>
      val json = request.asJson
      val parsingResult = json.as[PaymentRequest]
      parsingResult.isRight shouldBe true
      val restored = parsingResult.right.value
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

  property("AssetIssueRequest should be serialized to json") {
    val ergoSettings = ErgoSettings.read()
    implicit val requestEncoder: Encoder[AssetIssueRequest] = new AssetIssueRequestEncoder(ergoSettings)
    implicit val requestDecoder: Decoder[AssetIssueRequest] = new AssetIssueRequestDecoder(ergoSettings)
    forAll(assetIssueRequestGen) { request =>
      val json = request.asJson
      val parsingResult = json.as[AssetIssueRequest]
      parsingResult.isRight shouldBe true
      val restored = parsingResult.right.value
      restored.addressOpt shouldEqual request.addressOpt
      restored.amount shouldEqual request.amount
      restored.name shouldEqual request.name
      restored.description shouldEqual request.description
      restored.decimals shouldEqual request.decimals
    }
  }

  private def checkTrackedBox(c: ACursor, b: TrackedBox)(implicit opts: Detalization) = {
    c.downField("spent").as[Boolean] shouldBe Right(b.spendingStatus.spent)
    c.downField("onchain").as[Boolean] shouldBe Right(b.chainStatus.onChain)
    c.downField("certain").as[Boolean] shouldBe Right(b.certainty.certain)
    c.downField("creationOutIndex").as[Short] shouldBe Right(b.creationOutIndex)
    c.downField("inclusionHeight").as[Option[Int]] shouldBe Right(b.inclusionHeightOpt)
    c.downField("spendingHeight").as[Option[Int]] shouldBe Right(b.spendingHeightOpt)
    checkErgoBox(c.downField("box"), b.box)
    if (!opts.showDetails) {
      c.downField("creationTransactionId").as[String] shouldBe Right(b.creationTxId)
      c.downField("spendingTransactionId").as[Option[String]] shouldBe Right(b.spendingTxIdOpt)
    }
  }

  private def checkErgoBox(c: ACursor, b: ErgoBox): Unit = {
    c.downField("boxId").as[String] shouldBe Right(Algos.encode(b.id))
    c.downField("value").as[Long] shouldBe Right(b.value)
    c.downField("ergoTree").as[ErgoTree] shouldBe Right(b.ergoTree)
    checkAssets(c.downField("assets"), b.additionalTokens.toArray.toSeq)
    checkRegisters(c.downField("additionalRegisters"), b.additionalRegisters)
    c.downField("creationHeight").as[Int] shouldBe Right(b.creationHeight)
  }

  private def checkAssets(c: ACursor, assets: Seq[(ErgoBox.TokenId, Long)]) = {
    def stringify(assets: Seq[(ErgoBox.TokenId, Long)]) = {
      assets map { case (tokenId, amount) => (Algos.encode(tokenId), amount) }
    }
    import ErgoTransaction.assetDecoder
    val Right(decodedAssets) = c.as[Seq[(ErgoBox.TokenId, Long)]]
    stringify(decodedAssets) should contain theSameElementsAs stringify(assets)
  }

  private def checkRegisters(c: ACursor, registers: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]]) = {
    val Right(decodedRegs) = c.as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    decodedRegs should contain theSameElementsAs registers
  }

  private def checkTransaction(c: ACursor, txOpt: Option[ErgoTransaction]) = {
    import ErgoTransaction.transactionDecoder
    val Right(decodedTxOpt) = c.as[Option[ErgoTransaction]]
    if (txOpt.isEmpty) {
      decodedTxOpt shouldBe empty
    } else {
      val decoded = decodedTxOpt.get
      val tx = txOpt.get
      decoded.id shouldEqual tx.id
      decoded.inputs should contain theSameElementsInOrderAs tx.inputs
      decoded.outputs should contain theSameElementsInOrderAs tx.outputs
    }
  }

}
