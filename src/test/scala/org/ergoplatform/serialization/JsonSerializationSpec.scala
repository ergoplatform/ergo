package org.ergoplatform.serialization

import io.circe.{ACursor, Decoder, Encoder}
import io.circe.parser.parse
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.api.ApiEncoderOption.HideDetails.implicitValue
import org.ergoplatform.api.ApiEncoderOption.{Detalization, ShowDetails}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, TransactionIdsForHeader}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}
import org.scalatest.Inspectors
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.{SBoolean, SType}

class JsonSerializationSpec extends ErgoPropertyTest with WalletGenerators with ApiCodecs {

  property("TransactionIdsForHeader should be converted into json correctly") {
    val modifierId = genBytes(Constants.ModifierIdSize).sample.get
    val stringId = Algos.encode(modifierId)
    val Right(_) = parse(s"""{ "ids" : ["$stringId"]}""")
    val data = TransactionIdsForHeader(Seq(modifierId))
    val c = data.asJson.hcursor
    c.downField("ids").downArray.as[String] shouldBe Right(stringId)
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

  property("Payment Request should be serialized to json") {
    val ergoSettings = ErgoSettings.read(None)
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
      Inspectors.forAll(restored.assets.getOrElse(Seq.empty).zip(request.assets.getOrElse(Seq.empty))) {
        case ((restoredToken, restoredValue), (requestToken, requestValue)) =>
          restoredToken shouldEqual requestToken
          restoredValue shouldEqual requestValue
      }
    }
  }

  private def checkTrackedBox(c: ACursor, b: TrackedBox)(implicit opts: Detalization) = {
    c.downField("spent").as[Boolean] shouldBe Right(b.spendingStatus.spent)
    c.downField("onchain").as[Boolean] shouldBe Right(b.onchainStatus.onchain)
    c.downField("certain").as[Boolean] shouldBe Right(b.certainty.certain)
    c.downField("creationOutIndex").as[Short] shouldBe Right(b.creationOutIndex)
    c.downField("creationHeight").as[Option[Int]] shouldBe Right(b.creationHeightOpt)
    c.downField("spendingHeight").as[Option[Int]] shouldBe Right(b.spendingHeightOpt)
    checkErgoBox(c.downField("box"), b.box)
    if (!opts.showDetails) {
      c.downField("creationTransactionId").as[String] shouldBe Right(b.encodedCreationTxId)
      c.downField("spendingTransactionId").as[Option[String]] shouldBe Right(b.encodedSpendingTxId)
    } else {
      checkTransaction(c.downField("creationTransaction"), Some(b.creationTx))
      checkTransaction(c.downField("spendingTransaction"), b.spendingTxOpt)
    }
  }

  private def checkErgoBox(c: ACursor, b: ErgoBox): Unit = {
    c.downField("boxId").as[String] shouldBe Right(Algos.encode(b.id))
    c.downField("value").as[Long] shouldBe Right(b.value)
    c.downField("proposition").as[Value[SBoolean.type]] shouldBe Right(b.proposition)
    checkAssets(c.downField("assets"), b.additionalTokens)
    checkRegisters(c.downField("additionalRegisters"), b.additionalRegisters)
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
      decoded.id should contain theSameElementsInOrderAs tx.id
      decoded.inputs should contain theSameElementsInOrderAs tx.inputs
      decoded.outputs should contain theSameElementsInOrderAs tx.outputs
    }
  }

}
