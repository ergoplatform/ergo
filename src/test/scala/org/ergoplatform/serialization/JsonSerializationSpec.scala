package org.ergoplatform.serialization

import io.circe.ACursor
import io.circe.parser.parse
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.api.ApiEncoderOption.HideDetails.implicitValue
import org.ergoplatform.api.ApiEncoderOption.{Detalization, ShowDetails}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, TransactionIdsForHeader}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.{SBoolean, SType}

class JsonSerializationSpec extends ErgoPropertyTest with WalletGenerators with ApiCodecs {

  property("TransactionIdsForHeader should be converted into json correctly") {
    val modifierId = genBytes(Constants.ModifierIdSize).sample.get
    val stringId = Algos.encode(modifierId)
    val Right(expected) = parse(s"""{ "ids" : ["$stringId"]}""")
    val data = TransactionIdsForHeader(Seq(modifierId))
    val c = data.asJson.hcursor
    c.downField("ids").downArray.as[String] shouldBe Right(stringId)
  }

  property("ErgoBox should be converted into json correctly") {
    forAll(ergoBoxGen) { box =>
      validateErgoBox(box.asJson.hcursor, box)
    }
  }

  property("Unspent Offchain Box should be serialized to json") {
    forAll(unspentOffchainBoxGen) { b =>
      validateTrackedBox(b.asJson.hcursor, b)
      import ShowDetails.implicitValue
      validateTrackedBox(b.asJson.hcursor, b)
    }
  }

  property("Unspent Onchain Box should be serialized to json") {
    forAll(unspentOnchainBoxGen) { b =>
      validateUnspentOnchainBox(b.asJson.hcursor, b)
      import ShowDetails.implicitValue
      validateUnspentOnchainBox(b.asJson.hcursor, b)
    }
  }

  property("Spent Offchain Box should be serialized to json") {
    forAll(spentOffchainBoxGen) { b =>
      validateSpentOffchainBox(b.asJson.hcursor, b)
      import ShowDetails.implicitValue
      validateSpentOffchainBox(b.asJson.hcursor, b)
    }
  }

  property("Spent Onchain Box should be serialized to json") {
    forAll(spentOnchainBoxGen) { b =>
      validateSpentOnchainBox(b.asJson.hcursor, b)
      import ShowDetails.implicitValue
      validateSpentOnchainBox(b.asJson.hcursor, b)
    }
  }

  private def validateSpentOnchainBox(c: ACursor, b: SpentOnchainBox)(implicit opts: Detalization) = {
    validateSpentBox(c, b)(opts)
    c.downField("creationHeight").as[Int] shouldBe Right(b.creationHeight)
    c.downField("spendingHeight").as[Int] shouldBe Right(b.spendingHeight)
  }

  private def validateSpentOffchainBox(c: ACursor, b: SpentOffchainBox)(implicit opts: Detalization) = {
    validateSpentBox(c, b)(opts)
    c.downField("creationHeight").as[Option[Int]] shouldBe Right(b.creationHeight)
  }

  private def validateSpentBox(c: ACursor, b: SpentBox)(implicit opts: Detalization) = {
    validateTrackedBox(c, b)(opts)
    if (opts.showDetails) {
      validateTransaction(c.downField("spendingTransaction"), b.spendingTx)
    } else {
      c.downField("spendingTransactionId").as[String] shouldBe Right(Algos.encode(b.spendingTx.id))
    }
  }

  private def validateUnspentOnchainBox(c: ACursor, b: UnspentOnchainBox)(implicit opts: Detalization) = {
    validateTrackedBox(c, b)(opts)
    c.downField("creationHeight").as[Int] shouldBe Right(b.creationHeight)
  }

  private def validateTrackedBox(c: ACursor, b: TrackedBox)(implicit opts: Detalization) = {
    import b._
    c.downField("creationOutIndex").as[Short] shouldBe Right(creationOutIndex)
    c.downField("spent").as[Boolean] shouldBe Right(spent)
    c.downField("onchain").as[Boolean] shouldBe Right(onchain)
    c.downField("certain").as[Boolean] shouldBe Right(certain)
    validateErgoBox(c.downField("box"), box)
    if (opts.showDetails) {
      validateTransaction(c.downField("creationTransaction"), creationTx)
    } else {
      c.downField("creationTransactionId").as[String] shouldBe Right(Algos.encode(creationTx.id))
    }
  }

  private def validateErgoBox(c: ACursor, b: ErgoBox): Unit = {
    c.downField("boxId").as[String] shouldBe Right(Algos.encode(b.id))
    c.downField("value").as[Long] shouldBe Right(b.value)
    c.downField("proposition").as[Value[SBoolean.type]] shouldBe Right(b.proposition)
    validateAssets(c.downField("assets"), b.additionalTokens)
    validateRegisters(c.downField("additionalRegisters"), b.additionalRegisters)
  }

  private def validateAssets(c: ACursor, assets: Seq[(ErgoBox.TokenId, Long)]) = {
    def stringify(assets: Seq[(ErgoBox.TokenId, Long)]) = {
      assets map { case(tokenId, value) => (Algos.encode(tokenId), value) }
    }
    import ErgoTransaction.assetDecoder
    val Right(decodedAssets) = c.as[Seq[(ErgoBox.TokenId, Long)]]
    stringify(decodedAssets) should contain theSameElementsAs stringify(assets)
  }

  private def validateRegisters(c: ACursor, registers: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]]) = {
    import ErgoTransaction.identifierDecoder
    val Right(decodedRegs) = c.as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    decodedRegs should contain theSameElementsAs registers
  }

  private def validateTransaction(c: ACursor, tx: ErgoTransaction) = {
    import ErgoTransaction.transactionDecoder
    val Right(decoded) = c.as[ErgoTransaction]
    decoded.id should contain theSameElementsInOrderAs tx.id
    decoded.inputs should contain theSameElementsInOrderAs tx.inputs
    decoded.outputs should contain theSameElementsInOrderAs tx.outputs
  }

}
