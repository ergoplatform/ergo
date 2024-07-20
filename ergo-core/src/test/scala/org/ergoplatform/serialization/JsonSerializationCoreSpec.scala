package org.ergoplatform.serialization

import io.circe.syntax._
import io.circe.ACursor
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{AdditionalRegisters, NonMandatoryRegisterId}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.http.api.ApiEncoderOption.HideDetails.implicitValue
import org.ergoplatform.http.api.ApiEncoderOption.{Detalization, ShowDetails}
import org.ergoplatform.sdk.wallet.secrets.{DhtSecretKey, DlogSecretKey}
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.TrackedBox
import cats.syntax.either._
import sigma.ast.{ErgoTree, EvaluatedValue, SType}

class JsonSerializationCoreSpec extends ErgoCorePropertyTest
  with ApiCodecs {
  import org.ergoplatform.wallet.utils.WalletGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

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


  property("json-encoded dlog secret is always about 32 bytes") {
    forAll(dlogSecretWithPublicImageGen) { case (secret, _) =>
      val wrappedSecret = DlogSecretKey(secret)
      val json = wrappedSecret.asJson
      json.toString().length shouldBe 66 // 32-bytes hex-encoded data (64 ASCII chars) + quotes == 66 ASCII chars
    }
  }

  property("dlog secret roundtrip") {
    forAll(dlogSecretWithPublicImageGen) { case (secret, _) =>
      val wrappedSecret = DlogSecretKey(secret)
      val json = wrappedSecret.asJson
      val parsedSecret = json.as[DlogSecretKey].toOption.get
      parsedSecret shouldBe wrappedSecret
    }
  }

  property("dht secret roundtrip") {
    forAll(dhtSecretWithPublicImageGen) { case (secret, _) =>
      val wrappedSecret = DhtSecretKey(secret)
      val json = wrappedSecret.asJson
      val parsedSecret = json.as[DhtSecretKey].toOption.get
      parsedSecret shouldBe wrappedSecret
    }
  }

  private def checkTrackedBox(c: ACursor, b: TrackedBox)(implicit opts: Detalization) = {
    c.downField("spent").as[Boolean] shouldBe Right(b.spendingStatus.spent)
    c.downField("onchain").as[Boolean] shouldBe Right(b.chainStatus.onChain)
    c.downField("scans").as[Set[ScanId]] shouldBe Right(b.scans)
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

    val Right(decodedAssets) = c.as[Seq[(ErgoBox.TokenId, Long)]]
    stringify(decodedAssets) should contain theSameElementsAs stringify(assets)
  }

  private def checkRegisters(c: ACursor, registers: AdditionalRegisters) = {
    val Right(decodedRegs) = c.as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    decodedRegs should contain theSameElementsAs registers
  }

}
