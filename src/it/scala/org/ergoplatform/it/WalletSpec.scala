package org.ergoplatform.it

import com.typesafe.config.Config
import io.circe.Json
import io.circe.Json.JArray
import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.it.api.NodeApi.UnexpectedStatusCodeException
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util.RichEither
import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import org.ergoplatform.nodeView.wallet.requests.{PaymentRequest, PaymentRequestEncoder, RequestsHolder, RequestsHolderEncoder}
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, ErgoWalletServiceImpl}
import org.ergoplatform.settings.{Args, ErgoSettings, LaunchParameters}
import org.ergoplatform.utils.{ErgoTestHelpers, WalletTestOps}
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.ergoplatform.{ErgoBox, P2PKAddress}
import org.scalatest.wordspec.AsyncWordSpec
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, TrueLeaf}

import scala.concurrent.ExecutionContext

class WalletSpec extends AsyncWordSpec with IntegrationSuite with WalletTestOps with ApiCodecs {

  override implicit def executionContext: ExecutionContext = ErgoTestHelpers.defaultExecutionContext

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))

  private val nodeConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)
  private val node: Node = docker.startDevNetNode(nodeConfig, sequentialTopologyConfig).get
  implicit val requestsHolderEncoder: RequestsHolderEncoder = new RequestsHolderEncoder(ergoSettings)

  "it should be initialized with testMnemonic" in {
    node.waitForStartup.flatMap { node: Node =>
      node.getWihApiKey("/wallet/status")
    }.map { response =>
      val body = parse(response.getResponseBody)
      body.flatMap(_.hcursor.downField("isInitialized").as[Boolean]) shouldBe Right(true)
      body.flatMap(_.hcursor.downField("isUnlocked").as[Boolean]) shouldBe Right(true)
      body.flatMap(_.hcursor.downField("walletHeight").as[Int]) shouldBe Right(0)
    }
  }

  "initializing already initialized wallet should fail" in {
    node.waitForStartup.flatMap { node: Node =>
      recoverToExceptionIf[UnexpectedStatusCodeException] {
        node.postJson("/wallet/init", Json.obj("pass" -> "foo".asJson))
      }.map { ex =>
        ex.response.getStatusCode shouldBe 400
        ex.response.getResponseBody should include("Wallet is already initialized")
      }
    }
  }

  "restoring initialized wallet should fail" in {
    node.waitForStartup.flatMap { node: Node =>
      recoverToExceptionIf[UnexpectedStatusCodeException] {
        node.postJson("/wallet/restore", Json.obj("pass" -> "foo".asJson, "mnemonic" -> "bar".asJson))
      }.map { ex =>
        ex.response.getStatusCode shouldBe 400
        ex.response.getResponseBody should include("Wallet is already initialized")
      }
    }
  }

  "it should generate unsigned transaction" in {
    import sigmastate.eval._
    val mnemonic = walletAutoInitConfig.getString("ergo.wallet.testMnemonic")
    val prover = new ErgoWalletServiceImpl().buildProverFromMnemonic(mnemonic, None, LaunchParameters)
    val pk = prover.hdPubKeys.head.key
    val ergoTree = ErgoTree.fromProposition(TrueLeaf)
    val transactionId = ModifierId @@ Base16.encode(Array.fill(32)(5: Byte))
    val input = new ErgoBox(60000000, ergoTree, Colls.emptyColl[(TokenId, Long)], Map.empty, transactionId, 0, 1)

    val encodedBox = Base16.encode(ErgoBoxSerializer.toBytes(input))

    val paymentRequest = PaymentRequest(P2PKAddress(pk), 50000000, Seq.empty, Map.empty)
    val requestsHolder = RequestsHolder(Seq(paymentRequest), feeOpt = Some(100000L), Seq(encodedBox), dataInputsRaw = Seq.empty, minerRewardDelay = 720)

    node.waitForStartup.flatMap { node: Node =>
      for {
        _ <- node.postJson("/wallet/payment/send", Json.arr(new PaymentRequestEncoder(settings)(paymentRequest)))
        generateTxResp <- node.postJson("/wallet/transaction/generateUnsigned", requestsHolder.asJson)
        txs            <- node.getWihApiKey("/wallet/transactions")
      } yield (txs, generateTxResp)
    }.map { case (txs, generateTxResp) =>
      decode[Seq[AugWalletTransaction]](txs.getResponseBody).left.map(_.getMessage).get shouldBe empty

      val generatedTx = decode[UnsignedErgoTransaction](generateTxResp.getResponseBody).left.map(_.getMessage).get
      generatedTx.inputs.size shouldBe 1
      generatedTx.outputs.size shouldBe 3
      generatedTx.outputCandidates.size shouldBe 3
    }
  }

}
