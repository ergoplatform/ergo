package org.ergoplatform.nodeView.wallet

import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils._
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.scalatest.concurrent.Eventually
import scala.concurrent.duration._

class InputBlockWalletSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  property("input block transactions prevent double spending") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      
      // Create initial state with some boxes
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      
      // Generate a transaction that spends some boxes and creates new ones
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }
      
      // Scan the transaction as a locally generated input block
      wallet.scanInputBlock(Seq(tx))
      
      // Wait for wallet state to update
      eventually {
        // Verify that we cannot generate another transaction that would double-spend the same inputs
        // This should fail because the inputs are already marked as spent
        val attempt = await(wallet.generateTransaction(Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))))
        
        // The generation should fail due to insufficient funds (inputs already spent)
        attempt shouldBe 'failure
      }
    }
  }


  property("boxes created in input blocks can be spent in subsequent blocks") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      
      // Create initial state with some boxes
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      
      // Generate first transaction that creates outputs
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx1 = eventually {
        val sumToSpend = MinBoxValue * 10
        val req = Seq(PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
        await(wallet.generateTransaction(req)).get
      }

      // Apply first transaction as an input block (making outputs spendable)
      wallet.scanInputBlock(Seq(tx1))

      Thread.sleep(100)

      val boxes = eventually {
        await(wallet.walletBoxes(unspentOnly = true, considerUnconfirmed = true))
      }

      boxes.size shouldBe 2
      
      // Generate second transaction that spends outputs from first transaction
      eventually {
        // Create a transaction spending the outputs from tx1
        val req2 = Seq(PaymentRequest(addresses.head, MinBoxValue, Array.empty, Map.empty))
        await(wallet.generateTransaction(req2)).get
      }
    }
  }

}
