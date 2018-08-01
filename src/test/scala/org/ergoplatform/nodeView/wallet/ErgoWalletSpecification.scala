package org.ergoplatform.nodeView.wallet

import akka.actor.ActorSystem
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.ADKey
import sigmastate.{SBoolean, Values}
import sigmastate.Values.Value
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.Random


class ErgoWalletSpecification extends ErgoPropertyTest {

  property("successfully scans an offchain transaction") {

    implicit val actorSystem = ActorSystem()
    val w: ErgoWallet = new ErgoWallet(actorSystem, null, null, settings)

    val bf0 = w.unconfirmedBalances()

    val bs0 = Await.result(bf0, 1.second)

    bs0.balance shouldBe 0
    bs0.assetBalances.isEmpty shouldBe true

    val af = w.walletAddresses()
    val as = Await.result(af, 1.second)

    val pubKey = as.head.asInstanceOf[P2PKAddress].pubkey

    def makeTx(balance: Int, script: Value[SBoolean.type] = pubKey) = {
      val input = Input(ADKey @@ Array.fill(32)(0: Byte), ProverResult(Array.emptyByteArray, ContextExtension(Map())))
      new ErgoTransaction(IndexedSeq(input), IndexedSeq(new ErgoBoxCandidate(balance, script)))
    }

    val balance1 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance1))

    Thread.sleep(1000)

    val bf1 = w.unconfirmedBalances()
    val bs1 = Await.result(bf1, 1.second)
    bs1.balance shouldBe balance1
    bs1.assetBalances.isEmpty shouldBe true

    val balance2 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance2))

    Thread.sleep(1000)

    val bf2 = w.unconfirmedBalances()
    val bs2 = Await.result(bf2, 1.second)
    bs2.balance shouldBe (balance1 + balance2)
    bs2.assetBalances.isEmpty shouldBe true

    w.watchFor(ScriptAddress(Values.TrueLeaf))
    val balance3 = Random.nextInt(1000) + 1
    w.scanOffchain(makeTx(balance3, Values.TrueLeaf))

    Thread.sleep(1000)

    val bf3 = w.unconfirmedBalances()
    val bs3 = Await.result(bf3, 1.second)
    bs3.balance shouldBe (balance1 + balance2 + balance3)
    bs3.assetBalances.isEmpty shouldBe true

    //todo: enhance the test, e.g. add assets
  }

  property("Successfully scans an onchain transaction") {
  }

  property("Successfully doing a rollback") {
  }
}
