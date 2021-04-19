package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, Input, UnsignedInput}
import org.ergoplatform.ErgoBox.{AdditionalRegisters, TokenId}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletVars}
import org.ergoplatform.utils.ErgoTestConstants
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.interpreter.{ErgoProvingInterpreter, TransactionHintsBag}
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.interpreter.{ContextExtension, ProverResult}

object WalletRegistryBenchmark extends App with ErgoTestConstants {

  def createBox(value: Long,
                ergoTree: ErgoTree,
                creationHeight: Int,
                additionalTokens: Seq[(TokenId, Long)] = Nil,
                additionalRegisters: AdditionalRegisters = Map.empty,
                transactionId: ModifierId = ErgoBox.allZerosModifierId,
                boxIndex: Short = 0): ErgoBox = {
    import sigmastate.eval._
    new ErgoBox(value, ergoTree,
      CostingSigmaDslBuilder.Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)
  }

  implicit val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

  val registry = WalletRegistry(settings)
  val storage = WalletStorage.readOrCreate(settings)(enc)

  val rootSecret = ExtendedSecretKey.deriveMasterKey(Array.fill(32)(0: Byte))

  val derivedSecrets = (1 to 15000).map { i =>
    val k = rootSecret.derive(DerivationPath.fromEncoded(s"m/44'/429'/0'/0/$i").get)
    storage.addKey(k.publicKey)
    k
  }

  val prover = ErgoProvingInterpreter(rootSecret +: derivedSecrets, parameters)
  var walletVars = WalletVars.apply(storage, settings).withProver(prover)

  val boxes = walletVars.proverOpt.get.hdPubKeys.map { pk =>
    createBox(1000000000, pk.key, 1)
  }.map { box =>
    TrackedBox(box, 2, Set(Constants.PaymentsScanId))
  }

  val scanResults0 = ScanResults(boxes, Seq.empty, Seq.empty)
  registry.updateOnBlock(scanResults0, ModifierId @@ Base16.encode(Array.fill(32)(0: Byte)), 1)
  println("keys: " + walletVars.proverOpt.get.secretKeys.size)

  val bts0 = System.currentTimeMillis()
  val boxesRead = registry.unspentBoxes(Constants.PaymentsScanId)
  val bts = System.currentTimeMillis()
  println("boxes read: " + boxesRead.size)
  println("boxes read time: " + (bts - bts0) + " ms")

  val stateContext = storage.readStateContext

  val txs = boxes.map { tb =>
    val bx = tb.box
    val input = new Input(bx.id, ProverResult(Array.fill(64)(0: Byte), ContextExtension.empty))
    val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(bx.toCandidate))
    WalletTransaction(tx, 2, Seq(Constants.PaymentsScanId))
  }

  val scanResults1 = ScanResults(Seq.empty, Seq.empty, txs)
  registry.updateOnBlock(scanResults1, ModifierId @@ Base16.encode(Array.fill(32)(1: Byte)), 2)

  val tts0 = System.currentTimeMillis()
  val txsRead = registry.allWalletTxs()
  val tts = System.currentTimeMillis()
  println("txs read: " + txsRead.size)
  println("txs read time: " + (tts - tts0) + " ms")

}
