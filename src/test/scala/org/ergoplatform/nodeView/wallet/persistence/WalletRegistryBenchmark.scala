package org.ergoplatform.nodeView.wallet.persistence

import org.ergoplatform.ErgoBox.{AdditionalRegisters, TokenId}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.{WalletTransaction, WalletVars}
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, ExtendedSecretKey}
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.boxes.TrackedBox
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, Input}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.ast.ErgoTree
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.ast.ErgoTree
import sigma.data.CSigmaDslBuilder

import scala.collection.compat.immutable.ArraySeq

object WalletRegistryBenchmark extends App {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  def createBox(value: Long,
                ergoTree: ErgoTree,
                creationHeight: Int,
                additionalTokens: Seq[(TokenId, Long)] = Nil,
                additionalRegisters: AdditionalRegisters = Map.empty,
                transactionId: ModifierId = ErgoBox.allZerosModifierId,
                boxIndex: Short = 0): ErgoBox = {
    new ErgoBox(value, ergoTree,
      CSigmaDslBuilder.Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)
  }

  implicit val enc = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)

  val registry = WalletRegistry(settings).get
  val storage = WalletStorage.readOrCreate(settings)

  val rootSecret = ExtendedSecretKey.deriveMasterKey(Array.fill(32)(0: Byte), usePre1627KeyDerivation = false)

  val derivedSecrets = (1 to 15000).map { i =>
    val k = rootSecret.derive(DerivationPath.fromEncoded(s"m/44'/429'/0'/0/$i").get)
    storage.addPublicKey(k.publicKey).get
    k
  }

  val prover = ErgoProvingInterpreter(rootSecret +: derivedSecrets, parameters)
  val walletVars = WalletVars.apply(storage, settings).withProver(prover)

  val boxes = walletVars.proverOpt.get.hdPubKeys.map { pk =>
    createBox(1000000000, ErgoTree.fromSigmaBoolean(pk.key), 1)
  }.map { box =>
    TrackedBox(box, 2, Set(Constants.PaymentsScanId))
  }

  val scanResults0 = ScanResults(boxes, ArraySeq.empty, ArraySeq.empty)
  registry.updateOnBlock(scanResults0, ModifierId @@ Base16.encode(Array.fill(32)(0: Byte)), 1).get
  println("keys: " + walletVars.proverOpt.get.secretKeys.size)

  val bts0 = System.currentTimeMillis()
  val boxesRead = registry.unspentBoxes(Constants.PaymentsScanId)
  val bts = System.currentTimeMillis()
  println("boxes read: " + boxesRead.size)
  println("boxes read time: " + (bts - bts0) + " ms")

  val stateContext = storage.readStateContext(parameters)

  val txs = boxes.map { tb =>
    val bx = tb.box
    val input = new Input(bx.id, ProverResult(Array.fill(64)(0: Byte), ContextExtension.empty))
    val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(bx.toCandidate))
    WalletTransaction(tx, 2, Seq(Constants.PaymentsScanId))
  }

  val scanResults1 = ScanResults(ArraySeq.empty, ArraySeq.empty, txs)
  registry.updateOnBlock(scanResults1, ModifierId @@ Base16.encode(Array.fill(32)(1: Byte)), 2).get

  val tts0 = System.currentTimeMillis()
  val txsRead = registry.allWalletTxs()
  val tts = System.currentTimeMillis()
  println("txs read: " + txsRead.size)
  println("txs read time: " + (tts - tts0) + " ms")

}
