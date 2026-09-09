package org.ergoplatform.nodeView.state

import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.{ErgoCorePropertyTest, RandomWrapper}
import scorex.db.ByteArrayWrapper

class StateReopenAfterRollbackSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val BaseTimestamp = 1700000000000L

  private def nextBlock(parent: Option[ErgoFullBlock],
                        state: UtxoState,
                        holder: BoxHolder,
                        seed: Int,
                        timestamp: Long): (ErgoFullBlock, BoxHolder) = {
    scala.util.Random.setSeed(seed.toLong)
    val (transactions, nextHolder) =
      validTransactionsFromBoxHolder(holder, new RandomWrapper(Some(seed)))
    val block = validFullBlock(parent, state, transactions, Some(timestamp))
    block -> nextHolder
  }

  property("UTXO and Digest reopen the rollback checkpoint before any alternate-branch update") {
    val utxoDir = createTempDir
    val digestDir = createTempDir
    val (initialUtxo, initialHolder) =
      ErgoState.generateGenesisUtxoState(utxoDir, settings)
    var utxo = initialUtxo
    var digest =
      DigestState.create(Some(utxo.version), Some(utxo.rootDigest), digestDir, settings)

    try {
      val (g, holderG) =
        nextBlock(None, utxo, initialHolder, seed = 100, BaseTimestamp)
      utxo = utxo.applyModifier(g, None)(_ => ()).get
      digest = digest.applyModifier(g, None)(_ => ()).get
      val versionG = idToVersion(g.id)
      val rootG = g.header.stateRoot.clone()
      val utxoContextG = utxo.stateContext.bytes.clone()
      val digestContextG = digest.stateContext.bytes.clone()

      val (a1, holderA1) =
        nextBlock(Some(g), utxo, holderG, seed = 101, BaseTimestamp + 1)
      utxo = utxo.applyModifier(a1, None)(_ => ()).get
      digest = digest.applyModifier(a1, None)(_ => ()).get
      val (a2, holderA2) =
        nextBlock(Some(a1), utxo, holderA1, seed = 102, BaseTimestamp + 2)
      utxo = utxo.applyModifier(a2, None)(_ => ()).get
      digest = digest.applyModifier(a2, None)(_ => ()).get

      val aOnlyBox = holderA2.boxes.values
        .find(box => !holderG.boxes.contains(ByteArrayWrapper(box.id)))
        .get
      val gRestoredBox = holderG.boxes.values
        .find(box => !holderA2.boxes.contains(ByteArrayWrapper(box.id)))
        .get

      utxo.closeStorage()
      digest.close()
      utxo = UtxoState.create(utxoDir, settings)
      digest = DigestState.create(None, None, digestDir, settings)

      utxo.version shouldEqual idToVersion(a2.id)
      utxo.rootDigest shouldEqual a2.header.stateRoot
      utxo.stateContext.currentHeight shouldEqual a2.header.height
      utxo.boxById(aOnlyBox.id) shouldEqual Some(aOnlyBox)
      digest.version shouldEqual idToVersion(a2.id)
      digest.rootDigest shouldEqual a2.header.stateRoot
      digest.stateContext.currentHeight shouldEqual a2.header.height

      def assertRollbackCheckpoint(): Unit = {
        utxo.version shouldEqual versionG
        utxo.rootDigest shouldEqual rootG
        utxo.stateContext.currentHeight shouldEqual g.header.height
        utxo.stateContext.bytes shouldEqual utxoContextG
        utxo.boxById(gRestoredBox.id) shouldEqual Some(gRestoredBox)
        utxo.boxById(aOnlyBox.id) shouldBe empty
        digest.version shouldEqual versionG
        digest.rootDigest shouldEqual rootG
        digest.stateContext.currentHeight shouldEqual g.header.height
        digest.stateContext.bytes shouldEqual digestContextG
      }

      utxo = utxo.rollbackTo(versionG).get
      digest = digest.rollbackTo(versionG).get
      assertRollbackCheckpoint()

      utxo.closeStorage()
      digest.close()
      utxo = UtxoState.create(utxoDir, settings)
      digest = DigestState.create(None, None, digestDir, settings)
      assertRollbackCheckpoint()
      println(
        s"ROLLBACK_REOPEN_RESULT version=${g.id} root=${Algos.encode(rootG)} " +
          s"utxoContext=${Algos.encode(Algos.hash(utxo.stateContext.bytes))} " +
          s"digestContext=${Algos.encode(Algos.hash(digest.stateContext.bytes))}"
      )

      val (b1, holderB1) =
        nextBlock(Some(g), utxo, holderG, seed = 201, BaseTimestamp + 10)
      b1.id should not equal a1.id
      utxo = utxo.applyModifier(b1, None)(_ => ()).get
      digest = digest.applyModifier(b1, None)(_ => ()).get
      val (b2, holderB2) =
        nextBlock(Some(b1), utxo, holderB1, seed = 202, BaseTimestamp + 11)
      b2.id should not equal a2.id
      utxo = utxo.applyModifier(b2, None)(_ => ()).get
      digest = digest.applyModifier(b2, None)(_ => ()).get
      val bOnlyBox = holderB2.boxes.values
        .find(box => !holderG.boxes.contains(ByteArrayWrapper(box.id)))
        .get

      utxo.closeStorage()
      digest.close()
      utxo = UtxoState.create(utxoDir, settings)
      digest = DigestState.create(None, None, digestDir, settings)
      utxo.version shouldEqual idToVersion(b2.id)
      utxo.rootDigest shouldEqual b2.header.stateRoot
      utxo.stateContext.currentHeight shouldEqual b2.header.height
      utxo.boxById(bOnlyBox.id) shouldEqual Some(bOnlyBox)
      utxo.boxById(aOnlyBox.id) shouldBe empty
      digest.version shouldEqual idToVersion(b2.id)
      digest.rootDigest shouldEqual b2.header.stateRoot
      digest.stateContext.currentHeight shouldEqual b2.header.height
      println(
        s"ROLLBACK_CONTINUATION_RESULT g=${g.id} a1=${a1.id} a2=${a2.id} " +
          s"b1=${b1.id} b2=${b2.id} root=${Algos.encode(utxo.rootDigest)}"
      )
    } finally {
      try utxo.closeStorage() finally digest.close()
    }
  }
}
