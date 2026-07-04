package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.settings.Constants
import org.ergoplatform.settings.Parameters._
import org.ergoplatform.utils.ErgoCorePropertyTest

class ErgoStateContextSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._

  property("Header votes") {
    val fb = genChain(1).head
    val header = fb.header

    def fbWithVotes(votes: Array[Byte], h: Int = 1): ErgoFullBlock = {
      val newHeader = header.copy(votes = votes, version = 0: Byte, height = h)
      fb.copy(header = newHeader)
    }

    //double vote
    val wrongVotes1 = Array(StorageFeeFactorIncrease, StorageFeeFactorIncrease, NoParameter)
    emptyStateContext.appendFullBlock(fbWithVotes(wrongVotes1)) shouldBe 'failure

    //contradictory votes
    val wrongVotes2 = Array(StorageFeeFactorIncrease, StorageFeeFactorDecrease, NoParameter)
    emptyStateContext.appendFullBlock(fbWithVotes(wrongVotes2)) shouldBe 'failure

    //too many votes - only two ordinary changes allowed per epoch
    val wrongVotes3 = Array(StorageFeeFactorIncrease, MaxBlockCostIncrease, MaxBlockSizeDecrease)
    emptyStateContext.appendFullBlock(fbWithVotes(wrongVotes3)) shouldBe 'failure

    //a vote proposed on non-existing parameter - breaks rule #215
    //voting epoch length is 1024 blocks long
    val wrongVotes4 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    emptyStateContext.appendFullBlock(fbWithVotes(wrongVotes4, 1024)) shouldBe 'failure

    //correct votes
    val correctVotes = Array(StorageFeeFactorIncrease, MaxBlockSizeDecrease, NoParameter)
    emptyStateContext.appendFullBlock(fbWithVotes(correctVotes)) shouldBe 'success


    //a vote for non-existing parameter in the middle of epoch - does not break rule #215
    //voting epoch length is 1024 blocks long
    val correctVotes2 = Array((-50).toByte, NoParameter, MaxBlockSizeDecrease)
    emptyStateContext.appendFullBlock(fbWithVotes(correctVotes2, 2)) shouldBe 'success

  }

  property("Extension validation") {
    val chain = genChain(2)
    val sc = emptyStateContext.appendFullBlock(chain.head).get
    val fb = chain.last
    val extension = fb.extension
    val oldFields = extension.fields

    def fbWithFields(newFields: Seq[(Array[Byte], Array[Byte])]): ErgoFullBlock = {
      val newExtension = extension.copy(fields = newFields)
      fb.copy(extension = newExtension)
    }

    // checks, specific for extension
    // validation of field keys size
    val imvKey = extensionKvGen(Extension.FieldKeySize - 1, Extension.FieldValueMaxSize).sample.get
    sc.appendFullBlock(fbWithFields(imvKey +: oldFields)) shouldBe 'failure

    // validation of field value sizes
    val imvValue = extensionKvGenImvValue(Extension.FieldKeySize, Extension.FieldValueMaxSize + 1).sample.get
    sc.appendFullBlock(fbWithFields(imvValue +: oldFields)) shouldBe 'failure

    // validation of incorrect interlinks
    val invalidInterlinks = nipopowAlgos.interlinksToExtension(
      NipopowAlgos.unpackInterlinks(fb.extension.fields).get ++ Seq(fb.header.id)
    ).fields
    sc.appendFullBlock(fbWithFields(invalidInterlinks ++ oldFields)) shouldBe 'failure

    val validMKV = extensionKvGenValidMKV(Extension.FieldKeySize, Extension.FieldValueMaxSize).sample.get
    // validation of key duplicates in fields
    sc.appendFullBlock(fbWithFields(Seq(validMKV, validMKV) ++ oldFields)) shouldBe 'failure

    // valid application of correct extension
    sc.appendFullBlock(fbWithFields(validMKV +: oldFields)) shouldBe 'success

  }

  property("Block with malformed extension application") {
    val chain = genChain(2)
    val sc = emptyStateContext.appendFullBlock(chain.head).get
    val fb = chain.last
    val extension = fb.extension
    val oldFields = extension.fields

    def fbWithFields(newFields: Seq[(Array[Byte], Array[Byte])]): ErgoFullBlock = {
      val newExtension = extension.copy(fields = newFields)
      fb.copy(extension = newExtension)
    }

    fb.header.isGenesis shouldBe false

    // rule 400 exSize - serialized extension > Constants.MaxExtensionSize.
    // Each field contributes 2 (key) + 1 (length) + 64 (value) = 67 bytes. 500 distinct
    // 2-byte keys with max-size values (>= 33500 bytes) clear the 32 KiB cap with margin.
    // Bloat keys must steer clear of reserved prefixes so structural validation passes
    // and exSize is the rule that fires.
    val reservedPrefixes: Set[Byte] = Set(
      Extension.SystemParametersPrefix,
      Extension.InterlinksVectorPrefix,
      Extension.ValidationRulesPrefix
    )
    val bloatFields: Seq[(Array[Byte], Array[Byte])] = (0 until 500).map { i =>
      val keyHigh: Byte = if (i < 256) 0x05.toByte else 0x06.toByte
      val keyLow = (i % 256).toByte
      (Array(keyHigh, keyLow), Array.fill[Byte](Extension.FieldValueMaxSize)(0))
    }
    bloatFields.map(_._1.head).toSet.intersect(reservedPrefixes) shouldBe empty
    val bloatBytes = bloatFields.size * (Extension.FieldKeySize + 1 + Extension.FieldValueMaxSize)
    bloatBytes should be > Constants.MaxExtensionSize
    sc.appendFullBlock(fbWithFields(bloatFields ++ oldFields)) shouldBe 'failure

    // rule 401 exIlEncoding - interlink-prefixed field whose value is not 33 bytes,
    // so NipopowAlgos.unpackInterlinks fails. Strip the existing interlink entries
    // so the only 0x01-prefixed field is our malformed one (no duplicate-key check
    // can fire first).
    val nonInterlinkFields =
      oldFields.filterNot(_._1.headOption.contains(Extension.InterlinksVectorPrefix))
    val badInterlink: (Array[Byte], Array[Byte]) =
      (Array(Extension.InterlinksVectorPrefix, 0.toByte), Array.fill[Byte](10)(0))
    sc.appendFullBlock(fbWithFields(badInterlink +: nonInterlinkFields)) shouldBe 'failure

    // rule 403 exKeyLength - key longer than FieldKeySize
    val oversizeKey = extensionKvGen(Extension.FieldKeySize + 1, Extension.FieldValueMaxSize).sample.get
    sc.appendFullBlock(fbWithFields(oversizeKey +: oldFields)) shouldBe 'failure

    // rule 406 exEmpty - non-genesis block must have non-empty extension
    sc.appendFullBlock(fbWithFields(Seq.empty[(Array[Byte], Array[Byte])])) shouldBe 'failure

    // positive control - the unmutated block still validates, so the failures above
    // are attributable to the mutations rather than the fixture
    sc.appendFullBlock(fb) shouldBe 'success
  }

}
