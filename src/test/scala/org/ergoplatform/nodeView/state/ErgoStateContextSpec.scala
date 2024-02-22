package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
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

    // https://github.com/ergoplatform/ergo/issues/2114
    // this fails sporadically, when `(imvValue._1.head == 0)`, because less value bytes will be generated
    // by extensionKvGen(). Workaround is to just generate again while `(imvValue._1.head == 0)`
    // TODO: document, and possibly rewrite/replace extensionKvGen after the above issues are clarified/solved
    //       https://github.com/ergoplatform/ergo/issues/2118

    // validation of field value sizes
    var imvValue = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize + 1).sample.get
    while (imvValue._1.head == 0) {
        imvValue = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize + 1).sample.get
    }
    sc.appendFullBlock(fbWithFields(imvValue +: oldFields)) shouldBe 'failure

    // validation of incorrect interlinks
    val invalidInterlinks = nipopowAlgos.interlinksToExtension(
      NipopowAlgos.unpackInterlinks(fb.extension.fields).get ++ Seq(fb.header.id)
    ).fields
    sc.appendFullBlock(fbWithFields(invalidInterlinks ++ oldFields)) shouldBe 'failure

    // https://github.com/ergoplatform/ergo/issues/2114
    // if validMKV._1.head is 1, appendFullBlock within "valid application of correct extension" will fail,
    // because with "key.head == 1", improperly packed interlink would be generated.
    // As a workaround, just generate new values until (validMKV._1.head != 1)
    // TODO: investigate and provide a full fix (followup issue)
    //       https://github.com/ergoplatform/ergo/issues/2117
    var validMKV = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize).sample.get
    while (validMKV._1.head == 1) {
      validMKV = extensionKvGen(Extension.FieldKeySize, Extension.FieldValueMaxSize).sample.get
    }
    // validation of key duplicates in fields
    sc.appendFullBlock(fbWithFields(Seq(validMKV, validMKV) ++ oldFields)) shouldBe 'failure

    // valid application of correct extension
    sc.appendFullBlock(fbWithFields(validMKV +: oldFields)) shouldBe 'success

  }

}
