package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history.popow.NipopowProofSerializer
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.wallet.persistence.WalletDigestSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.nipopowAlgos
import org.ergoplatform.utils.generators.ErgoNodeGenerators.poPowProofGen

class SerializationTests extends ErgoCorePropertyTest with org.ergoplatform.utils.SerializationTests {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  property("Serializers should be defined for all block sections") {
    val block = invalidErgoFullBlockGen.sample.get
    block.toSeq.foreach { s =>
      ErgoNodeViewSynchronizer.modifierSerializers.get(s.modifierTypeId) should not be None
    }
  }

  property("WalletDigest serialization") {
    forAll(registrySummaryGen) { index =>
      WalletDigestSerializer.parseBytes(WalletDigestSerializer.toBytes(index)) shouldEqual index
    }
  }

  property("PoPowProof serialization") {
    checkSerializationRoundtrip(poPowProofGen, new NipopowProofSerializer(nipopowAlgos))
  }

}
