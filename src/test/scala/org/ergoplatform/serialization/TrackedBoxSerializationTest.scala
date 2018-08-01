package org.ergoplatform.serialization

import org.ergoplatform.nodeView.wallet.TrackedBoxSerializer.TransactionLookup
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}
import org.scalacheck.Gen
import org.scalatest.Assertion
import scorex.core.serialization.Serializer

import scala.util.Success

class TrackedBoxSerializationTest extends ErgoPropertyTest with WalletGenerators {

  property("UnspentOffchainBox serialization") {
    checkBoxSerialization(unspentOffchainBoxGen, new UnspentOffchainBoxSerializer(_))
  }

  property("UnspentOnchainBoxSerializer serialization") {
    checkBoxSerialization(unspentOnchainBoxGen, new UnspentOnchainBoxSerializer(_))
  }

  property("SpentOffchainBoxSerializer serialization") {
    checkBoxSerialization(spentOffchainBoxGen, new SpentOffchainBoxSerializer(_))
  }

  property("SpentOnchainBoxSerializer serialization") {
    checkBoxSerialization(spentOnchainBoxGen, new SpentOnchainBoxSerializer(_))
  }

  property("TrackedBox serialization") {
    checkBoxSerialization(trackedBoxGen, new TrackedBoxSerializer(_))
  }

  def checkBoxSerialization[T <: TrackedBox](gen: Gen[T],
                                             serializerFactory: TransactionLookup => Serializer[T]): Assertion = {
    forAll(gen) { box =>
      val serializer = serializerFactory(txLookup(box))
      val bytes = serializer.toBytes(box)
      val recovered = serializer.parseBytes(bytes)
      recovered shouldBe Success(box)
    }
  }

  def txLookup(box: TrackedBox): TransactionLookup = box match {
    case unspentBox: UnspentBox =>
      import unspentBox._
      Map(creationTx.id -> creationTx).get(_)

    case spentBox: SpentBox =>
      import spentBox._
      Map(creationTx.id -> creationTx, spendingTx.id -> spendingTx).get(_)
  }

}
