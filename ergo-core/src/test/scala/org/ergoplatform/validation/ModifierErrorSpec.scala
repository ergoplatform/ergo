package org.ergoplatform.validation

import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.generators.CoreObjectGenerators
import org.scalacheck.Gen
import scorex.util.ModifierId

class ModifierErrorSpec extends ErgoCorePropertyTest {

  import CoreObjectGenerators._

  property("ParentHeaderNotFoundError should extend RecoverableModifierError") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error shouldBe a[RecoverableModifierError]
    }
  }

  property("ParentHeaderNotFoundError should not be fatal") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.isFatal shouldBe false
    }
  }

  property("ParentHeaderNotFoundError should store parent header id") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.parentId shouldBe parentId
    }
  }

  property("ParentHeaderNotFoundError message should contain parent id") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.message should include(parentId.toString)
      error.message should include("Parent header")
    }
  }

  property("ParentHeaderNotFoundError should have correct modifierId and modifierTypeId") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.modifierId shouldBe modifierId
      error.modifierTypeId shouldBe modifierTypeId
    }
  }

  property("ParentHeaderNotFoundError should be a NoStackTrace exception") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.getStackTrace shouldBe empty
    }
  }

  property("ParentHeaderNotFoundError info should indicate recoverable failure") {
    forAll(modifierIdGen, modifierIdGen, modifierTypeIdGen) { (parentId: ModifierId, modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value) =>
      val error = new ParentHeaderNotFoundError(parentId, modifierId, modifierTypeId)
      error.info should include("recoverably")
      error.info should include(modifierId.toString)
      error.info should include(modifierTypeId.toString)
    }
  }

  property("MalformedModifierError should be fatal") {
    forAll(modifierIdGen, modifierTypeIdGen, Gen.alphaNumStr) { (modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, message: String) =>
      val error = new MalformedModifierError(message, modifierId, modifierTypeId)
      error.isFatal shouldBe true
    }
  }

  property("RecoverableModifierError should not be fatal") {
    forAll(modifierIdGen, modifierTypeIdGen, Gen.alphaNumStr) { (modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, message: String) =>
      val error = new RecoverableModifierError(message, modifierId, modifierTypeId)
      error.isFatal shouldBe false
    }
  }

  property("MultipleErrors should be fatal if any contained error is fatal") {
    forAll(modifierIdGen, modifierTypeIdGen, Gen.alphaNumStr) { (modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, message: String) =>
      val fatalError = new MalformedModifierError(message, modifierId, modifierTypeId)
      val recoverableError = new RecoverableModifierError(message, modifierId, modifierTypeId)

      val multipleFatal = MultipleErrors(Seq(fatalError, recoverableError))
      multipleFatal.isFatal shouldBe true

      val multipleRecoverable = MultipleErrors(Seq(recoverableError, recoverableError))
      multipleRecoverable.isFatal shouldBe false
    }
  }

  property("ModifierError info should contain correct fatality indicator") {
    forAll(modifierIdGen, modifierTypeIdGen, Gen.alphaNumStr) { (modifierId: ModifierId, modifierTypeId: NetworkObjectTypeId.Value, message: String) =>
      val fatalError = new MalformedModifierError(message, modifierId, modifierTypeId)
      fatalError.info should include("fatally")

      val recoverableError = new RecoverableModifierError(message, modifierId, modifierTypeId)
      recoverableError.info should include("recoverably")
    }
  }

}
