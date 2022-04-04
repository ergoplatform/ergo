package scorex.testkit.properties.state

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.state.{DigestState, ErgoState}
import org.scalacheck.Gen
import scala.collection.mutable.ListBuffer


trait StateApplicationTest[ST <: ErgoState[ST]] extends StateTests[ST] {

  lazy val stateGenWithValidModifier: Gen[(ST, ErgoPersistentModifier)] = {
    stateGen.map { s => (s, semanticallyValidModifier(s)) }
  }

  lazy val stateGenWithInvalidModifier: Gen[(ST, ErgoPersistentModifier)] = {
    stateGen.map { s => (s, semanticallyInvalidModifier(s))}
  }

  private def propertyNameGenerator(propName: String): String = s"StateTests: $propName"

  property(propertyNameGenerator("apply modifier")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      val sTry = s.applyModifier(m, None)(_ => ())
        sTry.isSuccess shouldBe true
      sTry.get.version == ver shouldBe false
    }
  }

  property(propertyNameGenerator("do not apply same valid modifier twice")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      val sTry = s.applyModifier(m, None)(_ => ())
      sTry.isSuccess shouldBe true
      val s2 = sTry.get
      s2.version == ver shouldBe false
      s2.applyModifier(m, None)(_ => ()).isSuccess shouldBe false
    }
  }

  property(propertyNameGenerator("do not apply invalid modifier")) {
    forAll(stateGenWithInvalidModifier) { case (s, m) =>
      val sTry = s.applyModifier(m, None)(_ => ())
      sTry.isSuccess shouldBe false
    }
  }

  property(propertyNameGenerator("apply valid modifier after rollback")) {
    forAll(stateGenWithValidModifier) { case (s, m) =>
      val ver = s.version
      s.store.setKeepVersions(10)
      val sTry = s.applyModifier(m, Some(0))(_ => ())
      sTry.isSuccess shouldBe true
      val s2 = sTry.get
      s2.version == ver shouldBe false
      val ver2 = s2.version

      val s3 = s2.rollbackTo(ver).get
      s3.version == ver shouldBe true

      val sTry2 = s3.applyModifier(m, None)(_ => ())
      sTry2.isSuccess shouldBe true
      val s4 = sTry2.get
      s4.version == ver shouldBe false
      s4.version == ver2 shouldBe true
    }
  }

  property(propertyNameGenerator("application after rollback is possible")) {
    forAll(stateGen) { s =>
      s.store.setKeepVersions(10)
      val maxRollbackDepth = s match {
        case ds: DigestState =>
          ds.store.rollbackVersions().size
        case _ =>
          10
      }
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val rollbackDepth = Gen.chooseNum(1, maxRollbackDepth).sample.get
      val buf = new ListBuffer[ErgoPersistentModifier]()
      val ver = s.version

      val s2 = (0 until rollbackDepth).foldLeft(s) { case (state, _) =>
        val modifier = semanticallyValidModifier(state)
        buf += modifier
        val sTry = state.applyModifier(modifier, Some(rollbackDepth))(_ => ())
        sTry shouldBe 'success
        sTry.get
      }

      val lastVersion = s2.version
      val rollbackTry = s2.rollbackTo(ver)
      rollbackTry.toOption shouldBe defined
      val s3 = rollbackTry.get
      s3.version == ver shouldBe true

      val s4 = buf.foldLeft(s3) { case (state, m) =>
        val sTry = state.applyModifier(m, Some(0))(_ => ())
        sTry shouldBe 'success
        sTry.get
      }

      s4.version == lastVersion shouldBe true
    }
  }
}
