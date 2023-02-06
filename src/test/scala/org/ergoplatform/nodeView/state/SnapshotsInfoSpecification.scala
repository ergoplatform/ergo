package org.ergoplatform.nodeView.state

import org.ergoplatform.utils.ErgoPropertyTest

class SnapshotsInfoSpecification extends ErgoPropertyTest {

 property("makeEmpty / nonEmpty / withNewManifest") {
   val empty = SnapshotsInfo.empty
   empty.nonEmpty shouldBe false

   val h = 10
   val d = digest32Gen.sample.get
   val nonEmpty = empty.withNewManifest(h, d)
   nonEmpty.nonEmpty shouldBe true
   nonEmpty.availableManifests(h).sameElements(d) shouldBe true

   val h2 = 20
   val d2 = digest32Gen.sample.get
   val ne2 = nonEmpty.withNewManifest(h2, d2)
   nonEmpty.availableManifests.size shouldBe 1
   ne2.availableManifests(h).sameElements(d) shouldBe true
   ne2.availableManifests(h2).sameElements(d2) shouldBe true
 }

}
