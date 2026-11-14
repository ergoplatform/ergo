package org.ergoplatform.utils.fixtures

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestContext, NodeViewTestOps}
import scorex.db.{LDBFactory, StoreRegistry}

import java.io.File

class NodeViewFixtureLifecycleSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private def openStores(directory: File): Int = {
    val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
    registry.lock.readLock().lock()
    try registry.map.keys.count(_.toPath.toAbsolutePath.normalize()
      .startsWith(directory.toPath.toAbsolutePath.normalize()))
    finally registry.lock.readLock().unlock()
  }

  for (bodyFails <- Seq(false, true)) {
    property(s"release fixture stores and terminate actors when bodyFails=$bodyFails") {
      val config = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
        popowBootstrap = false).toSettings
      val fixture = new NodeViewFixture(config, parameters)
      val unrelatedDir = java.nio.file.Files.createTempDirectory("unrelated-leveldb-").toFile
      val unrelated = LDBFactory.createKvDb(unrelatedDir.toString)
      val failure = new IllegalStateException("injected test body failure")
      try {
        val run = () => fixture.apply { active =>
          implicit val ctx: NodeViewTestContext = active
          getCurrentView.history.headersHeight shouldBe 0
          // Await wallet initialization before testing cleanup, including stores that
          // are not explicitly released by the wallet actor's postStop.
          scala.concurrent.Await.result(getCurrentView.vault.confirmedBalances,
            scala.concurrent.duration.Duration(20, "seconds"))
          openStores(active.nodeViewDir) should be > 0
          if (bodyFails) throw failure
        }
        if (bodyFails) intercept[IllegalStateException](run()) shouldBe failure
        else run()
        fixture.actorSystem.whenTerminated.isCompleted shouldBe true
        openStores(fixture.nodeViewDir) shouldBe 0
        openStores(unrelatedDir) shouldBe 1
      } finally unrelated.close()
    }
  }
}
