package org.ergoplatform.nodeView.state

import java.io.File
import java.nio.file.Files

import org.ergoplatform.core.bytesToVersion
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import scorex.crypto.authds.ADDigest
import scorex.db.LDBVersionedStore

import scala.util.{Failure, Try}

class DigestStateRollbackFailureSpec extends ErgoCorePropertyTest {
  private val previousVersion = Array.fill[Byte](32)(1)
  private val currentVersion = Array.fill[Byte](32)(2)
  private val previousRoot = Array.fill[Byte](33)(3)
  private val currentRoot = Array.fill[Byte](33)(4)

  private class ControlledStore(directory: File) extends LDBVersionedStore(directory, initialKeepVersions = 10) {
    var rollbackOutcome: Option[() => Try[Unit]] = None
    var cleanCalls = 0
    var rootReads = 0
    var rollbackCalls = 0

    override def rollbackTo(versionID: VersionID): Try[Unit] = {
      rollbackCalls += 1
      rollbackOutcome.map(_()).getOrElse(super.rollbackTo(versionID))
    }

    override def clean(count: Int): Unit = {
      cleanCalls += 1
      super.clean(count)
    }

    override def get(key: Array[Byte]): Option[Array[Byte]] = {
      if (key.sameElements(previousVersion)) rootReads += 1
      super.get(key)
    }
  }

  private class TestDigestState(storage: LDBVersionedStore)
    extends DigestState(bytesToVersion(currentVersion), ADDigest @@ currentRoot, storage, settings)

  private def withState(test: (DigestState, ControlledStore) => Unit): Unit = {
    val store = new ControlledStore(Files.createTempDirectory("digest-rollback").toFile)
    try {
      store.update(previousVersion, Seq.empty, Seq(previousVersion -> previousRoot)).get
      store.update(currentVersion, Seq.empty, Seq(currentVersion -> currentRoot)).get
      val state = new TestDigestState(store)
      store.cleanCalls = 0
      store.rootReads = 0
      test(state, store)
    } finally store.close()
  }

  for (throwsFailure <- Seq(false, true)) {
    property(s"digest rollback preserves the original ${if (throwsFailure) "thrown" else "returned"} store failure") {
      withState { (state, store) =>
        val error = new IllegalStateException("local digest rollback failure")
        store.rollbackOutcome = Some(() => if (throwsFailure) throw error else Failure(error))

        val result = state.rollbackTo(bytesToVersion(previousVersion))

        result.failed.get should be theSameInstanceAs error
        store.rollbackCalls shouldBe 1
        store.cleanCalls shouldBe 0
        store.rootReads shouldBe 0
        store.lastVersionID.get shouldEqual currentVersion
        state.version shouldBe bytesToVersion(currentVersion)
        state.rootDigest shouldEqual currentRoot
      }
    }
  }

  property("digest rollback cleans and reads the restored root after store success") {
    withState { (state, store) =>
      val restored = state.rollbackTo(bytesToVersion(previousVersion)).get

      store.rollbackCalls shouldBe 1
      store.cleanCalls shouldBe 1
      store.rootReads shouldBe 1
      restored.version shouldBe bytesToVersion(previousVersion)
      restored.rootDigest shouldEqual previousRoot
      store.lastVersionID.get shouldEqual previousVersion
    }
  }
}
