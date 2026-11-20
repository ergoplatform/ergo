package org.ergoplatform.db

import java.io.File

import org.ergoplatform.settings.Algos.HF
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, PersistentBatchAVLProver, VersionedLDBAVLStorage}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.utils.Random

class VersionedStoreSpec extends AnyPropSpec with Matchers with DBSpec {

  private val (keyA, valA) = (byteString("A"), byteString("1"))
  private val (keyB, valB) = (byteString("B"), byteString("2"))
  private val (keyC, valC) = (byteString("C"), byteString("3"))
  private val (keyD, valD) = (byteString("D"), byteString("4"))
  private val (keyE, valE) = (byteString("E"), byteString("5"))

  private val v1 = versionId("v1")
  private val v2 = versionId("v2")
  private val v3 = versionId("v3")
  private val v4 = versionId("v4")

  property("rollback (1 version back)") {
    withVersionedStore(10) { store =>
      store.insert(v1, Seq(keyA -> valA)).get
      store.insert(v2, Seq(keyB -> valB, keyC -> valC)).get
      store.update(v3, Seq(keyC), Seq(keyA -> byteString("6"), keyD -> valD)).get

      store.getAll.toSeq.toBs should contain allElementsOf Seq(
        keyA -> byteString("6"),
        keyB -> valB,
        keyD -> valD
      ).toBs

      store.rollbackTo(v2) shouldBe 'success

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC).toBs
      store.get(keyD) shouldBe None
      store.get(keyA).get.sameElements(valA) shouldBe true

      store.lastVersionID shouldBe Some(v2)
    }
  }

  property("rollback (2 versions back)") {
    withVersionedStore(10) { store =>
      store.insert(v1, Seq(keyA -> valA)).get
      store.insert(v2, Seq(keyB -> valB, keyC -> valC)).get
      store.update(v3, Seq(keyC), Seq(keyA -> byteString("6"), keyD -> valD)).get
      store.update(v4, Seq(keyA), Seq(keyB -> byteString("7"), keyE -> valE)).get

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyB -> byteString("7"), keyE -> valE, keyD -> valD).toBs
      store.get(keyC) shouldBe None
      store.get(keyA) shouldBe None

      store.rollbackTo(v2) shouldBe 'success

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC).toBs
      store.get(keyE) shouldBe None
      store.get(keyD) shouldBe None
    }
  }

  property("Outdated versions pruning") {
    withVersionedStore(2) { store =>
      store.insert(v1, Seq(keyA -> valA)).get
      store.insert(v2, Seq(keyB -> valB)).get

      store.versionIdExists(v1) shouldBe true

      store.insert(v3, Seq(keyC -> valC)).get
      store.insert(v4, Seq(keyA -> valA)).get

      store.versionIdExists(v4) shouldBe true
      store.versionIdExists(v3) shouldBe true
      store.versionIdExists(v2) shouldBe true
      store.versionIdExists(v1) shouldBe false

      store.get(keyA).isDefined shouldBe true
      store.get(keyB).isDefined shouldBe true
      store.get(keyC).isDefined shouldBe true

      store.rollbackTo(v1).isSuccess shouldBe false

      store.get(keyA).isDefined shouldBe true
      store.get(keyB).isDefined shouldBe true
      store.get(keyC).isDefined shouldBe true
    }
  }

  // #1631: restarts after the state store ran with keepVersions = 0, either switched to it at runtime (UtxoState
  // calls setKeepVersions(0) while far behind the tip) or configured with it; a node reopens the store with its
  // configured keepVersions

  // a version id and the value it writes under keyA
  private case class Written(id: Array[Byte], value: Array[Byte])

  private def written(prefix: String, n: Int): Seq[Written] =
    (1 to n).map(i => Written(versionId(s"$prefix$i"), byteString(s"value$i")))

  private def writeVersions(store: LDBVersionedStore, versions: Seq[Written]): Unit =
    versions.foreach(w => store.update(w.id, Seq.empty, Seq(keyA -> w.value)).get)

  // a prover created as UtxoState.create creates it: PersistentBatchAVLProver.create requires
  // storage.version == avlProver.digest
  private def withProverAt[T](dir: File, keepVersions: Int)
                             (body: (LDBVersionedStore, PersistentBatchAVLProver[Digest32, HF]) => T): T =
    withVersionedStore(keepVersions, dir) { store =>
      val bp = new BatchAVLProver[Digest32, HF](keyLength = 32, valueLengthOpt = None)
      body(store, PersistentBatchAVLProver.create(bp, new VersionedLDBAVLStorage(store)).get)
    }

  private def insertRandom(prover: PersistentBatchAVLProver[Digest32, HF], n: Int): Unit =
    (1 to n).foreach { _ =>
      prover.performOneOperation(Insert(ADKey @@ Random.randomBytes(32), ADValue @@ Random.randomBytes(8))).get
      prover.generateProofAndUpdateStorage()
    }

  property("reopened store keeps the last version written after setKeepVersions(0)") {
    val dir = createTempDir
    val versions = written("r", 5)
    withVersionedStore(10, dir) { store =>
      writeVersions(store, versions.take(3))
      store.setKeepVersions(0)
      writeVersions(store, versions.drop(3))
    }
    withVersionedStore(10, dir) { store =>
      store.lastVersionID.map(_.toSeq) shouldBe Some(versions.last.id.toSeq)
      store.rollbackTo(versions.last.id) shouldBe 'success
      store.get(keyA).map(_.toSeq) shouldBe Some(versions.last.value.toSeq)
    }
  }

  property("persistent prover restarts after the store switched to keepVersions = 0") {
    val dir = createTempDir
    val digest = withProverAt(dir, 10) { (store, prover) =>
      insertRandom(prover, 3)
      store.setKeepVersions(0)
      insertRandom(prover, 2)
      prover.digest
    }
    withProverAt(dir, 10) { (_, restored) => restored.digest.toSeq shouldBe digest.toSeq }
  }

  property("store reopened with keepVersions = 0 over earlier undo records keeps the last version") {
    val dir = createTempDir
    val versions = written("c", 5)
    withVersionedStore(10, dir)(writeVersions(_, versions.take(3)))
    withVersionedStore(0, dir)(writeVersions(_, versions.drop(3)))
    withVersionedStore(0, dir) { store =>
      store.lastVersionID.map(_.toSeq) shouldBe Some(versions.last.id.toSeq)
      store.get(keyA).map(_.toSeq) shouldBe Some(versions.last.value.toSeq)
    }
  }

  property("persistent prover restarts with keepVersions = 0 over earlier undo records") {
    val dir = createTempDir
    withProverAt(dir, 10)((_, prover) => insertRandom(prover, 3))
    val digest = withProverAt(dir, 0) { (_, prover) =>
      insertRandom(prover, 2)
      prover.digest
    }
    withProverAt(dir, 0) { (_, restored) => restored.digest.toSeq shouldBe digest.toSeq }
  }

  property("store reopened with keepVersions > 0 keeps its rollback history") {
    val dir = createTempDir
    val versions = written("k", 3)
    withVersionedStore(10, dir)(writeVersions(_, versions))
    withVersionedStore(10, dir) { store =>
      store.rollbackTo(versions.head.id) shouldBe 'success
      store.lastVersionID.map(_.toSeq) shouldBe Some(versions.head.id.toSeq)
      store.get(keyA).map(_.toSeq) shouldBe Some(versions.head.value.toSeq)
    }
  }

  // a store opened with 0, then with keepVersions > 0, then with 0 again: its last_version_key still names the
  // version written in the first period, and the undo records name the later ones
  property("store reopened with keepVersions = 0 after a keepVersions > 0 period keeps the later version") {
    val dir = createTempDir
    val versions = written("z", 5)
    withVersionedStore(0, dir)(writeVersions(_, versions.take(2)))
    withVersionedStore(10, dir)(writeVersions(_, versions.drop(2)))
    (1 to 2).foreach { i =>
      withClue(s"open $i: ") {
        withVersionedStore(0, dir) { store =>
          store.lastVersionID.map(_.toSeq) shouldBe Some(versions.last.id.toSeq)
          store.get(keyA).map(_.toSeq) shouldBe Some(versions.last.value.toSeq)
        }
      }
    }
  }

  private def replaceSubDir(srcDir: File, dstDir: File): Unit = {
    def deleteRecursively(f: File): Unit = {
      Option(f.listFiles()).foreach(_.foreach(deleteRecursively))
      f.delete()
    }
    def copyRecursively(src: File, dst: File): Unit = {
      if (src.isDirectory) {
        dst.mkdirs()
        Option(src.listFiles()).foreach(_.foreach(child => copyRecursively(child, new File(dst, child.getName))))
      } else {
        java.nio.file.Files.copy(src.toPath, dst.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
    }
    deleteRecursively(dstDir)
    copyRecursively(srcDir, dstDir)
  }

  // #2859: process stop inside LDBVersionedStore.update (after ldb_undo write, before ldb_main write)
  property("persistent prover restarts after a stop between ldb_undo and ldb_main writes in update") {
    val dir = createTempDir
    val backupDir = createTempDir

    val (firstDigest, committedDigest) = withProverAt(dir, 10) { (_, prover) =>
      insertRandom(prover, 1)
      val d1 = prover.digest
      insertRandom(prover, 1)
      (d1, prover.digest)
    }
    replaceSubDir(new File(dir, "ldb_main"), new File(backupDir, "ldb_main"))

    val uncommittedDigest = withProverAt(dir, 10) { (_, prover) =>
      insertRandom(prover, 1)
      prover.digest
    }
    // Simulate a stop after ldb_undo persisted uncommittedDigest and before ldb_main persisted its batch
    replaceSubDir(new File(backupDir, "ldb_main"), new File(dir, "ldb_main"))

    withProverAt(dir, 10) { (store, restored) =>
      restored.digest.toSeq shouldBe committedDigest.toSeq
      store.lastVersionID.map(_.toSeq) shouldBe Some(committedDigest.toSeq)
      store.versionIdExists(uncommittedDigest) shouldBe false
      store.versionIdExists(firstDigest) shouldBe true
      insertRandom(restored, 1)
      restored.rollback(firstDigest) shouldBe 'success
      restored.digest.toSeq shouldBe firstDigest.toSeq
    }
  }

  property("store recovers from a stop during the first update on an empty store") {
    val dir = createTempDir
    val backupDir = createTempDir
    val vFirst = written("first", 1).head
    val vNext = written("next", 1).head

    withVersionedStore(10, dir)(_ => ())
    replaceSubDir(new File(dir, "ldb_main"), new File(backupDir, "ldb_main"))

    withVersionedStore(10, dir)(store => store.update(vFirst.id, Seq.empty, Seq(keyA -> vFirst.value)).get)
    replaceSubDir(new File(backupDir, "ldb_main"), new File(dir, "ldb_main"))

    withVersionedStore(10, dir) { store =>
      store.lastVersionID shouldBe None
      store.get(keyA) shouldBe None
      store.versionIdExists(vFirst.id) shouldBe false
      store.update(vNext.id, Seq.empty, Seq(keyA -> vNext.value)).get
      store.lastVersionID.map(_.toSeq) shouldBe Some(vNext.id.toSeq)
      store.get(keyA).map(_.toSeq) shouldBe Some(vNext.value.toSeq)
    }
  }

  property("store recovers from a stop inside update after a keepVersions = 0 period") {
    val dir = createTempDir
    val backupDir = createTempDir
    val versions = written("kz", 3)

    withVersionedStore(0, dir)(writeVersions(_, versions.take(2)))
    // Initialize committed metadata on reopen with keepVersions > 0, then snapshot ldb_main at versions(1)
    withVersionedStore(10, dir)(_ => ())
    replaceSubDir(new File(dir, "ldb_main"), new File(backupDir, "ldb_main"))

    withVersionedStore(10, dir)(writeVersions(_, versions.drop(2)))
    replaceSubDir(new File(backupDir, "ldb_main"), new File(dir, "ldb_main"))

    withVersionedStore(10, dir) { store =>
      store.lastVersionID.map(_.toSeq) shouldBe Some(versions(1).id.toSeq)
      store.get(keyA).map(_.toSeq) shouldBe Some(versions(1).value.toSeq)
      store.versionIdExists(versions(2).id) shouldBe false
    }
  }

  property("persistent prover restarts after a stop between ldb_main and ldb_undo writes in rollbackTo") {
    val dir = createTempDir
    val backupDir = createTempDir

    val (targetDigest, _, latestDigest) = withProverAt(dir, 10) { (_, prover) =>
      insertRandom(prover, 1)
      val d1 = prover.digest
      insertRandom(prover, 1)
      val d2 = prover.digest
      insertRandom(prover, 1)
      (d1, d2, prover.digest)
    }
    replaceSubDir(new File(dir, "ldb_undo"), new File(backupDir, "ldb_undo"))

    withProverAt(dir, 10) { (_, prover) =>
      prover.rollback(targetDigest).get
    }
    // Simulate a stop inside rollbackTo after ldb_main rolled back to targetDigest and before ldb_undo deleted d2..latestDigest
    replaceSubDir(new File(backupDir, "ldb_undo"), new File(dir, "ldb_undo"))

    withProverAt(dir, 10) { (store, restored) =>
      restored.digest.toSeq shouldBe targetDigest.toSeq
      store.lastVersionID.map(_.toSeq) shouldBe Some(targetDigest.toSeq)
      store.versionIdExists(latestDigest) shouldBe false
      insertRandom(restored, 1)
      val newDigest = restored.digest
      store.versionIdExists(newDigest) shouldBe true
      restored.rollback(targetDigest) shouldBe 'success
    }
  }

  property("legacy store without committed_state_key preserves undo versions after a keepVersions = 0 period") {
    val dir = createTempDir
    val versions = written("legacy", 4)
    val committedStateKey = scorex.crypto.hash.Blake2b256("committed_state")

    withVersionedStore(0, dir)(writeVersions(_, versions.take(2)))
    withVersionedStore(10, dir) { store =>
      writeVersions(store, versions.drop(2))
      // Remove committed_state_key directly to emulate a pre-#2859 store directory on disk
      store.db.delete(committedStateKey)
    }

    withVersionedStore(10, dir) { store =>
      store.lastVersionID.map(_.toSeq) shouldBe Some(versions.last.id.toSeq)
      store.get(keyA).map(_.toSeq) shouldBe Some(versions.last.value.toSeq)
      store.rollbackTo(versions(2).id) shouldBe 'success
      store.get(keyA).map(_.toSeq) shouldBe Some(versions(2).value.toSeq)
    }
  }

}
