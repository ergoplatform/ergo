package scorex.crypto.authds.avltree.batch.helpers

import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.util.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.db.LDBVersionedStore

trait TestHelper extends FileHelper {

  type HF = Blake2b256.type
  type D = Digest32
  type AD = ADDigest
  type P = SerializedAdProof
  type PROVER = BatchAVLProver[D, HF]
  type VERIFIER = BatchAVLVerifier[D, HF]
  type PERSISTENT_PROVER = PersistentBatchAVLProver[D, HF]
  type STORAGE = VersionedLDBAVLStorage

  protected val KL: Int
  protected val VL: Int

  implicit val hf: HF = Blake2b256

  def createVersionedStore(initialKeepVersions: Int = 10): LDBVersionedStore = {
    val dir = getRandomTempDir
    new LDBVersionedStore(dir, initialKeepVersions = initialKeepVersions)
  }

  def createVersionedStorage(store: LDBVersionedStore): STORAGE =
    new VersionedLDBAVLStorage(store)

  def createPersistentProver(storage: STORAGE): PERSISTENT_PROVER = {
    val prover = new BatchAVLProver[D, HF](KL, Some(VL))
    createPersistentProver(storage, prover)
  }

  def createPersistentProver(storage: STORAGE, prover: PROVER): PERSISTENT_PROVER =
    PersistentBatchAVLProver.create[D, HF](prover, storage, paranoidChecks = true).get

  def createPersistentProver(keepVersions: Int = 10): PERSISTENT_PROVER = {
    val store = createVersionedStore(keepVersions)
    val storage = createVersionedStorage(store)
    createPersistentProver(storage)
  }

  def createVerifier(digest: AD, proof: P): VERIFIER = new BatchAVLVerifier[D, HF](digest, proof, KL, Some(VL))

  implicit class DigestToBase58String(d: ADDigest) {

    def toBase58: String = Base58.encode(d)
  }

}
