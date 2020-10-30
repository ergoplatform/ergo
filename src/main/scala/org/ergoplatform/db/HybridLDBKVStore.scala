package org.ergoplatform.db


import java.io.File

import scorex.db.{LDBKVStore, LDBVersionedStore}

/**
  * Storage which is for both versioned and non-versioned data.
  *
  * Please not that non-versioned data have priority over version. For example, if at some moment of time
  * a record got removed, after rollback it is still removed too.
  *
  * Currently the only use for non-versioned data is box <-> application correspondence in EIP-1 implementation.
  */
//todo: remove
class HybridLDBKVStore(protected override val dir: File, keepVersions: Int)
  extends LDBVersionedStore(dir, keepVersions) {

}
