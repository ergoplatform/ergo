package org.ergoplatform.consensus

import org.ergoplatform.core.BytesSerializable

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo extends BytesSerializable

