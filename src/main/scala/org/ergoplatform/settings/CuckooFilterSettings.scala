package org.ergoplatform.settings

/**
  * Cuckoo filter settings
  * @param entriesPerBucket - entries per bucket
  * @param bucketsQty - number of buckets in the filter
  */
case class CuckooFilterSettings(entriesPerBucket: Int, bucketsQty: Int)