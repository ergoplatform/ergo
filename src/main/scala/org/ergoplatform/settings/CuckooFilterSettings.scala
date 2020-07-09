package org.ergoplatform.settings

/**
  * Cuckoo filter settings.
  *
  * Cuckoo filter is used by the wallet in order to recognize boxes which belong to wallet's addresses.
  *
  * Please note that maximum number of keys the filter can accept is entriesPerBucket * bucketsQty
  *
  * @param entriesPerBucket - entries per bucket
  * @param bucketsQty - number of buckets in the filter
  */
case class CuckooFilterSettings(entriesPerBucket: Int, bucketsQty: Int)
