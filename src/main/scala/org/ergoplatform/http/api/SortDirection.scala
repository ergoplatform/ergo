package org.ergoplatform.http.api

/**
  * Encoded results sorting direction (in ascending or descending order)
  */
object SortDirection {

  type Direction = Byte

  val ASC: Direction = 1.asInstanceOf[Direction]
  val DESC: Direction = 0.asInstanceOf[Direction]
  val INVALID: Direction = (-1).asInstanceOf[Direction]

}
