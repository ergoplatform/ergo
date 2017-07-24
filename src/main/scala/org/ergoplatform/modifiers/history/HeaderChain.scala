package org.ergoplatform.modifiers.history

case class HeaderChain(headers: Seq[Header]) {
  headers.indices.foreach { i =>
    if (i > 0) assert(headers(i).parentId sameElements headers(i - 1).id)
  }

  def exists(f: Header => Boolean): Boolean = headers.exists(f)

  def head: Header = headers.head

  def last: Header = headers.last

  def tail: HeaderChain = HeaderChain(headers.tail)

  def take(i: Int) = HeaderChain(headers.take(i))

  def takeAfter(h: Header) = {
    val commonIndex = headers.indexWhere(_.id sameElements h.id)
    val commonBlockThenSuffixes = headers.takeRight(headers.length - commonIndex)
    HeaderChain(commonBlockThenSuffixes)
  }

  def ++(c: HeaderChain) = HeaderChain(headers ++ c.headers)
}

