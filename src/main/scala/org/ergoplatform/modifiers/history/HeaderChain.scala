package org.ergoplatform.modifiers.history

case class HeaderChain(chain: Seq[Header]) {
  chain.indices.foreach { i =>
    if (i > 0) assert(chain(i).parentId sameElements chain(i - 1).id)
  }

  def exists(f: Header => Boolean): Boolean = chain.exists(f)

  def head: Header = chain.head

  def takeAfter(h: Header) = {
    val commonIndex = chain.indexWhere(_.id sameElements h.id)
    val commonBlockThenSuffixes = chain.takeRight(chain.length - commonIndex)
    HeaderChain(commonBlockThenSuffixes)
  }
}

