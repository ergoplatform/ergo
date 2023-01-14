package scorex.crypto.authds.avltree.batch

/**
  * Parameters of AVL+ tree nodes (internal and leaves)
  * @param keySize - size of a key (fixed)
  * @param valueSize - size of a value in a leaf (fixed, if defined, arbitrary, if None)
  * @param labelSize - size of a label (node hash), fixed
  */
case class NodeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int) {
  /**
    * @return whether value is fixed-size
    */
  def fixedSizeValue: Boolean = valueSize.isDefined

  // todo: move out? not looking so good here
  private[batch] val TopNodeKey: Array[Byte] = Array.fill(labelSize)(123: Byte)
  private[batch] val TopNodeHeight: Array[Byte] = Array.fill(labelSize)(124: Byte)
}
