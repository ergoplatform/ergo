package scorex.crypto.authds.avltree.batch

import Constants.HashLength

/**
  * Parameters of AVL+ tree nodes (internal and leaves)
  * @param keySize - size of a key (fixed)
  * @param valueSize - size of a value in a leaf (fixed, if defined, arbitrary, if None)
  * @param labelSize - size of a label (node hash), fixed
  */
case class AvlTreeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int) {
  /**
    * @return whether value is fixed-size
    */
  def fixedSizeValue: Boolean = valueSize.isDefined
}


/**
  * AVL+ tree node parameters. The tree is used to authenticate UTXO set.
  * Keys and hashes are 256-bits long, values are boxes, so value size is dynamic.
  */
object StateTreeParameters extends AvlTreeParameters(keySize = HashLength, valueSize = None, labelSize = HashLength)
