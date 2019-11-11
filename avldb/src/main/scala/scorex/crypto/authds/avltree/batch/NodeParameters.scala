package scorex.crypto.authds.avltree.batch

//todo: support arbitrary-size values
case class NodeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int)
