package scorex.crypto.authds.avltree.batch

//todo: support arbitrary-size values
@specialized
case class NodeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int)
