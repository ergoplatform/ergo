package org.ergoplatform.nodeView.wallet.scanning

/**
  * Values for a scan-related flag which shows how scan is interacting with the in-built p2pk wallet.
  * There are three options:
  *  * off - if a box associated with a scan with interaction flag == off, the box will be added to the scan only
  *  * shared - box can be in both the wallet and the scan if wallet finds it (box is protected by wallet-related P2PK)
  *  * forced - box is always added to the wallet if it is added to the scan
  */
object ScanWalletInteraction extends Enumeration {
  type ScanWalletInteraction = Value

  val Off = Value("off")
  val Shared = Value("shared")
  val Forced = Value("forced")

  def toByte(v: Value): Byte = v match {
    case Off => -1 : Byte
    case Shared => -2 : Byte
    case Forced => -3 : Byte
  }

  def fromByte(b: Byte): ScanWalletInteraction = b match {
    case x: Byte if x == -1 => Off
    case x: Byte if x == -2 => Shared
    case x: Byte if x == -3 => Forced
  }

  def interactingWithWallet(v: Value): Boolean = v == Shared || v == Forced
}
