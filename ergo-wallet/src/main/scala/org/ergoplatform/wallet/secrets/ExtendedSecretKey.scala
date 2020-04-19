package org.ergoplatform.wallet.secrets

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants

/**
  * Secret, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedSecretKey(val keyBytes: Array[Byte],
                              val chainCode: Array[Byte],
                              val path: DerivationPath)
  extends ExtendedKey with SecretKey {

  override def key: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(keyBytes))

  def publicImage: DLogProtocol.ProveDlog = key.publicImage

  def child(idx: Int): ExtendedSecretKey = ExtendedSecretKey.deriveChildSecretKey(this, idx)

  def publicKey: ExtendedPublicKey =
    new ExtendedPublicKey(key.publicImage.value.getEncoded(true), chainCode, path.toPublic)

  def isErased: Boolean = keyBytes.forall(_ == 0x00)

  def zeroSecret(): Unit = util.Arrays.fill(keyBytes, 0: Byte)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: ExtendedSecretKey =>
      util.Arrays.equals(that.keyBytes, this.keyBytes) &&
        util.Arrays.equals(that.chainCode, this.chainCode) &&
        that.path == this.path
    case _ => false
  })

  override def hashCode(): Int = {
    var h = util.Arrays.hashCode(keyBytes)
    h = 31 * h + util.Arrays.hashCode(chainCode)
    h = 31 * h + path.hashCode()
    h
  }

}

object ExtendedSecretKey {

  @scala.annotation.tailrec
  def deriveChildSecretKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedSecretKey = {
    val keyCoded: Array[Byte] =
      if (Index.isHardened(idx)) (0x00: Byte) +: parentKey.keyBytes
      else parentKey.key.publicImage.value.getEncoded(true)
    val (childKeyProto, childChainCode) = HmacSHA512
      .hash(parentKey.chainCode, keyCoded ++ Index.serializeIndex(idx))
      .splitAt(Constants.KeyLen)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = childKeyProtoDecoded
      .add(BigIntegers.fromUnsignedByteArray(parentKey.keyBytes))
      .mod(CryptoConstants.groupOrder)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || childKey.equals(BigInteger.ZERO))
      deriveChildSecretKey(parentKey, idx + 1)
    else
      new ExtendedSecretKey(BigIntegers.asUnsignedByteArray(childKey), childChainCode, parentKey.path.extended(idx))
  }

  def deriveChildPublicKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedPublicKey = {
    val derivedSecret = deriveChildSecretKey(parentKey, idx)
    val derivedPk = derivedSecret.key.publicImage.value.getEncoded(true)
    val derivedPath = derivedSecret.path.copy(publicBranch = true)
    new ExtendedPublicKey(derivedPk, derivedSecret.chainCode, derivedPath)
  }

  def deriveMasterKey(seed: Array[Byte]): ExtendedSecretKey = {
    val (masterKey, chainCode) = HmacSHA512.hash(Constants.BitcoinSeed, seed).splitAt(Constants.KeyLen)
    new ExtendedSecretKey(masterKey, chainCode, DerivationPath.MasterPath)
  }

}

object ExtendedSecretKeySerializer extends ErgoWalletSerializer[ExtendedSecretKey] {

  import scorex.util.Extensions._

  override def serialize(obj: ExtendedSecretKey, w: Writer): Unit = {
    w.putBytes(obj.keyBytes)
    w.putBytes(obj.chainCode)
    val pathBytes = DerivationPathSerializer.toBytes(obj.path)
    w.putUInt(pathBytes.length)
    w.putBytes(pathBytes)
  }

  override def parse(r: Reader): ExtendedSecretKey = {
    val keyBytes = r.getBytes(Constants.KeyLen)
    val chainCode = r.getBytes(Constants.KeyLen)
    val pathLen = r.getUInt().toIntExact
    val path = DerivationPathSerializer.parseBytes(r.getBytes(pathLen))
    new ExtendedSecretKey(keyBytes, chainCode, path)
  }

}
