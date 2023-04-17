package org.ergoplatform.wallet.secrets

import org.bouncycastle.math.ec.ECPoint

import java.math.BigInteger
import java.util
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.crypto.CryptoFacade
import sigmastate.interpreter.CryptoConstants

/**
  * Secret, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedSecretKey(private[secrets] val keyBytes: Array[Byte],
                              private[secrets] val chainCode: Array[Byte],
                              private[secrets] val usePre1627KeyDerivation: Boolean,
                              val path: DerivationPath)
  extends ExtendedKey[ExtendedSecretKey] with SecretKey {

  def selfReflection: ExtendedSecretKey = this

  override def privateInput: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(keyBytes))

  def publicImage: DLogProtocol.ProveDlog = privateInput.publicImage

  def child(idx: Int): ExtendedSecretKey = ExtendedSecretKey.deriveChildSecretKey(this, idx)

  /** Returns extended public key corresponding to this secret key. */
  def publicKey: ExtendedPublicKey =
    new ExtendedPublicKey(
      CryptoFacade.encodePoint(privateInput.publicImage.value, compressed = true),
      chainCode, path.toPublicBranch)

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
      else CryptoFacade.encodePoint(parentKey.privateInput.publicImage.value, compressed = true)
    val (childKeyProto, childChainCode) = HmacSHA512
      .hash(parentKey.chainCode, keyCoded ++ Index.serializeIndex(idx))
      .splitAt(Constants.SecretKeyLength)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = childKeyProtoDecoded
      .add(BigIntegers.fromUnsignedByteArray(parentKey.keyBytes))
      .mod(CryptoConstants.groupOrder)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || childKey.equals(BigInteger.ZERO))
      deriveChildSecretKey(parentKey, idx + 1)
    else {
      val keyBytes = if (parentKey.usePre1627KeyDerivation) {
        // maybe less than 32 bytes if childKey is small enough while BIP32 requires 32 bytes.
        // see https://github.com/ergoplatform/ergo/issues/1627 for details
        BigIntegers.asUnsignedByteArray(childKey)
      } else {
        // padded with leading zeroes to 32 bytes
        BigIntegers.asUnsignedByteArray(Constants.SecretKeyLength, childKey)
      }
      new ExtendedSecretKey(keyBytes, childChainCode, parentKey.usePre1627KeyDerivation, parentKey.path.extended(idx))
      }
  }

  def deriveChildPublicKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedPublicKey = {
    val derivedSecret = deriveChildSecretKey(parentKey, idx)
    val derivedPk = CryptoFacade.encodePoint(
      derivedSecret.privateInput.publicImage.value, compressed = true)
    val derivedPath = derivedSecret.path.copy(publicBranch = true)
    new ExtendedPublicKey(derivedPk, derivedSecret.chainCode, derivedPath)
  }


  /**
   * Derives master secret key from the seed 
   * @param seed - seed bytes
   * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
   */
  def deriveMasterKey(seed: Array[Byte], usePre1627KeyDerivation: Boolean): ExtendedSecretKey = {
    val (masterKey, chainCode) = HmacSHA512.hash(Constants.BitcoinSeed, seed).splitAt(Constants.SecretKeyLength)
    new ExtendedSecretKey(masterKey, chainCode, usePre1627KeyDerivation, DerivationPath.MasterPath)
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
    val keyBytes = r.getBytes(Constants.SecretKeyLength)
    val chainCode = r.getBytes(Constants.SecretKeyLength)
    val pathLen = r.getUInt().toIntExact
    val path = DerivationPathSerializer.parseBytes(r.getBytes(pathLen))
    new ExtendedSecretKey(keyBytes, chainCode, false, path)
  }

}
