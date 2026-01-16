package org.ergoplatform.crypto

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.settings.ErgoAlgos
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.crypto.CryptoConstants
import sigma.data.{GroupElementSerializer, ProveDlog}
import sigma.serialization.GroupElementSerializer

import java.security.SecureRandom
import org.bouncycastle.util.BigIntegers

/**
 * Test to verify the Schnorr signature implementation according to the specification
 */
class SchnorrSignatureVerificationTest extends AnyFlatSpec with Matchers {

  "Schnorr signature" should "follow the specification correctly" in {
    
    // Generate a random private key
    val secureRandom = new SecureRandom()
    val privateKeyBytes = new Array[Byte](32)
    secureRandom.nextBytes(privateKeyBytes)
    val privateKeyBI = BigInt(BigIntegers.fromUnsignedByteArray(privateKeyBytes))
    
    // Calculate public key: P = s*G where s is the private key
    val publicKeyPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, privateKeyBI.bigInteger)
    val publicKeyBytes = GroupElementSerializer.toBytes(publicKeyPoint)
    
    // Create a sample message to sign
    val message = "02415748f8eef16c5ea6896cec3a8defccc8a0dace245248be66ffd6ff2159da32000000000003d09000000000694fa26d"
    val messageBytes = Base16.decode(message).get
    
    // Generate the Schnorr signature following the specification
    // Step 1: Generate a random nonce k
    val kBytes = new Array[Byte](32)
    secureRandom.nextBytes(kBytes)
    val kBI = BigInt(BigIntegers.fromUnsignedByteArray(kBytes))
    
    // Step 2: Calculate R = k*G (random point)
    val rPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, kBI.bigInteger)
    
    // Step 3: Calculate challenge e = H(R || message || public_key)
    val rBytes = GroupElementSerializer.toBytes(rPoint)
    val challengeInput = rBytes ++ messageBytes ++ publicKeyBytes
    val eFull = Blake2b256(challengeInput)
    val eBI = BigInt(BigIntegers.fromUnsignedByteArray(eFull)) % CryptoConstants.groupOrder
    
    // Step 4: Calculate response z = k + e * s (mod n) where s is the private key
    val zBI = (kBI.bigInteger.add(eBI.bigInteger.multiply(privateKeyBI))).remainder(CryptoConstants.groupOrder)
    val z = BigInt(zBI)
    
    // Step 5: Format the signature as [prefix][a_component][z_component]
    val rCompressed = GroupElementSerializer.toBytes(rPoint)
    val prefixByte = rCompressed.head
    val aComponent = rCompressed.tail // 32 bytes (compressed point without prefix)
    val zComponent = BigIntegers.asUnsignedByteArray(32, z.bigInteger)
    
    val formattedSignature = Array(prefixByte.toByte) ++ aComponent ++ zComponent
    
    // Verify the signature
    val signatureHex = Base16.encode(formattedSignature)
    val publicKeyHex = Base16.encode(publicKeyBytes)
    
    // Verification: Check that R = z*G - e*P
    // Calculate z*G
    val zgPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, z.bigInteger)
    // Calculate e*P (where P is the public key)
    val epPoint = CryptoConstants.dlogGroup.exponentiate(publicKeyPoint, eBI.bigInteger)
    // Calculate z*G - e*P
    val verificationPoint = CryptoConstants.dlogGroup.operate(zgPoint, CryptoConstants.dlogGroup.inverse(epPoint))
    
    // The verification point should equal R
    val verificationRBytes = GroupElementSerializer.toBytes(verificationPoint)
    val originalRBytes = rBytes
    
    assert(java.util.Arrays.equals(verificationRBytes, originalRBytes), 
      "Schnorr signature verification failed: R != z*G - e*P")
    
    // Also verify that the signature format matches the specification
    assert(formattedSignature.length == 65, s"Signature should be 65 bytes, got ${formattedSignature.length}")
    assert(signatureHex.length == 130, s"Signature hex should be 130 characters, got ${signatureHex.length}")
    
    // Check that the first byte is a valid prefix (0x02 or 0x03 for compressed points)
    val prefix = formattedSignature.head
    assert(prefix == 0x02.toByte || prefix == 0x03.toByte, 
      s"Prefix should be 0x02 or 0x03, got 0x${String.format("%02x", prefix & 0xff)}")
    
    println(s"Signature: $signatureHex")
    println(s"Public key: $publicKeyHex")
    println(s"Message: $message")
    println("Schnorr signature verification passed!")
  }

  "Schnorr signature" should "be verifiable with the public key" in {
    // Test with a known address and verify the signature can be verified
    val addressEncoder = new ErgoAddressEncoder(ErgoAlgos.TestnetNetworkPrefix)
    
    // Generate a private key and corresponding P2PK address
    val secureRandom = new SecureRandom()
    val privateKeyBytes = new Array[Byte](32)
    secureRandom.nextBytes(privateKeyBytes)
    val privateKeyBI = BigInt(BigIntegers.fromUnsignedByteArray(privateKeyBytes))
    
    // Calculate public key
    val publicKeyPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, privateKeyBI.bigInteger)
    val publicKeyBytes = GroupElementSerializer.toBytes(publicKeyPoint)
    
    // Create P2PK address
    val proveDlog = ProveDlog(publicKeyPoint)
    val p2pkAddress = org.ergoplatform.P2PKAddress(proveDlog)(addressEncoder)
    
    // Message to sign
    val message = "test message for schnorr signature"
    val messageBytes = message.getBytes("UTF-8")
    
    // Generate signature following the specification
    val kBytes = new Array[Byte](32)
    secureRandom.nextBytes(kBytes)
    val kBI = BigInt(BigIntegers.fromUnsignedByteArray(kBytes))
    
    val rPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, kBI.bigInteger)
    
    val rBytes = GroupElementSerializer.toBytes(rPoint)
    val challengeInput = rBytes ++ messageBytes ++ publicKeyBytes
    val eFull = Blake2b256(challengeInput)
    val eBI = BigInt(BigIntegers.fromUnsignedByteArray(eFull)) % CryptoConstants.groupOrder
    
    val zBI = (kBI.bigInteger.add(eBI.bigInteger.multiply(privateKeyBI))).remainder(CryptoConstants.groupOrder)
    val z = BigInt(zBI)
    
    val rCompressed = GroupElementSerializer.toBytes(rPoint)
    val prefixByte = rCompressed.head
    val aComponent = rCompressed.tail
    val zComponent = BigIntegers.asUnsignedByteArray(32, z.bigInteger)
    
    val formattedSignature = Array(prefixByte.toByte) ++ aComponent ++ zComponent
    
    // Verify the signature using the verification equation
    val zgPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, z.bigInteger)
    val epPoint = CryptoConstants.dlogGroup.exponentiate(publicKeyPoint, eBI.bigInteger)
    val verificationPoint = CryptoConstants.dlogGroup.operate(zgPoint, CryptoConstants.dlogGroup.inverse(epPoint))
    
    val verificationRBytes = GroupElementSerializer.toBytes(verificationPoint)
    val originalRBytes = rBytes
    
    assert(java.util.Arrays.equals(verificationRBytes, originalRBytes), 
      "Schnorr signature verification failed")
      
    println(s"Generated P2PK address: ${p2pkAddress}")
    println(s"Signature successfully generated and verified!")
  }

  "Schnorr signature format" should "match the specification" in {
    // Test that the signature format matches the specification exactly
    val secureRandom = new SecureRandom()
    val privateKeyBytes = new Array[Byte](32)
    secureRandom.nextBytes(privateKeyBytes)
    val privateKeyBI = BigInt(BigIntegers.fromUnsignedByteArray(privateKeyBytes))
    
    val publicKeyPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, privateKeyBI.bigInteger)
    val publicKeyBytes = GroupElementSerializer.toBytes(publicKeyPoint)
    
    val message = "sample message"
    val messageBytes = message.getBytes("UTF-8")
    
    // Generate signature
    val kBytes = new Array[Byte](32)
    secureRandom.nextBytes(kBytes)
    val kBI = BigInt(BigIntegers.fromUnsignedByteArray(kBytes))
    
    val rPoint = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, kBI.bigInteger)
    
    val rBytes = GroupElementSerializer.toBytes(rPoint)
    val challengeInput = rBytes ++ messageBytes ++ publicKeyBytes
    val eFull = Blake2b256(challengeInput)
    val eBI = BigInt(BigIntegers.fromUnsignedByteArray(eFull)) % CryptoConstants.groupOrder
    
    val zBI = (kBI.bigInteger.add(eBI.bigInteger.multiply(privateKeyBI))).remainder(CryptoConstants.groupOrder)
    val z = BigInt(zBI)
    
    val rCompressed = GroupElementSerializer.toBytes(rPoint)
    val prefixByte = rCompressed.head
    val aComponent = rCompressed.tail
    val zComponent = BigIntegers.asUnsignedByteArray(32, z.bigInteger)
    
    val formattedSignature = Array(prefixByte.toByte) ++ aComponent ++ zComponent
    
    // Verify format according to spec:
    // - Total length: 65 bytes
    // - Format: [prefix][a_component][z_component]
    // - prefix: 1 byte (0x02 or 0x03)
    // - a_component: 32 bytes
    // - z_component: 32 bytes
    
    assert(formattedSignature.length == 65, s"Expected 65 bytes, got ${formattedSignature.length}")
    assert(aComponent.length == 32, s"a_component should be 32 bytes, got ${aComponent.length}")
    assert(zComponent.length == 32, s"z_component should be 32 bytes, got ${zComponent.length}")
    assert(prefixByte == 0x02.toByte || prefixByte == 0x03.toByte, 
      s"Prefix should be 0x02 or 0x03, got 0x${String.format("%02x", prefixByte & 0xff)}")
    
    println("Schnorr signature format verification passed!")
  }
}