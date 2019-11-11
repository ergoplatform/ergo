package org.ergoplatform.wallet.interface4j.crypto;

import org.bouncycastle.math.ec.custom.sec.SecP256K1Point;
import scala.math.BigInt;

import java.math.BigInteger;

/**
 * A wrapper over Schnorr signature implementation in Scala.
 */
public class ErgoSignature {

    /**
     * Signs given `msg` using given `sk`.
     *
     * @return signature bytes
     */
    public byte[] sign(byte[] msg, BigInteger sk) {
        return org.ergoplatform.wallet.crypto.ErgoSignature.sign(msg, BigInt.apply(sk));
    }

    /**
     * Checks whether a given `signature` corresponds to a given `msg` and `pk`.
     *
     * @return `true` is the signature is valid, `false` otherwise
     */
    public boolean verify(byte[] msg, byte[] signature, SecP256K1Point pk) {
        return org.ergoplatform.wallet.crypto.ErgoSignature.verify(msg, signature, pk);
    }

}
