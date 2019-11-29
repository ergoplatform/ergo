package org.ergoplatform.wallet.interface4j.crypto;

import org.ergoplatform.ErgoLikeTransaction;
import org.ergoplatform.UnsignedErgoLikeTransaction;
import sigmastate.basics.DLogProtocol;

/**
 * A wrapper over naive Ergo prover implementation.
 */
public class ErgoUnsafeProver {

    /**
     * Signs all inputs of a given `unsignedTx`.
     *
     * @return signed transaction
     */
    public ErgoLikeTransaction prove(UnsignedErgoLikeTransaction unsignedTx, DLogProtocol.DLogProverInput sk) {
        return org.ergoplatform.wallet.interpreter.ErgoUnsafeProver.prove(unsignedTx, sk);
    }

}
