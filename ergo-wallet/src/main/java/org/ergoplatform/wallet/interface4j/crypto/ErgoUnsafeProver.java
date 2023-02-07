package org.ergoplatform.wallet.interface4j.crypto;

import org.ergoplatform.ErgoLikeTransaction;
import org.ergoplatform.UnsignedErgoLikeTransaction;
import scala.collection.JavaConverters;
import sigmastate.basics.DLogProtocol;
import java.util.Map;

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

    /**
     * Signs all inputs of a given `unsignedTx`.
     *
     * @return signed transaction
     */
    public ErgoLikeTransaction prove(UnsignedErgoLikeTransaction unsignedTx, Map<String, DLogProtocol.DLogProverInput> sks) {
        // JavaConversions used to support Scala 2.11
        return org.ergoplatform.wallet.interpreter.ErgoUnsafeProver.prove(unsignedTx, JavaConverters.mapAsScalaMap(sks));
    }

}
