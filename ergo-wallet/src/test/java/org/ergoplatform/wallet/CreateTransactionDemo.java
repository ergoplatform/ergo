package org.ergoplatform.wallet;

import io.circe.Json;
import org.ergoplatform.*;
import org.ergoplatform.wallet.interface4j.crypto.ErgoUnsafeProver;
import org.ergoplatform.wallet.secrets.ExtendedSecretKey;
import org.ergoplatform.wallet.serialization.JsonCodecsWrapper;
import scorex.util.Random;

public class CreateTransactionDemo {

    /**
     * A demo describing the process of creating simple payment transaction.
     * Note, more complex transaction would require more steps which are not described in this demo.
     */
    public void createTransaction() throws Exception {
        ErgoAddressEncoder encoder = new ErgoAddressEncoder((byte) 0x00);

        String receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq";
        ErgoAddress receiverAddress = encoder.fromString(receiverAddressStr).get();

        // Create an address
        byte[] entropy = Random.randomBytes(32);
        ExtendedSecretKey extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy);
        ErgoAddress myAddress = P2PKAddress.apply(extendedSecretKey.privateInput().publicImage(), encoder);

        int transferAmt = 25000000; // amount to transfer
        int feeAmt = 1000000; // minimal fee amount
        int changeAmt = 20000; // amount to return back

        int currentNetworkHeight = 32987; // from explorer `https://api.ergoplatform.com/blocks`

        // from explorer `https://api.ergoplatform.com/transactions/boxes/byAddress/unspent/{myAddress}`
        String[] myInputs = new String[] {
                "430e80ca31a25400e77dac0ad14c1cd39cb09dc3f7c1c384dce9aef19b604e27",
                "d9ba3ed2f55bc61ec90b7ee3949362a9a20fbf8514d2306eb97f14e07d234797" };

        UnsignedErgoLikeTransaction unsignedTx = Utils.paymentTransaction(
                receiverAddress,
                myAddress,
                transferAmt,
                feeAmt,
                changeAmt,
                myInputs,
                currentNetworkHeight
        );

        ErgoLikeTransaction tx = new ErgoUnsafeProver().prove(unsignedTx, extendedSecretKey.privateInput());

        // print transaction JSON
        // then the transaction can be broadcasted by sending the json to
        // https://api.ergoplatform.com/api/v0/transactions/send (POST request)
        Json json = JsonCodecsWrapper.ergoLikeTransactionEncoder().apply(tx);
        System.out.println(json.toString());
    }

}
