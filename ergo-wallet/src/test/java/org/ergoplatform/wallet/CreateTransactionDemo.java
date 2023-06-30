package org.ergoplatform.wallet;

import io.circe.Json;
import org.ergoplatform.*;
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey;
import org.ergoplatform.wallet.interface4j.crypto.ErgoUnsafeProver;
import org.ergoplatform.wallet.serialization.JsonCodecsWrapper;
import org.ergoplatform.wallet.transactions.TransactionBuilder;
import org.ergoplatform.wallet.transactions.TransactionBuilder.Payment;
import scorex.util.Random;
import sigmastate.basics.DLogProtocol;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CreateTransactionDemo {

    public static void main(String[] args) {
        createMultiPaymentTransaction();
    }

    /**
     * A demo describing the process of creating simple payment transaction with one key.
     * Note, more complex transaction would require more steps which are not described in this demo.
     */
    public static void createTransaction() throws Exception {
        ErgoAddressEncoder encoder = new ErgoAddressEncoder((byte) 0x00);

        String receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq";
        ErgoAddress receiverAddress = encoder.fromString(receiverAddressStr).get();

        // Create an address
        byte[] entropy = Random.randomBytes(32);
        ExtendedSecretKey extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy, false);
        ErgoAddress myAddress = P2PKAddress.apply(extendedSecretKey.privateInput().publicImage(), encoder);

        int transferAmt = 25000000; // amount to transfer
        int feeAmt = 1000000; // minimal fee amount
        int changeAmt = 20000; // amount to return back

        int currentNetworkHeight = 32987; // from explorer `https://api.ergoplatform.com/blocks`

        // from explorer `https://api.ergoplatform.com/transactions/boxes/byAddress/unspent/{myAddress}`
        String[] myInputs = new String[] {
                "430e80ca31a25400e77dac0ad14c1cd39cb09dc3f7c1c384dce9aef19b604e27",
                "d9ba3ed2f55bc61ec90b7ee3949362a9a20fbf8514d2306eb97f14e07d234797" };

        UnsignedErgoLikeTransaction unsignedTx =
                TransactionBuilder.paymentTransaction(
                        receiverAddress, myAddress, transferAmt, feeAmt, changeAmt, myInputs, currentNetworkHeight
                );

        ErgoLikeTransaction tx = new ErgoUnsafeProver().prove(unsignedTx, extendedSecretKey.privateInput());

        // print transaction JSON
        // then the transaction can be broadcasted by sending the json to
        // https://api.ergoplatform.com/api/v0/transactions/send (POST request)
        Json json = JsonCodecsWrapper.ergoLikeTransactionEncoder().apply(tx);
        System.out.println(json.toString());
    }

    /**
     * A demo describing the process of creating simple payment transaction with one key.
     * Note, more complex transaction would require more steps which are not described in this demo.
     */
    public static void createMultiPaymentTransaction() {
        ErgoAddressEncoder encoder = new ErgoAddressEncoder((byte) 0x00);

        String receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq";
        ErgoAddress receiverAddress = encoder.fromString(receiverAddressStr).get();

        // Create an address
        byte[] entropy = Random.randomBytes(32);
        ExtendedSecretKey extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy, false);
        ErgoAddress myAddress = P2PKAddress.apply(extendedSecretKey.privateInput().publicImage(), encoder);

        int feeAmt = 1000000; // minimal fee amount
        int changeAmt = 20000; // amount to return back

        int currentNetworkHeight = 32987; // from explorer `https://api.ergoplatform.com/blocks`

        // from explorer `https://api.ergoplatform.com/transactions/boxes/byAddress/unspent/{myAddress}`
        String[] myInputs = new String[] {
                "430e80ca31a25400e77dac0ad14c1cd39cb09dc3f7c1c384dce9aef19b604e27",
                "d9ba3ed2f55bc61ec90b7ee3949362a9a20fbf8514d2306eb97f14e07d234797"
        };

        Payment payment1 = new Payment(receiverAddress, 30000000);
        Payment payment2 = new Payment(receiverAddress, 15000000);

        List<Payment> payments = new ArrayList<>();
        payments.add(payment1);
        payments.add(payment2);
        UnsignedErgoLikeTransaction unsignedTx =
                TransactionBuilder.multiPaymentTransaction(
                        myInputs, feeAmt, payments, myAddress, changeAmt, currentNetworkHeight
                );

        ErgoLikeTransaction tx = new ErgoUnsafeProver().prove(unsignedTx, extendedSecretKey.privateInput());
        // print transaction JSON
        // then the transaction can be broadcasted by sending the json to
        // https://api.ergoplatform.com/api/v0/transactions/send (POST request)
        Json json = JsonCodecsWrapper.ergoLikeTransactionEncoder().apply(tx);
        System.out.println(json.toString());
    }

    /**
     * A demo describing the process of creating simple payment transaction with multiple keys.
     * Note, more complex transaction would require more steps which are not described in this demo.
     */
    public static void createTransactionMultipleKeys() throws Exception {
        ErgoAddressEncoder encoder = new ErgoAddressEncoder((byte) 0x00);

        String receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq";
        ErgoAddress receiverAddress = encoder.fromString(receiverAddressStr).get();

        // Create second address
        byte[] entropy1 = Random.randomBytes(32);
        ExtendedSecretKey extendedSecretKey1 = ExtendedSecretKey.deriveMasterKey(entropy1, false);
        ErgoAddress changeAddress = P2PKAddress.apply(extendedSecretKey1.privateInput().publicImage(), encoder);

        byte[] entropy2 = Random.randomBytes(32);
        ExtendedSecretKey extendedSecretKey2 = ExtendedSecretKey.deriveMasterKey(entropy2, false);


        int transferAmt = 25000000; // amount to transfer
        int feeAmt = 1000000; // minimal fee amount
        int changeAmt = 20000; // amount to return back

        int currentNetworkHeight = 32987; // from explorer `https://api.ergoplatform.com/blocks`

        // from explorer `https://api.ergoplatform.com/transactions/boxes/byAddress/unspent/{myAddress}`
        Map<String, DLogProtocol.DLogProverInput> myInputs = new HashMap<String, DLogProtocol.DLogProverInput>();
        myInputs.put("430e80ca31a25400e77dac0ad14c1cd39cb09dc3f7c1c384dce9aef19b604e27", extendedSecretKey1.privateInput());
        myInputs.put("d9ba3ed2f55bc61ec90b7ee3949362a9a20fbf8514d2306eb97f14e07d234797", extendedSecretKey2.privateInput());

        UnsignedErgoLikeTransaction unsignedTx =
                TransactionBuilder.paymentTransaction(
                        receiverAddress, changeAddress, transferAmt, feeAmt, changeAmt, myInputs.keySet().toArray(new String[0]), currentNetworkHeight
                );

        ErgoLikeTransaction tx = new ErgoUnsafeProver().prove(unsignedTx, myInputs);

        // print transaction JSON
        // then the transaction can be broadcasted by sending the json to
        // https://api.ergoplatform.com/api/v0/transactions/send (POST request)
        Json json = JsonCodecsWrapper.ergoLikeTransactionEncoder().apply(tx);
        System.out.println(json.toString());
    }
}
