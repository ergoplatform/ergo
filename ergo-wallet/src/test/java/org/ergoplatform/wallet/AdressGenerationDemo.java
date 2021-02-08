package org.ergoplatform.wallet;

import org.ergoplatform.ErgoAddressEncoder;
import org.ergoplatform.P2PKAddress;
import org.ergoplatform.wallet.mnemonic.Mnemonic;
import org.ergoplatform.wallet.secrets.DerivationPath;
import org.ergoplatform.wallet.secrets.ExtendedPublicKey;
import org.ergoplatform.wallet.secrets.ExtendedSecretKey;
import scala.Option;


public class AdressGenerationDemo {
    public static byte[] secretSeedFromMnemonic(String mnemonic) {
        byte[] secret =  Mnemonic.toSeed(mnemonic, Option.empty());
        return secret;
    }

    public static ExtendedSecretKey masterSecretFromSeed(byte[] seed) {
        ExtendedSecretKey rootSk = ExtendedSecretKey.deriveMasterKey(seed);
        return rootSk;
    }

    public static ExtendedSecretKey deriveSecretKey(ExtendedSecretKey rootSecret, DerivationPath path) {
        return (ExtendedSecretKey)rootSecret.derive(path);
    }

    public static DerivationPath nextPath(ExtendedSecretKey rootSecret, DerivationPath lastPath) {
        return rootSecret.derive(lastPath).path().increased();
    }

    public static void main(String[] args) {
        String mnemonic = "change me do not use me change me do not use me";

        ErgoAddressEncoder addressEncoder = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix());

        byte[] seed = secretSeedFromMnemonic(mnemonic);
        ExtendedSecretKey rootSecret = masterSecretFromSeed(seed);

        // Let's use "m/44'/429'/0'/0/0" path for change (this path is from EIP-3 which is BIP-44 for Ergo)
        DerivationPath changePath = Constants.eip3DerivationPath();
        ExtendedSecretKey changeSecretKey = deriveSecretKey(rootSecret, changePath);
        ExtendedPublicKey changePubkey = changeSecretKey.publicKey();
        P2PKAddress address = P2PKAddress.apply(changePubkey.key(), addressEncoder);

        System.out.println("Change address: " + address);

        DerivationPath firstUserPath = nextPath(rootSecret, changePath);
    }

}