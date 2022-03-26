package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.utils.Generators
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ExtendedPublicKeySpec
  extends ExtendedSecretKeySpec
    with ScalaCheckPropertyChecks
    with Generators {

  val rootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(seed)

  property("public key tree derivation from seed (test vectors from BIP32 check)") {
    val expectedRoot = "kTV6HY41wXZVSqdpoe1heA8pBZFEN2oq5T59ZCMpqKKJ"
    val cases = Seq(
      ("uRg1eWWRkhghMxhcZEy2rRjfbc3MqWCJ1oVSP4dNmBAW", 1),
      ("xfhJ6aCQUodzhw1J4NcD7iJFvGVc3iPk3pBARCTncYcE", 1),
      ("2282dj5QqC7SM7G2ndp4pzaMZwT7vGgUAUZLCKhmXQFxG", 1)
    )

    val rootPk = rootSecret.publicKey

    equalBase58(rootPk.keyBytes, expectedRoot)

    cases.foldLeft(rootPk) { case (parent, (expectedKey, idx)) =>
      val child = parent.child(idx)
      equalBase58(child.keyBytes, expectedKey)
      child
    }
  }

  property("derived public tree corresponds to private tree") {
    forAll(derivationPathGen(isPublic = false, allowHardened = false)) { path =>
      val sk: ExtendedSecretKey = rootSecret.derive(path)
      val pk = rootSecret.publicKey.derive(path.copy(publicBranch = true))

      java.util.Arrays.equals(sk.publicKey.keyBytes, pk.keyBytes) shouldBe true
    }
  }

}
