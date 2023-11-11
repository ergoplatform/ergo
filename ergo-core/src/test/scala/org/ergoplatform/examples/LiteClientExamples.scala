package org.ergoplatform.examples

import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base16

/**
  * This class contains how-to examples for building clients
  */
class LiteClientExamples extends ErgoPropertyTest {

  /**
    * This code shows how to do a client checking a Merkle-tree based membership proof against an
    * incomplete header (so header without a proof-of-work solution). This could be useful for a
    * decentralized pool with collaterals, where the pool checks shares from miners, and a share
    * includes proof that a corresponding block contains a certain transaction (paying to the pool).
    *
    * For example, a working scheme for a decentralized pool could be as follows:
    *   1. A miner is creating a transaction which is paying to the pool
    *     (posting of the tx by the pool outside the block should be impossible)
    *   2. The miner is creating block candidate with the tx included (via calling "mining/candidateWithTxs").
    *     The result would be like the following:
    *       {
    *         "msg" : "6cb37d0a202bc2984f43de003cbc5558804db45798d0fc8faae7390b96d42d15",
    *         "b" : 748014723576678314041035877227113663879264849498014394977645987,
    *         "pk" : "0278011ec0cf5feb92d61adb51dcb75876627ace6fd9446ab4cabc5313ab7b39a7",
    *         "proof" : {
    *           "msgPreimage" : "01fb9e35f8a73c128b73e8fde5c108228060d68f11a69359ee0fb9bfd84e7ecde6d19957ccbbe75b075b3baf1cac6126b6e80b5770258f4cec29fbde92337faeec74c851610658a40f5ae74aa3a4babd5751bd827a6ccc1fe069468ef487cb90a8c452f6f90ab0b6c818f19b5d17befd85de199d533893a359eb25e7804c8b5d7514d784c8e0e52dabae6e89a9d6ed9c84388b228e7cdee09462488c636a87931d656eb8b40f82a507008ccacbee05000000",
    *           "txProofs" : [{
    *             "leaf" : "642c15c62553edd8fd9af9a6f754f3c7a6c03faacd0c9b9d5b7d11052c6c6fe8",
    *             "levels" : [
    *               "0139b79af823a92aa72ced2c6d9e7f7f4687de5b5af7fab0ad205d3e54bda3f3ae"
    *             ]
    *           }]
    *         }
    *       }
    *   3. The miner is mining the block using "msg", "b", "pk" as it happens now. If block is found, the miner is
    *   posting the transaction to the network. However, if real difficulty of the solution is not enough to form a
    *   block but enough for a share, then the miner can post the share to the pool. The share consists of "msg", "b",
    *   "pk", solution and the "proof".
    *
    *   4. The pool checks that the share is valid. In particular, the pool checks that:
    *     a) the "msgPreimage" (which is a header without a PoW solution) along with the PoW form a valid header
    *     with enough difficulty
    *     b) the header contains transaction by using "proof"
    *
    *   The code below is about step 4b only.
    */
  property("Example client code for tx proof") {
    implicit val hashFn: Blake2b256.type = Blake2b256

    val msgPreimageBase16 = "01fb9e35f8a73c128b73e8fde5c108228060d68f11a69359ee0fb9bfd84e7ecde6d19957ccbbe75b075b3baf1cac6126b6e80b5770258f4cec29fbde92337faeec74c851610658a40f5ae74aa3a4babd5751bd827a6ccc1fe069468ef487cb90a8c452f6f90ab0b6c818f19b5d17befd85de199d533893a359eb25e7804c8b5d7514d784c8e0e52dabae6e89a9d6ed9c84388b228e7cdee09462488c636a87931d656eb8b40f82a507008ccacbee05000000"
    val msgPreimage = Base16.decode(msgPreimageBase16).get

    val msg = "6cb37d0a202bc2984f43de003cbc5558804db45798d0fc8faae7390b96d42d15"

    //hash of "msgPreimage" (which is a header without PoW) should be equal to "msg"
    Base16.encode(hashFn(msgPreimage)) == msg

    //Transactions Merkle tree digest is in bytes 65-96 (inclusive) of the unproven header
    val txsRoot = msgPreimage.slice(65, 97)

    //txId is a "leaf" in a Merkle proof
    val txId = "642c15c62553edd8fd9af9a6f754f3c7a6c03faacd0c9b9d5b7d11052c6c6fe8"

    // Merkle roof is constructed by given leaf data, leaf hash sibling and also siblings for parent nodes. Using this
    // data, it is possible to compute nodes on the path to root hash, and the hash itself. The picture of a proof
    // given below. In the picture, "^^" is leaf data(to compute leaf hash from), "=" values are to be computed,
    // "*" values are to be stored.
    //
    // ........= Root
    // ..... /  \
    // .... *   =
    // ....... / \
    // ...... *   =
    // ......... /.\
    // .........*   =
    // ............ ^^
    //
    // Merkle proof element is encoded in the following way:
    //  - first, 1 byte showing whether COMPUTED value is on the right (1) or on the left (0)
    //  - second, 32 bytes of stored value

    // To check the proof, first, hash of the leaf should be computed with the 1-byte zero prefix
    // (i.e. blake2b256(0 ++ leaf). Then, for each level, if the first byte is "right" (1) then
    // computed hash for the next level is blake2b256(1 ++ COMPUTED ++ stored hash (last 32 bytes of the level),
    // where COMPUTED is the value computed on the previous step (and "1" is internal node prefix).
    // A value computed on the last step is to be compared with expected root hash value.

    val levelsEncoded = Seq("0139b79af823a92aa72ced2c6d9e7f7f4687de5b5af7fab0ad205d3e54bda3f3ae")

    val levels = levelsEncoded.map { le =>
      val leBytes = Base16.decode(le).get
      val side: Byte = leBytes.head
      val digest = leBytes.tail
      (Digest32 @@ digest, Side @@ side)
    }

    val merkleProof = MerkleProof[Digest32](LeafData @@ Base16.decode(txId).get, levels)

    merkleProof.valid(Digest32 @@ txsRoot) shouldBe true
  }

}
