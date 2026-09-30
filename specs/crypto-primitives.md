# Cryptographic Primitives

**Status:** Draft
**Scope:** Consensus-critical cryptographic primitives of the Ergo protocol.
Wallet-level cryptography (BIP39 mnemonics, HD key derivation, AES-GCM secret
storage) is intentionally out of scope; it is implementation-level, not
protocol-level.

This document is extracted and reworked from the yellow paper draft
(`papers/yellow/YellowPaper.tex`, section "Cryptographic Primitives", and
`papers/yellow/pow/ErgoPow.tex`), grounded in the reference implementation.

---

## 1. Hash function: Blake2b-256

Ergo uses a single cryptographic hash function for all consensus purposes:
**BLAKE2b with a 256-bit (32-byte) digest** (RFC 7693).

Reference implementation: `scorex.crypto.hash.Blake2b256` (BouncyCastle),
exposed as `Algos.hash` in
`ergo-core/src/main/scala/org/ergoplatform/settings/Algos.scala`.

The key words "MUST", "SHOULD" below are to be interpreted as in RFC 2119.

### 1.1 Consensus uses of Blake2b-256

| Use | Definition |
|-----|-----------|
| Transaction id | `Blake2b256(messageToSign)` where `messageToSign` is the transaction serialized with all spending proofs empty (`ErgoTransaction.scala:68`) |
| Transaction witness id | `Blake2b256(concat(spendingProofs))`, **first byte dropped** — 248 bits, to distinguish witness ids from 256-bit transaction ids in the same Merkle tree (`ErgoTransaction.scala:77-78`) |
| Header id | `Blake2b256(headerBytes)` of the fully serialized header including the PoW solution (`Header.scala:62`) |
| ADProofs digest | `Blake2b256(serializedProofBytes)` (`ADProofs.scala:78`) |
| Merkle trees | Transaction tree, extension tree (see §4) |
| PoW message | `msg = Blake2b256(headerWithoutPow)` (`AutolykosPowScheme.msgByHeader`) |
| Autolykos puzzle | Element generation, index generation, hit computation (see §6) |
| Interlinks / NiPoPoW | Header ids and level computation (see §7) |

Any alternative hash function or digest length MUST be considered a consensus
change.

---

## 2. Elliptic curve: secp256k1

Ergo uses the **secp256k1** elliptic curve (SEC 2) as the discrete-logarithm
group for all signature and Σ-protocol purposes, and inside Autolykos v1.

Reference implementation: `sigma.crypto.CryptoConstants.dlogGroup`
(BouncyCastle), wrapped in
`ergo-core/src/main/scala/org/ergoplatform/mining/mining.scala`.

### 2.1 Group parameters

- Curve: `y² = x³ + 7` over GF(p), p = 2²⁵⁶ − 2³² − 977
- Generator `g`: standard secp256k1 base point
- Group order `q` = `0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141`
- The order `q` is additionally used by the consensus layer to derive the PoW
  target: `b = q / difficulty` (`AutolykosPowScheme.getB`), and the NiPoPoW
  "real difficulty" of a header: `realDifficulty = q / powHit`.

### 2.2 Point encoding

- Public keys and group elements are **compressed points, 33 bytes**
  (`PublicKeyLength = 33`, `mining.scala:13`).
- Serialization is `sigma.serialization.GroupElementSerializer`
  (SEC 1 compressed format).

### 2.3 Consensus uses

1. **Schnorr proofs of knowledge of discrete logarithm** (`proveDlog`) — the
   base authentication mechanism of ErgoScript (see §3).
2. **Autolykos v1** — non-outsourceability key pair `pk = g^sk`, one-time
   PK `w = g^x`, and the check `w^f = g^d · pk`
   (`AutolykosPowScheme.checkPoWForVersion1`). Both points MUST lie on the
   curve and MUST NOT be the point at infinity.
3. **Diffie-Hellman tuples** (`proveDHTuple`) in ErgoScript — same group.

---

## 3. Σ-protocols (Schnorr identification, Fiat-Shamir)

Spending a box means proving a Σ-boolean expression: a tree whose leaves are
dlog (`proveDlog`) or Diffie-Hellman-tuple (`proveDHTuple`) predicates and
whose internal nodes are AND/OR (threshold) connectives. Proofs are made
non-interactive via the **Fiat-Shamir transform**, hashed with Blake2b-256
over the commitment, the proven statement tree, and the message
(`messageToSign`).

The proving algorithm (yellow paper "Signing A Transaction", cleaned up):

1. **Mark real/simulated (bottom-up).** A dlog leaf is *real* if the prover
   knows the secret, else *simulated*. OR is real if at least one child is
   real. AND is real only if all children are real. The root MUST end up
   real, otherwise no proof exists for this prover.
2. **Propagate simulation (top-down).** Every child of a simulated node is
   simulated. If a real OR has more than one real child, all but one are
   marked simulated.
3. **Assign challenges to simulated nodes (top-down).** Each simulated child
   of an OR gets a fresh random challenge. Children of a simulated AND
   inherit the AND's challenge.
4. **Commitments (bottom-up).** Simulated leaves run the Schnorr simulator
   (response + commitment). Real leaves compute the Schnorr first message.
   The commitment of an AND/OR node is the set-union of its children's
   commitments.
5. **Root challenge.** The Fiat-Shamir challenge is the hash of the root
   commitment, the tree being proven, and the message.
6. **Real challenges (top-down).** For a real OR, the one real child's
   challenge is the XOR of the OR's challenge and all simulated siblings'
   challenges. For a real AND, every real child inherits the AND's challenge.

The proof is then flattened into a binary string of `(e, z)`
(challenge, response) pairs per leaf sub-protocol and serialized into the
spending proof.

> **Note.** The yellow paper draft ends mid-sentence before the flattening
> description; the full normative text lives in the sigma-state specification
> (see References). This spec defers Σ-proof serialization to it.

---

## 4. Merkle trees

Reference implementation: `scorex.crypto.authds.merkle.MerkleTree` (scrypto),
wrapped by `Algos.merkleTree` / `Algos.merkleTreeRoot`.

Construction over Blake2b-256 with **domain separation**:

- Leaf: `H(0x00 ‖ data)` (`LeafPrefix = 0`)
- Internal node: `H(0x01 ‖ left ‖ right)` (`InternalNodePrefix = 1`)
- Nodes with two empty children are `null` (carry no hash)
- Root of an empty tree instance: 32 zero bytes
- **Special case (consensus-relevant):** `Algos.merkleTreeRoot(∅)` returns
  `Blake2b256("")` = `0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8`,
  NOT the zero root of an empty `MerkleTree` instance
  (`Algos.scala:34-39`, ergo issue #1077). Implementations MUST reproduce
  this exactly.

### 4.1 Transaction Merkle tree

The miner commits to block transactions in the header's `transactionsRoot`.

- **Block version 1:** leaves are the transaction ids
  (`BlockTransactions.scala:60`).
- **Block version 2+:** leaves are `txIds ++ witnessIds` — each transaction
  id (32 bytes) followed by its witness id (31 bytes, see §1.1)
  (`BlockTransactions.scala:62`). Witness commitments were added by the
  hardening fork to make the tree cover spending proofs.

> **Divergence from the yellow paper.** The draft describes a leaf as
> `hash(0 ‖ pos ‖ data)` with 64-byte `data = txId ‖ digest(proofs)`. The
> reference implementation does not include the position in the leaf hash
> (position is implicit in tree structure) and, since block version 2, appends
> 31-byte witness ids as separate leaves instead of 64-byte combined leaves.
> This spec follows the implementation.

### 4.2 Extension Merkle tree

The extension section is committed to in the header's `extensionRoot` via the
same tree construction over the extension's key-value fields
(`Extension.scala`).

---

## 5. Authenticated state: AVL+ trees

The UTXO set is authenticated with an **AVL+ tree** — a balanced binary
authenticated dictionary supporting batch proofs — as described in
[eprint 2016/994](https://eprint.iacr.org/2016/994).

Reference implementation: `scorex.crypto.authds.avltree.batch.BatchAVLProver`
(avldb / scrypto), instantiated in
`src/main/scala/org/ergoplatform/nodeView/state/UtxoState.scala:291` with:

- **Key length: 32 bytes** (box ids, `ADProofs.KL = 32`)
- **Value length: variable** (`valueLengthOpt = None`) — values are
  serialized boxes
- Hash function: Blake2b-256

### 5.1 Consensus roles

1. **State root in header.** The header's `stateRoot` is the AVL+ digest of
   the UTXO set *after* the block is applied. The digest is **33 bytes**:
   32-byte root hash plus 1 byte of tree height
   (`HeaderWithoutPow.scala:14`).
   > **Divergence from the yellow paper.** The header table in `block.tex`
   > lists `stateRoot` as 32 bytes; the implementation (and consensus
   > serialization) is 33 bytes.
2. **ADProofs.** A block's ADProofs section contains the batch proof of all
   AVL+ lookups/insertions/removals performed by the block's transactions.
   A node holding only the 33-byte digest (light-fullnode mode) can verify
   all state transitions of a block from `oldDigest + blockTransactions +
   ADProofs → newDigest`, and the new digest MUST equal the header's
   `stateRoot` (validation rules 500–501).
3. **Digest commitment.** `ADProofsRoot = Blake2b256(serialized ADProofs)`
   in the header (§1.1).
4. **UTXO snapshots.** Pruned-bootstrapping nodes download manifest/chunk
   snapshots whose root MUST match the AVL+ digest at the snapshot height
   (`UtxoState` manifest/subtree types).

---

## 6. Autolykos proof-of-work (primitives)

Full normative treatment is deferred to the Autolykos spec (source:
`papers/yellow/pow/ErgoPow.tex`); this section fixes only the cryptographic
primitives and parameters the rest of the protocol depends on.

Reference implementation:
`ergo-core/src/main/scala/org/ergoplatform/mining/AutolykosPowScheme.scala`,
consensus parameters `k = 32`, `n = 26` (`src/main/resources/application.conf`).

### 6.1 Common

- PoW message: `m = Blake2b256(headerWithoutPow)` (32 bytes).
- Target: `b = q / decodeCompactBits(nBits)`, where `q` is the secp256k1
  group order (§2.1). A solution is valid iff `hit < b`.
- Padding constant `M`: the 1024 big-endian 64-bit integers 0..1023
  concatenated (8 KiB), mixed into element hashes to slow evaluation
  (`AutolykosPowScheme.M`).
- Index generator: `genIndexes(seed, N)` — compute `h = Blake2b256(seed)`,
  extend to 35 bytes as `h ‖ h[0..3)`, then take `k` consecutive 4-byte
  big-endian slices mod `N` (`genIndexes`).
- Table size `N(height)` (block version ≥ 2): `N₀ = 2²⁶`; starting at height
  614,400 (`600·1024`), `N` grows by 5% every 51,200 blocks (`50·1024`);
  growth stops at height 4,198,400, where `N = 2,143,944,600 < 2³¹`
  (`calcN`). For version 1 headers `N = 2²⁶` always.

### 6.2 Autolykos v1 (block version 1)

- Hash into the group: `hashModQ(x)` — Blake2b-256 with **rejection
  sampling**: interpret the digest as an unsigned 256-bit integer; accept and
  reduce mod `q` iff it is below `⌊2²⁵⁶/q⌋·q`, otherwise re-hash the digest
  (`ModQHash`). This yields uniform elements of Zq.
- Elements: `e(j) = hashModQ(j ‖ M ‖ pk ‖ m ‖ w)`.
- Check: `w^f = g^d · pk` with `f = Σ e(jᵢ) mod q` over the `k` generated
  indexes, plus `d < b` and on-curve, non-infinity `pk`, `w`.

### 6.3 Autolykos v2 (block version ≥ 2)

- Elements: `e(j) = Blake2b256(j ‖ h ‖ M)` with the **first byte dropped**
  (248-bit big-endian integer), where `h` is the block height as 4 bytes.
- Seed: `f₀ = Blake2b256(i ‖ h ‖ M)` (drop first byte), where
  `i = Blake2b256(m ‖ nonce)[24..32) mod N`; then `seed = f₀ ‖ m ‖ nonce`.
- Hit: `hit = Blake2b256(f₂)` as unsigned integer, where `f₂` is the
  32-byte big-endian sum of the `k` selected elements.
- Valid iff `hit < b`. The hit doubles as the header's `powHit` for NiPoPoW.

---

## 7. NiPoPoW-relevant primitives

- `powHit(header)`: for version 1 headers, the solution value `d`; for
  version ≥ 2, `hitForVersion2(header)` (§6.3).
- `realDifficulty(header) = q / powHit(header)`
  (`AutolykosPowScheme.scala:232-234`) — the actual work evidenced by the
  header, used for NiPoPoW level computation.
- Interlinks are committed via the extension section (see `block.tex`,
  key prefix `0x01`) and thus to the extension Merkle root (§4.2).

---

## 8. Randomness

Consensus does not mandate a randomness source. The reference implementation
uses a cryptographically secure RNG (`DLogProverInput.random()`,
`scorex.utils.Random`) for secret generation (Autolykos secrets, Schnorr
nonces). Weak randomness in Schnorr nonces leaks private keys;
implementations MUST use a CSPRNG or deterministic nonce generation.

---

## References

- RFC 7693 — The BLAKE2 Cryptographic Hash and MAC
- SEC 2 — Recommended Elliptic Curve Domain Parameters (secp256k1)
- A. Reyzin, D. Meshkov, A. Chepurnoy, S. Ivanov —
  [Improving Authenticated Dynamic Dictionaries](https://eprint.iacr.org/2016/994) (AVL+)
- `papers/yellow/pow/ErgoPow.tex` — ErgoPow: Autolykos specification
- sigma-state (ErgoTree/ErgoScript and Σ-protocol serialization):
  https://github.com/ScorexFoundation/sigma-state
- Reference implementation files cited inline throughout this document.
