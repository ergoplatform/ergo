# Σ-Proof (Signature) Serialization

**Status:** Draft
**Scope:** Normative binary format of Ergo Σ-protocol spending proofs — the
byte strings carried in transaction inputs — including the Fiat-Shamir
transcript format used to compute and verify the root challenge.

This document fills the gap left in the yellow paper draft
(`papers/yellow/YellowPaper.tex`, "Signing A Transaction", which ends
mid-sentence before the flattening description). It is grounded in the
reference implementation, `sigmastate-interpreter` (see References).

The key words "MUST", "MUST NOT", "SHOULD" are to be interpreted as in
RFC 2119. `H` denotes Blake2b-256 (see `crypto-primitives.md`).

---

## 1. Background and data types

A box is guarded by a Σ-boolean expression: a tree with

- **leaves**: `ProveDlog(h)` — prove knowledge of `w` with `g^w = h`;
  `ProveDHTuple(g, h, u, v)` — prove knowledge of `w` with `u = g^w`,
  `v = h^w` (Chaum-Pedersen);
- **internal nodes**: `AND`, `OR`, and `THRESHOLD(k, children)`
  (k-of-n, at most 255 children).

All group elements are secp256k1 points; `q` is the group order
(see `crypto-primitives.md` §2).

Two fixed-size parameters govern the format:

| Parameter | Value | Definition |
|-----------|-------|-----------|
| Challenge size | **24 bytes** (192 bits) | `CryptoConstants.soundnessBits = 192`, `SigSerializer.hashSize`. Challenges are obtained by truncating `H(...)` to its first 24 bytes (`CryptoFunctions.hashFn`). MUST NOT be changed without replacing the GF(2¹⁹²) threshold polynomial arithmetic — soundness (192 bits) is coupled to it. |
| Response size | **32 bytes** | `SigSerializer.order = sigma.crypto.groupSize`. Responses `z` are Zq elements, serialized as unsigned big-endian, left-padded to 32 bytes (`BigIntegers.asUnsignedByteArray(32, z)`). |

A parsed proof is an `UncheckedTree` — same shape as the proposition, each
node carrying a `challenge` (24 bytes), leaves additionally carrying the
response `z`. The empty proposition proof is `NoProof`, serialized as an
**empty byte array**.

---

## 2. Container format (transaction input)

Within a transaction input, the proof travels inside a `ProverResult`
(`data/.../sigma/interpreter/ProverResult.scala`):

```
proofLength : UShort        -- VLQ-encoded unsigned, value range 0..65535
proof       : Byte[proofLength]   -- the Σ-proof bytes of §3; empty = NoProof
extension   : ContextExtension
                count : UByte                 -- number of pairs, <= 127
                pairs : (id : Byte, value : SValue)[count]
                          -- id >= 0; value is a serialized sigma expression
```

The extension (but NOT the proof bytes) is part of the transaction's
`messageToSign`: inputs are serialized for signing via `inputToSign`, which
empties only the proof byte array and keeps the extension
(`org/ergoplatform/Input.scala:50`,
`ErgoLikeTransaction.bytesToSign`). Consequently the extension is covered by
the transaction id and by the Fiat-Shamir transcript (§6), and any
modification of it invalidates the proof. The proof bytes alone (not the
extension) participate in the transaction witness id
(`crypto-primitives.md` §1.1); the proof bytes themselves never enter the
Fiat-Shamir transcript — they only carry the challenges/responses checked
against it.

> Note: the yellow paper draft says the input contains a "VLQ-encoded length
> of signature"; the implementation uses an unsigned-short VLQ, so proofs are
> limited to 65535 bytes.

---

## 3. Proof byte format (`SigSerializer.toProofBytes`)

The proof contains **only challenges and responses**. Propositions and
commitments are NOT serialized — the verifier knows the proposition from the
box and recomputes the commitments (§5).

Serialization is a pre-order traversal of the proof tree. A per-node flag
`writeChallenge` controls whether the node's 24-byte challenge is emitted:

```
serialize(node, writeChallenge):
  if writeChallenge: emit node.challenge                    -- 24 bytes
  match node:
    UncheckedSchnorr | UncheckedDiffieHellmanTuple:
      emit z                                                -- 32 bytes BE
    CAndUncheckedNode:
      for each child: serialize(child, writeChallenge = false)
        -- children's challenges equal this node's challenge
    COrUncheckedNode(children c₁..cₙ):
      for i in 1..n-1: serialize(cᵢ, writeChallenge = true)
      serialize(cₙ, writeChallenge = false)
        -- last child's challenge is recovered by XOR
    CThresholdUncheckedNode(k, children c₁..cₙ):
      emit polynomial                                       -- see below
      for each child: serialize(child, writeChallenge = false)
        -- children's challenges come from polynomial evaluation
```

The root is always serialized with `writeChallenge = true`, so every
non-empty proof begins with the 24-byte root challenge.

**Threshold polynomial.** For `THRESHOLD(k, n children)` the prover builds a
degree-(n−k) polynomial `Q` over GF(2¹⁹²) with `Q(0) = challenge`. The proof
carries the coefficients of degrees 1..(n−k), each a 24-byte GF(2¹⁹²)
element, lowest degree first — `(n−k) * 24` bytes total
(`GF2_192_Poly.toByteArray(coeff0 = false)`). The degree-0 coefficient is
omitted: it equals the node's challenge, already serialized.

**Field arithmetic.** GF(2¹⁹²) uses the irreducible polynomial
`x¹⁹² + x⁷ + x² + x + 1` (`GF2_192`). Elements are 24-byte little-endian
words (byte 0 = least significant). Evaluation points are the single bytes
1, 2, …, n embedded into the field.

---

## 4. Parsing (`SigSerializer.parseAndComputeChallenges`)

Parsing is driven by the **proposition** (known from the box's ErgoTree),
which fixes the tree shape; the bytes only supply challenges and responses.
This is Verifier Steps 1–3:

1. If the proof is empty → `NoProof` (verification MUST fail).
   Otherwise read the 24-byte root challenge.
2. Top-down, derive each node's challenge `e₀` (24 bytes):
   - read from the proof if the parent did not provide one;
   - **AND**: every child inherits `e₀`;
   - **OR**: children 1..n−1 are read from the proof; the last child's
     challenge is `e₀ ⊕ e₁ ⊕ … ⊕ eₙ₋₁` (bytewise XOR of 24-byte arrays);
   - **THRESHOLD(k, n)**: read `(n−k)·24` coefficient bytes, build
     `Q = GF2_192_Poly(challenge ‖ coeffs)`, then child `i` gets
     `Q(i)` for `i = 1..n` (single-byte evaluation point).
3. At each leaf, read the 32-byte response `z` (unsigned big-endian Zq
   element).

**Canonicality.** Consensus accepts non-canonical encodings — `z ≥ q`
(reduced implicitly during exponentiation), trailing bytes, short reads as
above. Since block version ≥ 2 commits the exact witness bytes into
`transactionsRoot`, mempool/policy layers SHOULD reject non-canonical proofs
to prevent wtxid malleation.

Notes on robustness, following the implementation:

- The parser consumes exactly as many bytes as the proposition's structure
  dictates; bytes remaining after the parse are NOT checked. (Witness
  commitments in block version ≥ 2 headers bind the exact proof bytes into
  the block's transaction Merkle tree — see `crypto-primitives.md` §4.1.)
- Short reads are tolerated during parsing (`readBytesChecked` only logs a
  warning); the resulting tree then fails the root-challenge comparison in
  §6, and any exception raised while parsing or verifying is caught and
  treated as verification failure (`Interpreter.verifySignature`).

---

## 5. Commitment recomputation (Verifier Step 4)

For every leaf, the verifier recomputes the first prover message (commitment)
from the challenge `e` (as unsigned big-endian integer) and response `z`.
(The yellow-paper verifier description allows a leaf to reject here; in the
reference implementation the commitment is always computable, and acceptance
is decided solely by the root-challenge comparison in §6.)

**Schnorr leaf** (`ProveDlog(h)`), `DLogProver.computeCommitment`:

```
a = g^z · h^(−e)          (point multiplication; −e mod q)
```

**Diffie-Hellman tuple leaf** (`ProveDHTuple(g, h, u, v)`),
`DiffieHellmanTupleProver.computeCommitment`:

```
a = g^z · u^(−e)
b = h^z · v^(−e)
```

(For reference, the prover side computes `z = (r + e·w) mod q` with
commitment(s) `a = g^r` resp. `a = g^r, b = h^r`; simulation picks random
`z` and derives `a` resp. `(a, b)` with the same formulas as the verifier.)

---

## 6. Fiat-Shamir transcript (Verifier Steps 5–6)

After commitments are recomputed, the verifier accepts iff the root challenge
equals:

```
challenge_root == H( FiatShamirTree.toBytes(root) ‖ message )[0..24)
```

where `message` is the transaction's `messageToSign` and the hash output is
truncated to 24 bytes (§1). The prover computes the root challenge
identically (Prover Step 7). The transcript serialization MUST be
unambiguous and MUST NOT contain challenges, responses, or real/simulated
flags — only propositions and commitments (`FiatShamirTree.toBytes`):

```
Leaf (prefix 0x01):
  0x01
  propLen  : 2 bytes, big-endian          -- putShortBytes
  prop     : ErgoTree serialization of SigmaPropConstant(proposition),
             with ZeroHeader and constant segregation
  commLen  : 2 bytes, big-endian
  comm     : commitment bytes
               Schnorr: 33-byte compressed point a
               DHT:     66 bytes — compressed a ‖ compressed b

Internal node (prefix 0x00):
  0x00
  conjectureType : 1 byte   -- 0 = AND, 1 = OR, 2 = THRESHOLD
  k              : 1 byte   -- only if conjectureType == THRESHOLD
  childCount     : 2 bytes, big-endian
  children serialized recursively in order
```

---

## 7. Size summary

| Node | Bytes contributed |
|------|-------------------|
| Root | 24 (challenge) |
| Schnorr / DHT leaf | 32 (z) + 24 if challenge written |
| AND | 0 beyond children |
| OR (n children) | 24 × (n−1) extra challenges |
| THRESHOLD(k, n) | 24 × (n−k) polynomial coefficients |

Single-key P2PK proof = 24 (challenge) + 32 (z) = **56 bytes**.

---

## 8. Test vectors

Cross-check against `SigSerializerSpecification` and
`ProveDlogSerializerSpec` / `PDHTSerializerSpecification` in the
sigmastate-interpreter test suite (see References). A minimal sanity check:
a `ProveDlog` proof MUST be 56 bytes; parsing any 56-byte string against a
`ProveDlog` proposition MUST NOT fail at the parsing stage (only at
verification).

---

## References

Reference implementation (sigmastate-interpreter):

- `interpreter/shared/src/main/scala/sigmastate/SigSerializer.scala` —
  proof bytes format and parsing (§3, §4)
- `interpreter/shared/src/main/scala/sigmastate/UnprovenTree.scala` —
  `FiatShamirTree.toBytes`, conjecture type ids (§6)
- `interpreter/shared/src/main/scala/sigmastate/crypto/DLogProtocol.scala`,
  `DiffieHellmanTupleProtocol.scala` — commitment computation (§5)
- `interpreter/shared/src/main/scala/sigmastate/crypto/GF2_192.scala`,
  `GF2_192_Poly.scala` — threshold field arithmetic (§3)
- `interpreter/shared/src/main/scala/sigmastate/crypto/CryptoFunctions.scala` —
  24-byte challenge derivation from Blake2b-256
- `data/shared/src/main/scala/sigma/interpreter/ProverResult.scala`,
  `ContextExtension.scala` — container format (§2)
- ErgoScript whitepaper, Appendix A — prover/verifier steps narrative:
  https://ergoplatform.org/docs/ErgoScript.pdf
