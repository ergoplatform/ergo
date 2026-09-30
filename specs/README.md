# Ergo Protocol Specifications

This directory contains the normative specifications of the Ergo protocol,
extracted and reworked from the yellow paper draft in `papers/yellow/`.

Each document specifies one area of the protocol. Where a spec and the yellow
paper draft diverge, the spec follows the reference implementation (which is
the consensus truth) and notes the divergence.

## Status

| Spec | Status | Source material |
|------|--------|-----------------|
| [Cryptographic Primitives](crypto-primitives.md) | Draft | `papers/yellow/YellowPaper.tex`, `papers/yellow/pow/ErgoPow.tex` |
| [Σ-Proof Serialization](sigma-proofs.md) | Draft | `papers/yellow/YellowPaper.tex` ("Signing A Transaction"), sigmastate-interpreter |

## Planned specs

- Serialization and notation (VLQ, encodings)
- Boxes, transactions and their validation
- Block structure (header, block sections, extension)
- Consensus validation rules (from `modifiersValidation.tex`)
- Emission, storage rent and economic rules (from `economy.tex`)
- Voting and parameter changes (from `voting.tex`)
- Autolykos proof-of-work (from `pow/ErgoPow.tex`)
- Modes of operation and synchronization (from `YellowPaper.tex`, `sync.tex`)
