# Capacity-Aware Candidate Transaction Selection

## Problem

`CandidateGenerator.collectTxs` currently stops scanning the prioritized
transaction sequence as soon as one valid transaction would make the candidate
exceed its cost or size limit. A transaction that does not fit can therefore
prevent later, independent transactions from being considered even when they
would fit.

This is a mining-policy issue. It does not change transaction validity,
mempool priority, or consensus limits.

## Design

Candidate assembly remains a single pass over the existing priority order. Each
transaction has one of three outcomes:

- accepted: the transaction and updated fee transaction fit, so both candidate
  state and resource accounting advance;
- invalid: existing validation or conflict checks fail, so the transaction ID
  remains eligible for elimination;
- deferred: the transaction is valid but does not fit, so candidate state stays
  unchanged and the scan continues.

When a transaction is deferred, its output IDs are recorded. A later
transaction consuming any recorded output is also deferred, and its outputs are
recorded transitively. This prevents descendants from being misclassified as
invalid merely because their parent was omitted from the current candidate.

The implementation preserves the current strict cost and size comparisons,
counts the fee-collection transaction exactly once, and keeps the relative order
of every accepted transaction.

## Verification

The primary regression uses this ordered sequence:

1. a valid parent that does not fit;
2. a valid child spending that parent's output;
3. a smaller independent transaction that fits.

The expected candidate contains the independent transaction and its fee
transaction. The parent and child are omitted, neither ID is eliminated, and
the final candidate remains below both limits.

Existing tests continue to cover invalid transactions, double spends, fee
collection, and block cost and size limits.

## Alternatives

A plain continue after overflow is rejected because the skipped transaction's
descendants would subsequently appear to have missing inputs and could be
eliminated from the mempool.

A dependency-package selector is intentionally deferred to a separate
refactoring. It requires an explicit scoring policy for shared ancestors, fees,
cost, size, conflicts, and prioritized transactions. This focused change fixes
the liveness defect without changing those policies.
