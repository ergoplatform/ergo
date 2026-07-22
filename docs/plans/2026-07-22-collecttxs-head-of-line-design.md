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
transaction consuming or reading any recorded output through a regular or data
input is also deferred, and its outputs are recorded transitively. This
prevents dependents from being misclassified as invalid merely because an
ancestor was omitted from the current candidate.

Once that dependency is known, deferral takes precedence over a conflict with
the current candidate prefix: the dependent may be valid with a different
prefix, and the old collector never reached it after its ancestor overflowed.
Transactions without a deferred dependency retain the existing invalid and
double-spend handling.

The implementation preserves the current strict cost and size comparisons,
counts the fee-collection transaction exactly once, and keeps the relative order
of every accepted transaction.

## Verification

The primary regression uses this ordered sequence:

1. an accepted transaction;
2. a valid parent that no longer fits;
3. a valid transaction reading that parent's output as a data input, while
   also conflicting with the accepted prefix;
4. two generations of spending descendants;
5. a separate conflict without a deferred dependency;
6. a smaller independent transaction that fits.

The expected candidate contains the accepted and independent transactions plus
the fee transaction. The deferred family is omitted without elimination, the
separate conflict remains eliminated, and the final candidate remains below
both limits. Separate size and cost fixtures assert that equality with either
limit is still rejected by the existing strict comparison.

Existing tests continue to cover invalid transactions, double spends, fee
collection, and block cost and size limits.

## Alternatives

A plain continue after overflow is rejected because the skipped transaction's
descendants would subsequently appear to have missing inputs and could be
eliminated from the mempool.

A dependency-package selector is intentionally deferred to a separate
refactoring. It requires an explicit scoring policy for shared ancestors, fees,
cost, size, conflicts, and prioritized transactions. This focused change fixes
the liveness defect without changing those policies. In particular, this pass
propagates only from a producer already encountered and deferred; it does not
topologically reorder data-input dependencies whose producer appears later in
the mempool sequence.
