Ergo Modifiers Processing
-------------------------

This document describes processing algorithm for Ergo modifiers in all security modes.  
Unlike most of blockchain systems, Ergo have the following types of modifiers:
- Transaction
- BlockTransactions - Sequence of transactions, corresponding to 1 block
- ADProofs - proof of transaction correctness relative to corresponding UTXO
- ??? Interlinks
- Header, that contains data required to verify PoW, link to previous block and root hash to it's payload (BlockTransactions, ADProofs, Interlinks, ...)

Fullnode
=========

For full node modifiers precessing workflow:

1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better then our best block
3. Request headers for all ids from 2.
4. On receiving header
```scala
 if(History.apply(header).isSuccess) {
    if(!isInitialBootstrapping) Broadcast INV for this header
    Request transaction ids from this block
 } else {
    blacklist peer
 }
```
5.On receiving transaction ids from header:
```scala
  History.apply(transactionIdsForHeader)
  transactionIdsForHeader.filter(txId => !MemPool.contains(txId)).foreach { txId => 
    request transaction with txId
  }
```
6. On receiving a transaction:
```scala
 if(Mempool.apply(transaction).isSuccess) {
    if(!isInitialBootstrapping) Broadcast INV for this transaction
    history.getHeadersWithoutTransactionsAndTheirTransactionIds.foreach { (h, txIds) =>
       if(txIds.forall(txId => Mempool.contains(txId))) {
          GOTO 7
       }
    }
 }
```
7. Now we have BlockTrasactions: all transactions corresponding to some Header
```scala
  if(History.apply(BlockTrasactions) == Success(ProgressInfo)) {
      if(!isInitialBootstrapping) Broadcast INV for BlockTrasactions // ?? Whe should notify our neighbours, that now we have all the transactions
     //State apply modifiers (may be empty for block in a fork chain) and generate ADProofs for them
     if(minimalState().apply(ProgressInfo) == Success(ADProofs)) {
       ADProofs.foreach { ADProof =>
         History.apply(ADProof)
       }
     }
  } else {
    blacklist peer who sent header
  }
```

Pruned-Fullnode
===============

("mode"="pruned-full" to enable this mode)
   
This mode is similar to fast-sync in Geth or Grothendieck, warp-mode 
in Parity (all the three are Ethereum protocol clients), but makes
 more aggressive optimizations. In particular, a pruned-fullnode is 
 not downloading and storing full blocks not residing in a target 
 blockchain suffix, and also removing full blocks going out of the suffix.
 
In detail, a pruned client is downloading all the headers, then, by using them,
  it checks proofs-of-work and linking structure(or parent id only?). Then it downloads a UTXO 
  snapshot for some height from its peers. Finally, full blocks after the snapshot are to be downloaded
   and applied to get a current UTXO set.
 
A pruned fullnode is also skipping AD-transformation block part, like a fullnode.      
   
Additional setting: "suffix" - how much full blocks to store(w. some minimum set?)
   
   
Light-Fullnode   
==============

("mode"="light-full" to enable this mode)

This mode is based on an idea to use a 2-party authenticated dynamic dictionary built on top of
UTXO set. A light-fullnode holds only a root digest of a dictionary. It check all the full blocks, or some
suffix of the full blockchain, depending on setting, thus starting from a trusted pre-genesis digest or some digest in 
the blockchain. A light-fullnode is using AD-transformations (authenticated dictionary transformations) block section
containing batch-proof for UTXO transformations to get a new digest from an old one. It also checks all the transactions, 
but doesn't store anything but a single digest for that. Details can be found in the paper 
https://eprint.iacr.org/2016/994.

Additional settings : "depth" - from which block in the past to check transactions (if 0, then go from genesis)

"additional-checks" - %% of random blocks to check before the suffix, if "depth" is finite. To check, a 
light-fullnode trusts previous digest and checks current digest validity by using the previous one as well
 as AD-transformations.
 
"additional-depth" - depth to start additional checks from.  

Light-SPV
=========

("mode"="light-spv" to enable this mode)

This mode is not about checking any full blocks. Like in Bitcoin, an SPV node is downloading block headers only,
and so checks only proofs of work and links. Unlike Bitcoin's SPV, the Light-SPV is downloading
 and checking not all the headers but a sublinear(in blockchain length) number of them(in benchmarks, this is about just
 tens of kilobytesm instead of tens or hundreds of megabytes for Bitcoin/Ethereum).
 
Light-SPV mode is intended to be useful for mobile phones and low-end hardware. 
   
   