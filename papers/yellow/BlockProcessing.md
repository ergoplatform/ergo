Ergo Modifiers Processing
-------------------------

This document describes processing algorithm for Ergo modifiers in all security modes.

Unlike most of blockchain systems, Ergo have the following types of **modifiers**:
1. In-memory:
- Transaction - in-memory modifier
- transactionIdsForHeader - ids of transactions in concrete block
- UTXOSnapshotManifest - ids of UTXO chunks and 
2. Persistent:
- BlockTransactions - Sequence of transactions, corresponding to 1 block
- ADProofs - proof of transaction correctness relative to corresponding UTXO
- ??? Interlinks
- Header, that contains data required to verify PoW, link to previous block and root hash to it's payload (BlockTransactions, ADProofs, Interlinks, ...)
- UTXOSnapshotChunk - part of UTXO

Ergo node have the following **parameters**:
1. Mode: Enum("full", "pruned-full", "light-full", "light-spv") - allows to select node security model
2. BlocksToKeep: Int (for modes "pruned-full") - number of last blocks to keep with transactions, for all other blocks it keep header only.

Fullnode
=========

For full node regime, modifiers precessing workflow is as follows:

1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better than our best block
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
  Mempool.apply(transactionIdsForHeader)
  transactionIdsForHeader.filter(txId => !MemPool.contains(txId)).foreach { txId => 
    request transaction with txId
  }
```
6.On receiving a transaction:
```scala
 if(Mempool.apply(transaction).isSuccess) {
    if(!isInitialBootstrapping) Broadcast INV for this transaction
    Mempool.getHeadersWithAllTransactions { BlockTransactions =>
       GOTO 7
    }
 }
```
7.Now we have BlockTransactions: all transactions corresponding to some Header
```scala
  if(History.apply(BlockTransactions) == Success(ProgressInfo)) {
      if(!isInitialBootstrapping) Broadcast INV for BlockTransactions // ?? Whe should notify our neighbours, that now we have all the transactions
     //State apply modifiers (may be empty for block in a fork chain) and generate ADProofs for them
     //TODO requires different interface from scorex-core, because it should return ADProofs
     //TODO when mininal state apply Progress info, it may also create UTXOSnapshot (e.g. every 30000 blocks like in Ethereum). This UTXOSnapshot should be required for mining by Rollerchain
     if(minimalState().apply(ProgressInfo) == Success((newState, ADProofs))) {
       ADProofs.foreach { ADProof =>
         History.apply(ADProof)
       }
       if("mode"="pruned-full") drop BlockTransactions older than BlocksToKeep
     } else {
       //Drop Header from history, because it's transaction sequence is not valid
       History.drop(BlockTransactions.headerId)
     }
  } else {
    blacklist peer who sent header
  }
```

Pruned-Fullnode
===============


It's **regular** modifiers processing is the same as for fullnode regime

It's **bootstrap** process is different:

1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better than our best block
3. Request headers for all ids from 2.
4. On receiving header
```scala
 if(History.apply(header).isSuccess) {
    if(!) GOTO 1
    else GOTO 5
 } else {
s    blacklist peer
 }
```
5.Request historical UTXOSnapshotManifest for at least BlocksToKeep back
6.On receiving UTXOSnapshotManifest
```scala
  UTXOSnapshotManifest.chunks.foreach { chunk => 
    request chunk from sender() //Or from random fullnode
  }
```
7.On receiving UTXOSnapshotChunk
```scala
  State.applyChunk(UTXOSnapshotChunk) match {
     case Success(Some(newMinimalState)) => GOTO 8
     case Success(None) => stay at 7 //we need more chunks to construct state. TODO periodicaly request missed chunks
     case Failure(e) => ??? //UTXOSnapshotChunk or constcucted state roothash is invalid  
  }
```
8.Request BlockTransactions starting from State we have
```scala
  History.headersStartingFromId(State.headerId).foreach { h => 
    send message(GetBlockTransactionsForHeader(h)) to Random peer
  }
```
9.On receiving BlockTransactions: ``` same as in Fullnode.7```   
   
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
   
   