Ergo Modifiers Processing
-------------------------

This document describes processing algorithm for Ergo modifiers in all security modes.

Unlike most of blockchain systems, Ergo have the following types of **modifiers**:
1. In-memory:
- Transaction - in-memory modifier
- transactionIdsForHeader - ids of transactions in concrete block
- UTXOSnapshotManifest - ids of UTXO chunks and 
2. Persistent:
- BlockTransactions - Sequence of transactions, corresponding to 1 block.
- ADProofs - proof of transaction correctness relative to corresponding UTXO
- Header, that contains data required to verify PoW, link to previous block, state root hash and root hash to it's payload (BlockTransactions, ADProofs, Interlinks ...)
- UTXOSnapshotChunk - part of UTXO
- PoPoWProof

Ergo node have the following **parameters**:
1. Mode: Enum("full", "pruned-full", "light-full", "light-spv") - allows to select node security model
2. BlocksToKeep: Int (for modes "pruned-full", "light-full") - number of last blocks to keep with transactions, for all other blocks it keep header only. if 0 - keep all blocks from genesis


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
      if(!isInitialBootstrapping) Broadcast INV for BlockTransactions // ?? We should notify our neighbours, that now we have all the transactions
     //State apply modifiers (may be empty for block in a fork chain) and generate ADProofs for them
     //TODO requires different interface from scorex-core, because it should return ADProofs
     //TODO when mininal state apply Progress info, it may also create UTXOSnapshot (e.g. every 30000 blocks like in Ethereum). This UTXOSnapshot should be required for mining by Rollerchain
     if(State().apply(ProgressInfo) == Success((newState, ADProofs))) {
       if("mode"="full" || "mode"=="pruned-full") ADProofs.foreach ( ADProof => History.apply(ADProof))
       if("mode"=="pruned-full" || "mode"=="light-full") drop BlockTransactions and ADProofs older than BlocksToKeep
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

Its **regular** modifiers processing is the same as for fullnode regime, while its **bootstrap** process is different:

1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better than our best block
3. Request headers for all ids from 2.
4. On receiving header
```scala
 if(History.apply(header).isSuccess) {
    if(!(localScore == networkScore)) GOTO 1
    else GOTO 5
 } else {
    blacklist peer
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
  History.headersStartingFromId(State.headerId).foreach { header => 
    send message(GetBlockTransactionsForHeader(header)) to Random fullnode
  }
```
9.On receiving BlockTransactions: ``` same as in Fullnode.7```
10.Operate as Fullnode
   
Light-Fullnode   
==============

1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better than our best block
3. Request headers for all ids from 2.
4. On receiving header
```scala
 if(History.apply(header).isSuccess) {
    if(!(localScore == networkScore)) GOTO 1
    else GOTO 5
 } else {
    blacklist peer
 }
```
5.Request BlockTransactions and ADProofs starting from BlocksToKeep back in History (just 1 last header after node botstrapping)
```scala
  History.lastBestHeaders(BlocksToKeep).foreach { header => 
    send message(GetBlockTransactionsForHeader(header)) to Random fullnode
    send message(GetAdProofsHeader(header)) to Random fullnode
  }
```
6.On receiving modifier: BlockTransactions or ADProofs
```scala
  if(History.apply(modifier) == Success(ProgressInfo)) {
  //TODO if history now contains both ADProofs and BlockTransactions, it should return ProgressInfo with both of them, otherwise it should return empty ProgressInfo
       if(State().apply(ProgressInfo) == Success((newState, ADProofs))) {
         if("mode"=="pruned-full") drop BlockTransactions and ADProofs older than BlocksToKeep
       } else {
         //Drop Header from history, because it's transaction sequence is not valid
         History.drop(BlockTransactions.headerId)
       }
  }
```

Light-Full-PoPoW
================

TODO - Mode, that have synchronization by PoPoWProof, download and verify full blocks with ADProof and discard it right after that

Light-SPV
=========

**boootstrap**
1. Send GetPoPoWProof for all connections
2. On receive PoPoWProof apply it to History (History should be able to determine, whether this PoPoWProof is better, than it's current best header chain)
3. GOTO regular regime

**regular**
1. Send ErgoSyncInfo message to connected peers
2. Get response with INV message, containing ids of blocks, better than our best block
3. Request headers for all ids from 2.
4. On receiving header
```scala
 if(History.apply(header).isSuccess) {
    State.apply(header) // just change state roothash
    if(!isInitialBootstrapping) Broadcast INV for this header
 } else {
    blacklist peer
 }
```
 

Generalization
==============

Lets have different set of parameters, instead of mode, will determine concrete History and State regime:
1. ADState: Boolean - keep state roothash only 
2. VerifyTransactions: Boolean - download block transactions and verify them (requires BlocksToKeep == 0)
3. PoPoWBootstrap: Boolean - download PoPoW proof only
4. BlocksToKeep: Int - number of last blocks to keep with transactions, for all other blocks it keep header only. Keep all blocks from genesis if negative
5. MinimalSuffix: Int - minimal suffix size for PoPoW proof (may be pre-defined constant)

Mode from previous sections can be determined as follows:
```scala
mode = if(ADState == false && VerifyTransactions == true && PoPoWBootstrap == false && BlocksToKeep < 0) "full"
else if(ADState == false && VerifyTransactions == true && PoPoWBootstrap == false && BlocksToKeep >= 0) "pruned-full"
else if(ADState == true && VerifyTransactions == true && PoPoWBootstrap == false) "light-full"
else if(ADState == true && VerifyTransactions == false && PoPoWBootstrap == true && BlocksToKeep == 0) "light-spv"
else if(ADState == true && VerifyTransactions == true && PoPoWBootstrap == true && BlocksToKeep == 0) "light-full-PoPoW"
else //Other combinations are possible
```

```scala
def updateHeadersChainToBestInNetwork() = {
  1.2.1. Send ErgoSyncInfo message to connected peers
  1.2.2. Get response with INV message, containing ids of blocks, better than our best block
  1.2.3. Request headers for all ids from 1.2.2.
  1.2.4. On receiving header
   if(History.apply(header).isSuccess) {
      if(!(localScore == networkScore)) GOTO 1.2.1
   } else {
      blacklist peer
      GOTO 1.2.1
   }
}
```

**boootstrap**
1.Download headers:
```scala
if(PoPoW) {
  1.1.1. Send GetPoPoWProof(suffix = Max(MinimalSuffix ,BlocksToKeep)) for all connections
  1.1.2. On receive PoPoWProof apply it to History (History should be able to determine, whether this PoPoWProof is better, than it's current best header chain)
} else {
  updateHeadersChainToBestInNetwork()
}
```
2.Download initial State to start process transactions:
```scala
if(ADState == true) {
  //Nothing to do, initialize state with state roothash from block header
} else if(BlocksToKeep < 0 || BlocksToKeep > History.headersHeight) {
  //Nothing to do, will calculate State by processing full blocks starting from genesis
} else {
  //We need to download full state BlocksToKeep back in history
  //TODO what if we can download state only "BlocksToKeep - N" or "BlocksToKeep + N" blocks back?
  2.1. Request historical UTXOSnapshotManifest for at least BlocksToKeep back
  2.2. On receiving UTXOSnapshotManifest: 
    UTXOSnapshotManifest.chunks.foreach ( chunk => request chunk from sender() //Or from random fullnode)
  2.3. On receiving UTXOSnapshotChunk
  State.applyChunk(UTXOSnapshotChunk) match {
     case Success(Some(newMinimalState)) => GOTO 3
     case Success(None) => stay at 2.3 //we need more chunks to construct state. TODO periodicaly request missed chunks
     case Failure(e) => ??? //UTXOSnapshotChunk or constcucted state roothash is invalid  
  }
}

```
3.Update State to best headers height
```scala
  if(State.bestHeader == History.bestHeader) {
    //Do nothing, State is already updated
  } else if(VerifyTransactions == false) {
    //Just update State rootshash to best header in history
    State.setBestHeader(History.bestHeader)
  } else {
    //we have headers chain better then full block         
    3.1. 
    assert(history contains header chain from State.bestHeader to History.bestHeaderx)
    History.continuation(from = State.bestHeader, size = ???).get.foreach { header => 
      sendToRandomFullNode(GetBlockTransactionsForHeader(header))
      if(ADState == true) sendToRandomFullNode(GetADProofsForHeader(header))
    }
    3.2. On receiving modifiers ADProofs or BlockTransactions
    //TODO History should return non-empty ProgressInfo only if it contains both ADProofs and BlockTransactions, or it contains BlockTransactions and ADState==false
    if(History.apply(modifier) == Success(ProgressInfo)) {
      if(State().apply(ProgressInfo) == Success((newState, ADProofs))) {
        if(ADState==false) ADProofs.foreach ( ADProof => History.apply(ADProof))
        if(BlocksToKeep>=0) remove BlockTransactions and ADProofs older than BlocksToKeep from history
      } else {
        //Drop Header from history, because it's transaction sequence is not valid
        History.drop(BlockTransactions.headerId)
      }
    } else {
      blacklistPeer
    }
    GOTO 3
  }
```
4. GOTO regular mode


**regular**
1.`updateHeadersChainToBestInNetwork()` // May work in a separate thread
2.Download and update full blocks when needed
```scala
  if(State.bestHeader == History.bestHeader) {
    //Do nothing, State is already updated
  } else if(VerifyTransactions == false) {
    //Just update State rootshash to best header in history
    State.setBestHeader(History.bestHeader)
  } else {
    //we have headers chain better then full block         
    3.1. Request transaction ids from all headers without transactions
      assert(history contains header chain from State.bestHeader to History.bestHeaderx)
      History.continuation(from = State.bestHeader, size = ???).get.foreach { header => 
        sendToRandomFullNode(GetBlockTransactionsForHeader(header))
        if(ADState == true) sendToRandomFullNode(GetADProofsForHeader(header))
      }
    3.2. On receiving transaction ids from header:
      Mempool.apply(transactionIdsForHeader)
      transactionIdsForHeader.filter(txId => !MemPool.contains(txId)).foreach { txId => 
        request transaction with txId
      }
    3.3. On receiving a transaction:
      if(Mempool.apply(transaction).isSuccess) {
         Broadcast INV for this transaction
         Mempool.getHeadersWithAllTransactions { BlockTransactions =>
            GOTO 3.4 //now we have BlockTransactions
         }
      }
    3.4. (same as 3.2. from bootstrap) On receiving modifiers ADProofs or BlockTransactions
    //TODO History should return non-empty ProgressInfo only if it contains both ADProofs and BlockTransactions, or it contains BlockTransactions and ADState==false
    if(History.apply(modifier) == Success(ProgressInfo)) {
      if(State().apply(ProgressInfo) == Success((newState, ADProofs))) {
        if(ADState==false) ADProofs.foreach ( ADProof => History.apply(ADProof))
        if(BlocksToKeep>=0) remove BlockTransactions and ADProofs older than BlocksToKeep from history
      } else {
        //Drop Header from history, because it's transaction sequence is not valid
        History.drop(BlockTransactions.headerId)
      }
    } else {
      blacklistPeer
    }
    GOTO 3
  }
```

