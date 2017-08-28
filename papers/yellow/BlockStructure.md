# Ergo Block Structure

## **ErgoMinimalHeader** is a minimal data amount, required to calculate blockId:
```
payloadRootHash: Array[Byte] - root hash (or simple hash of all payload data) of block payload
nonce: Int - field to iterate and generate valid PoW
```

## **ErgoHeader** is a header to keep in History and transfer: 
```

---------------------------------------------------------------------------------------------
|Field            |  Size | Decription                                                      |
---------------------------------------------------------------------------------------------
|version          |  1    | block version, to be increased on every soft- and hardfork      |
|parentId         |  32   | id of parent block                                              |
|interlinksRoot   |  32   | root hash of interlinks structure                               |
|ADProofsRoot     |  32   | hash of ADProofs for transactions in a block                    |
|stateRoot        |  32   | root hash (for an AVL+ tree) of a state after block application |
|transactionsRoot |  32   | root hash (for a Merkle tree) of transactions in a block        |
|timestamp        |  8    | block timestamp(in milliseconds since beginning of Unix Epoch)  |
|nonce            |  8    | Proof-of-Work nonce                                             |
---------------------------------------------------------------------------------------------
```
Some of this fields may be calculated by node by itself:
- parentId: if(status==bootstrap && PoPoWBootstrap == false) (kushti: ???)
- interlinksRoot: if(PoPoWBootstrap == false)
- ADProofsRoot: if(status==regular && ADState==false && BlocksToKeep>0) 
- stateRoot: if(status==regular && ADState==false && BlocksToKeep>0) 