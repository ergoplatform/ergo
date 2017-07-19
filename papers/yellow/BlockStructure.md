# Ergo Block Structure

## **ErgoMinimalHeader** is a minimal data amount, required to calculate blockId:
payloadRootHash: Array[Byte] - root hash (or simple hash of all payload data) of block payload

nonce: Int - field to iterate and generate valid PoW

## **ErgoHeader** is a header to keep in History and transfer: 
version: Version - block version

parentId: BlockId - id of parent block

interlinksRoot: Array[Byte] - root hash of interlinks structure

ADProofsRoot: Array[Byte] - root hash of ADProofs for transactions in a block

stateRoot: Array[Byte] - root hash of a state before(**???**) block application 

transactionsRoot: Array[Byte] - root hash of transactions in a block

timestamp: Block.Timestamp - block timestamp

nonce: Int - field to iterate and generate valid PoW

Some of this fields may be calculated by node by itself:
- parentId: if(status==bootstrap && PoPoWBootstrap == false)
- interlinksRoot: if(PoPoWBootstrap == false)
- ADProofsRoot: if(status==regular && ADState==false && BlocksToKeep>0) 
- stateRoot: if(status==regular && ADState==false && BlocksToKeep>0) 

