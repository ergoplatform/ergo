Sub-Blocks and Improved Confirmed Transactions Propagation
==========================

* Author: kushti
* Status: Proposed
* Created: 31-Oct-2023
* License: CC0
* Forking: Soft Fork

Motivation
----------

Currently, a block is generated every two minutes on average, and confirmed transactions are propagated along with 
other block sections. 

This is not efficient at all. Most of new block's transactions are already available in a node's mempool, and 
bottlenecking network bandwidth after two minutes of delay is also downgrading network performance.

Also, while average block delay in Ergo is 2 minutes, variance is high, and often a user may wait 10 minutes for 
first confirmation.   

Sub-Blocks
----------

A valid block is sequence of (semantically valid) header fields (and corresponding valid block sections, such as block 
transactions), including special field to iterate over called nonce, such as *H(b) < T*, where *H()* is Autolykos Proof-of-Work
function, *b* are block bytes (including nonce), and *T* is a Proof-of-Work *target* value. A value which is reverse 
to target is called difficulty *D*: *D = 2^256 / T* (in fact, slightly less value than 2^256 is taken, namely, order of 
secp256k1 curve group, heritage of initial Autolykos 1 Proof-of-Work algorithm). *D* (and so *T*) is being readjusted 
regularly via a deterministic procedure (called difficulty readjustment algorithm) to have blocks coming every two minutes on average. 

Aside of blocks, *superblocks" are also used in the Ergo protocol, for building NiPoPoWs on top of them. A superblock is
a block which is more difficult to find than an ordinary, for example, for a (level-1) superblock *S* we may require 
*H(b) < T/2*, and in general, we can call n-level superblock a block *S* for which *H(b) < T/2^n*. Please note that a
superblock is also a valid block (every superblock is passing block PoW test).

Similarly, we can go in opposite direction and use *subblocks*
