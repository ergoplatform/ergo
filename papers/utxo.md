Bootstrapping with UTXO set snapshot
====================================

Motivation
----------

With time, more and more resources are required to download, store and process the
whole blockchain with all the full blocks. In blockchain where UTXO set snapshot is
authenticated (such as Ergo or Ethereum), it is possible to avoid that, by downloading and applying 
only a historical UTXO set snapshot (accounts snapshot in Ethereum) and full-blocks after it.
It is shown (in [https://eprint.iacr.org/2018/129](https://eprint.iacr.org/2018/129)) that this
mode can be as secure as processing all the blocks (under the same assumptions, with overwhelming probability 
as a function of full-blocks suffix length).

This specification defines how bootstrapping with UTXO set snapshot is implemented in Ergo
protocol reference client. The specification may be useful in order to understand how implementation 
is done, and also to do alternative clients compatible with the reference client.

Implementation Details
----------------------

UTXO set is authenticated via AVL+ tree. Design principles for tree construction are provided in 
[https://eprint.iacr.org/2016/994.pdf](https://eprint.iacr.org/2016/994.pdf), the implementation of the 
tree is available in [the Scrypto framework](https://github.com/input-output-hk/scrypto).

Time is broken into epochs, 1 epoch = 52,224 blocks (~72.5 days).
Snapshot is taken after last block of an epoch, namely, after processing a block with 
height *h % 51200 == 52,224*.

Chunk format
------------



Manifest format
---------------

Networking Layer
----------------

Bootstrapping
-------------


Node Config
-----------

The node is using bootstrapping with UTXO set snapshot if *ergo.node.utxoBootstrap = true*.



Sync Info V3
------------
