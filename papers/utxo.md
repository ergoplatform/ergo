Bootstrapping with UTXO set snapshot
====================================

Motivation 
----------

With time it consumes more and more resources to download, store and process the 
whole blockchain with all the fullblocks. In blockchain where UTXO set snapshot is 
authenticated (e.g. Ergo or Ethereum), it is possible to avoid that, downloading only some 
historical UTXO set snapshot (accounts snapshot in Ethereum) and full-blocks after it. 
It is shown (in [https://eprint.iacr.org/2018/129](https://eprint.iacr.org/2018/129)) that this
mode can be as secure as processing all the blocks (under the same assumptions).

This specification defines how to implement bootstrapping with UTXO set snapshot in Ergo 
protocol reference client. 

Implementation Details 
----------------------

UTXO set is authenticated via AVL+ tree.

Time is broken into epochs, 1 epoch = 51,840 blocks (~72 days).

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

Sync Info V3
------------



Testing
-------

1. No nodes with UTXO set snapshots around - stuck with headers synced - tested