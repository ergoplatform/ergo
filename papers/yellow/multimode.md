Multiple Modes
--------------

Ergo (since the very first testing network Testnet0) will support 
multiple security models. In addition to fullnode mode, which is
similar to Bitcoin fullnode, Ergo reference implementation will 
support:
   - Light-SPV
   - Light-Fullnode
   - Pruned-Fullnode
  
For each mode, there is a corresponding value of a "mode" setting.
  
  
Fullnode
=========

("mode"="full" to enable this mode)

Like in Bitcoin, a full node is storing all the full blocks since 
genesis block. Full node checks proofs of work, linking structure 
correctness (parent block id, interlink elements), and all the 
transactions in all the blocks. A fullnode is storing all the full 
blocks forever. It is also holding full UTXO set to be able to validate an 
arbitrary transaction. 

The only optimization a fullnode is doing is that is is skipping downloading and checking 
AD-transformation block part (see below in the "Light-Fullnode" section).
   
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
   
   