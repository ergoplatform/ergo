Input-Blocks for Faster Transactions Propagation and Confirmation
=================================================================

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
bottlenecking network bandwidth after two minutes of (more or less) idle state is downgrading network performance (for 
more, see motivation in [1]).

Also, while average block delay in Ergo is 2 minutes, variance is high, and often a user may wait 10 minutes for 
first confirmation. Proposals to lower variance are introducing experimental and controversial changes in consensus protocol.
Changing block delay via hardfork would have a lot of harsh consequences (e.g. many contracts relying on current block 
delay would be broken), and security of consensus after reducing block delay under bounded processing capacity could be 
compromised [2]. Thus it makes sense to consider weaker notions of confirmation which still could be useful for 
a variety of applications. 

Input Blocks and Ordering Blocks
--------------------------------

Following ideas in PRISM [3], parallel Proof-of-Work [4], and Tailstorm [5], we introduce two kinds of blocks in the Ergo 
 via non-breaking consensus protocol update. 

For starters, lets revisit blocks in current Ergo protocol, which is classic Proof-of-Work protocol formalized in [6]. 
A valid block is a set of (semantically valid) header fields (and corresponding valid block sections, such as block 
transactions), including special field to iterate over, called nonce, such as *H(b) < T*, where *H()* is Autolykos Proof-of-Work
function, *b* are block header bytes (including nonce), and *T* is a Proof-of-Work *target* value. A value which is reverse 
to target is called difficulty *D*: *D = 2^256 / T* (in fact, slightly less value than 2^256 is being used, namely, order of 
secp256k1 curve group, this is inherited from initial Autolykos 1 Proof-of-Work algorithm). *D* (and so *T*) is being readjusted 
regularly via a deterministic procedure (called difficulty readjustment algorithm) to have blocks coming every two minutes on average. 

Aside of blocks, *superblocks" are also used in the Ergo protocol, for building NiPoPoWs on top of them. A superblock is
a block which is more difficult to find than an ordinary, for example, for a (level-1) superblock *S* we may require 
*H(S) < T/2*, and in general, we can call n-level superblock a block *S* for which *H(S) < T/2^n*. Please note that a
superblock is also a valid block (every superblock is passing block PoW test).

We propose to name full blocks in Ergo as *ordering blocks* from now, and use input-blocks (or sub-blocks) to carry most
of transactions. For starters, we set *t = T/64* (the divisor will be revisited later) and define input-block *ib* generation 
condition as *H(ib) < t*, then a miner can generate on average 63 input blocks plus an ordering block 
per orderring block generation period. Please note that, unlike superblocks, input blocks are not passing ordering-block PoW check, 
but an ordering block is passing input block check.

Thus we have now blockchain be like:

(ordering) block - input block - input block - input block - (ordering) block - input block - input block - (ordering) block

Next, we define how  transactions are spread among input-blocks, and what additional data structures are needed. 

Transactions Handling
---------------------

Transactions are broken into two classes, for first one result of transaction validation can't change from one input 
block to other , for the second, validation result can vary from one block candidate to another (this is true for transactions relying on block timestamp, 
miner pubkey and other fields changing from one block header candidate to another, a clear example here is ERG emission contract, which is relying on miner pubkey. 
See next section for more details).

Transactions of the first class (about 99% of all transactions normally) can be included in input blocks only. 
Transactions of the second class can be included in both kinds of blocks.

As a miner does not know in advance which kind of block (input/ordering) will be generated, he is preparing for both 
options by:

* setting Merkle tree root of the block header to transactions seen in all the input blocks since the last ordering 
block, plus all the second-class transactions miner has since the last ordering block.
     
* setting 3 new fields in extension field of a block:
   - setting a new field to a digest (Merkle tree root) of new first-class transactions since last input-block
   - setting a new field to a digest (Merkle tree root) first class transactions since ordering block
   - setting a new field to reference to a last seen input block
  
Miners are getting tx fees from first-class transactions and storage rent from input (sub) blocks, emission reward and tx fees 
from second-class transactions from (ordering) blocks. 
For tx fees to be collectable in input blocks, fee script should be changed to "true" just (todo: EIP).

Transaction Classes And Blocks Processing
-----------------------------------------

With overall picture provided in the previous section, we are going to define details of transactions and inputs- and
ordering-blocks here. 

First of all, lets define formally transactions classes. We define miner-affected transactions as transactions which 
validity can be affected by a miner and block candidate the miner is forming, as their input scripts are using 
following context fields:

```
def preHeader: PreHeader // timestamp, votes, minerPk can be changed from candidate to candidate

def minerPubKey: Coll[Byte]
```

An example of such a transaction is ERG emission transaction. As a miner does not know which kind of block 
(input/ordering) will be generated, he is including all the transactions into a block candidate. But then, if during 
validation it turns out that an input-block is subject to validation, then miner-affected transactions are to be skipped.

Input and Ordering Blocks Propagation
-------------------------------------

Here we consider how input and ordering blocks generated and their transactions are propagated over the p2p network, 
for different clients (stateful/stateless). 

When a miner is generating an input block, it is announcing it by spreading header along with id of a previous input 
block (parent). A peer, by receiving an announcement, is asking for input block data introspection message, which 
contains proof of parent and both transaction Merkle trees against extension digest in the header, along with 
first-class transaction 6-byte weak ids (similar to weak ids in Compact Blocks in Bitcoin). Receiver checks transaction
 ids and downloads only first-class transactions to check.

When a miner is generating an ordering block, it is announcing header similarly to input-block announcement. However, 
in this case 

TODO: stateless clients.

Incentivization
---------------

No incentives to generate and propagate sub-blocks are planned for the Ergo core protocols at the moment. At the same
time, incentives can be provided on the sub-block based merge-mined sidechains, or via application-specific agreements
(where applications may pay to miners for faster confirmations).


Sub-blocks Structure and Commitment to Sub-Blocks
-------------------------------------------------

Here we consider what kind of footprint sub-blocks would have in consensus-enforced data structures (i.e. on-chain). 
Proper balance here is critical and hard to achieve. Strict consensus-enforced commitments (when all the 
sub-blocks committed on-chain) require from all the miners to have all the sub-blocks in order to check them. But, 
at the same time, consensus-enforced commitments to properly ordered sub-blocks would allow for protocols and 
applications using sub-blocks data.

We have chosen weak commitments. That is, a miner may (and incentivized to) to commit to longest sub-blocks chain 
since previous full-block, but that there are no any requirements about that in Ergo consensus rules.

New extension key space starting with 0x03 will be used for sub-blocks related data, with one key used per this EIP:

0x03 0x00 - digest of a Merkle tree of longest sub-blocks chain starting with previous block (but not including it).

So first sub-block having full-block as a parent will have empty tree, next one will have only first, and next 
full-block will commit to all the sub-blocks since previous full-block. 

Note that sub-blocks (like blocks) are forming direct acyclic graph (DAG), but only longest sub-blocks chain is 
committed.

At the same time, no any new checks are planned for the Ergo protocol. Checks are possible for sidechains. 


Sub-Block Based Sidechains
--------------------------

As L1 incentivization for propagating and committing on-chain to sub-blocks are missed, we consider sub-block based 
merge-mined sidechains as possible option to incentivize miners to participate in the sub-blocks sub-protocol. They 
also can be used to enforce linearity (so that transactions added in a previous sub-block can't be reversed). 

A merged-mined sidechain is using sub-blocks as well as blocks to update its state which can be committed via main-chain 
transactions even. That is, in every sub-blocks side-chain state (sidechain UTXO set digest etc) can be written in a box 
with sidechain NFT, and then every sub-block the box may be updated. 

For rewarding miners submitting sub-blocks to Ergo network (sidechain block generators are listening to), a sidechain block
may be consist of main-chain sub-block and sidechain state along with membership proof. For enforcing linearity of transactions
, sidechain consensus may enforce rollback to a sub-block before transaction reversal on proof of reversal being published. 



Weak Confirmations
------------------

With linearity of transactions history in sub-blocks chain, sub-blocks may be used for getting faster confirmations 
with weaker security guarantees.


Security Considerations and Assumptions 
---------------------------------------


Protocol Update
---------------

And only mining nodes update would be needed, while older nodes can receive ordinary block transactions message after every ordering block.

And all the new rules will be made soft-forkable.



References
----------

1. Eyal, Ittay, et al. "{Bitcoin-NG}: A scalable blockchain protocol." 13th USENIX symposium on networked systems design and implementation (NSDI 16). 2016. 
   https://www.usenix.org/system/files/conference/nsdi16/nsdi16-paper-eyal.pdf
2. Kiffer, Lucianna, et al. "Nakamoto Consensus under Bounded Processing Capacity." Proceedings of the 2024 on ACM SIGSAC Conference on Computer and Communications Security. 2024.
   https://iacr.steepath.eu/2023/381-NakamotoConsensusunderBoundedProcessingCapacity.pdf
3. Bagaria, Vivek, et al. "Prism: Deconstructing the blockchain to approach physical limits." Proceedings of the 2019 ACM SIGSAC Conference on Computer and Communications Security. 2019.
   https://dl.acm.org/doi/pdf/10.1145/3319535.3363213
4. Garay, Juan, Aggelos Kiayias, and Yu Shen. "Proof-of-work-based consensus in expected-constant time." Annual International Conference on the Theory and Applications of Cryptographic Techniques. Cham: Springer Nature Switzerland, 2024.
   https://eprint.iacr.org/2023/1663.pdf
5. Keller, Patrik, et al. "Tailstorm: A secure and fair blockchain for cash transactions." arXiv preprint arXiv:2306.12206 (2023).
   https://arxiv.org/pdf/2306.12206
6. Garay, Juan, Aggelos Kiayias, and Nikos Leonardos. "The bitcoin backbone protocol: Analysis and applications." Journal of the ACM 71.4 (2024): 1-49.
   https://dl.acm.org/doi/pdf/10.1145/3653445 
