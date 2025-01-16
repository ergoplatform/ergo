Ok, so after re-checking Prism and checking some new papers (such as new parallel PoW paper https://iacr.org/submit/files/slides/2024/eurocrypt/eurocrypt2024/482/slides.pdf ), I think, it makes sense to split blocks into input blocks and ordering blocks with some new block validation rules introduced via SF, however, with rich context available during script execution, there are some complexities which are not covered in the papers and we have to bypass:

assume number of sub-blocks (input blocks) per (ordering) block is equal to 128 (but it can be adjustable via miners voting):

* an ordering block is defined as block in Ergo now, hash(block) < Target
* input block is defined as sub-block , Target <= hash(block_header) < Target * 128, actually, 2-for-1 PoW option (so reverse(hash(block_header)) < Target * 128)
  from GKL15 / parallel PoW papers is likely better but need to check what is needed from pools to support that

thus we have blockchain like

(ordering) block - input block - input block - input block - (ordering) block - input block - input block - (ordering) block

* transactions are broken into two classes, for first one result of transaction validation can't change from one input block to other , for the second, validation result can vary (this is true for transactions relying on block timestamp, miner pubkey).
* only transactions of the first class (about 99% of all transactions normally) can be included in input (sub) blocks only. Transactions of the second class can be included in both kinds of blocks.
* as a miner does not know in advance, he is preparing for both options by:
    - setting Merkle tree root of the block header to transactions seen in the last input block and before that (since the last ordering block) plus new second-class transactions
      setting 3 new fields in extension field of a block:
    - setting a new field to new transactions included
    - setting a new field to removed second-class transactions (first-class cant be removed)
    - setting a new field to reference to a last seen input block (or Merkle tree of input blocks seen since last ordering block maybe)
* miners are getting tx fees and storage rent from input (sub) blocks, constant reward from (ordering) blocks. For tx fees  to be collectable in input blocks, fee script should be changed to "true" just (I have early draft of such EIP for long time, this script would be good to make transactions more lightweight as well)


This should provide fast and quite reliable confirmations for most of transactions.

And only mining nodes update would be needed, while older nodes can receive ordinary block transactions message after every ordering block.

And all the new rules will be made soft-forkable.