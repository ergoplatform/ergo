# Ergo Node Metrics 

## Introduction
Ergo node performs validation of mined blocks which it receives from peers and the valid blocks
are used to update the state of the node.

The block validation procedure has many steps which take CPU time and should complete both within
limited time and limited cost budget. The cost of each block is computed by the node. It depends on
the number of transactions, the number of inputs and complexity of the input scripts, the number of
tokens involved and other specific content of the block. The cost estimation and control is used to 
ensure node health under all possible (including under planned attack) conditions.

The health of the network depends on the health of the majority of its nodes. And the health of each
Ergo node depend, in particular, on its ability to process blocks within the defined time limit. The
time limit is enforced by using block cost estimation (aka "costing") protocol, which rejects blocks
with cost exceeding the current MaxBlockCost limit of the network (this parameter can be changed by
miners).
 
Now, miners can increase MaxBlockCost and thus increase complexity of the blocks on the network,
i.e. the blocks may have more transactions or more complex contracts or both. This may lead to some
nodes become unhealthy, because they will not be able to process blocks in time and thus fall out of
sync with the network. Rational miners who care about network security need to understand
performance of the node before voting for MaxBlockCost increase.

Developers may do node optimizations and introduce new features via soft-forks. How they know the
new node release will be healthy on the network? They also need to understand performance of the
node before publishing a new release.

The following sections describe Metrics sub-system of Ergo node, which can be turned-on in the
configuration to collect runtime performance measurements and related cost estimations of blocks,
transactions, and individual verified input scripts.

## Block validation




