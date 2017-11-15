# Ergo 

This repository contains the reference implementation of the 
Ergo Platform protocol, which is an alternative to 
the [Bitcoin protocol](https://bitcoin.org/bitcoin.pdf).

Ergo Platform website: [https://ergoplatform.org/](https://ergoplatform.org/)

## Differences from Bitcoin

* Memory-hard Proof-of-Work function [Equihash](https://www.cryptolux.org/index.php/Equihash) 
* New modes of operation: [light-fullnode](https://eprint.iacr.org/2016/994), 
[light-SPV](http://fc16.ifca.ai/bitcoin/papers/KLS16.pdf), hybrid modes
* [Alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter), which is more powerful that Bitcoin Script but also safe against 
heavy validation attacks
* Alternative fee model with [mandatory storage-rent component](https://eprint.iacr.org/2017/644.pdf)   
  
## Specifications

It will be a White Paper with a brief description, and also a Yellow Paper with detailed specification.
At the moment, there are only [Yellow Paper drafts](https://github.com/ergoplatform/ergo/tree/master/papers/yellow), and 
the reference implementation code should be considered as a specification.

## Installation

### Docker Quick Start

    sudo docker run --rm -p 9001:9001 -p 9051:9051 -v ergo:/tmp/ergo/node1/data andyceo/ergo
