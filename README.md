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

Ergo has officially supported Docker package. To run Ergo as a console application with logs in console:

    sudo docker run --rm -p 9001:9001 -p 9051:9051 -v ergo-testnet:/root/ergo/data ergoplatform/ergo
    
This will connect to Ergo testnet with default config and open ports 9001 and 9051 on host system. All data will be stored in your named Docker volume `ergo-testnet`. 

To run Ergo as a service with custom config:

    sudo docker run -d -p 9001:9001 -p 9051:9051
		-v ergo:/root/ergo/data
		-v /path/on/host/system/to/myergo.conf:/root/ergo/myergo.conf
		ergoplatform/ergo /root/ergo/myergo.conf

This will connect to Ergo mainnet or testnet respecting your configuration passed in `myergo.conf`. Every default config value would be overwritten with corresponding value in `myergo.conf`. This also would store your data in named Docker volume `ergo` (if you change default data location in your config file, do not forget mount `ergo` volume to changed location) and open ports 9001 and 9051 on host system.
