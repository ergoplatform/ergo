# Ergo

This repository contains the reference implementation of the
Ergo Platform protocol, which is an alternative to
the [Bitcoin protocol](https://bitcoin.org/bitcoin.pdf).

Ergo Platform website: [https://ergoplatform.org/](https://ergoplatform.org/)


## Differences from Bitcoin

* Memory-hard non-outsourceable Proof-of-Work function [Autolykos](https://github.com/ergoplatform/autoleakus)
* New modes of operation: [light-fullnode](https://eprint.iacr.org/2016/994),
[light-SPV](http://fc16.ifca.ai/bitcoin/papers/KLS16.pdf), hybrid modes
* [Alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter), which is more powerful that Bitcoin Script but also safe against
heavy validation attacks
* Alternative fee model with [mandatory storage-rent component](https://eprint.iacr.org/2017/644.pdf)

## Specifications

It will be a White Paper with a brief description, and also a Yellow Paper with detailed specification.
At the moment, there is only [Yellow Paper drafts](https://github.com/ergoplatform/ergo/tree/master/papers/yellow/main.pdf),
and currently the reference implementation code should be considered as a specification.


## Installation

You can check our [Wiki](https://github.com/ergoplatform/ergo/wiki) page for different installation methods:

- [How to install Ergo node on Mac OS X](https://github.com/ergoplatform/ergo/wiki/How-to-install-Ergo-node-on-Mac-OS-X)
- [How to install Ergo node on Ubuntu](https://github.com/ergoplatform/ergo/wiki/How-to-install-Ergo-node-on-Ubuntu)
- [How to install Ergo node on Windows](https://github.com/ergoplatform/ergo/wiki/How-to-install-Ergo-node-on-Windows)

Also, reference with [Node Configuration File](https://github.com/ergoplatform/ergo/wiki/Node-Configuration-File) wiki page for creating your own configuration file.


## Docker Quick Start

Ergo has officially supported Docker package. To run Ergo as a console application with logs in console:

    sudo docker run --rm -p 9020:9020 -p 9052:9052 -v ergo-testnet:/home/ergo/.ergo ergoplatform/ergo

This will connect to Ergo testnet with default config and open ports `9020` and `9052` on host system. All data will be stored in your named Docker volume `ergo-testnet`.

To run specific Ergo version as a service with custom config:

    sudo docker run -d \
        -p 9020:9020 \
        -p 9052:9052 \
        -v ergo:/home/ergo/.ergo \
        -v /path/on/host/system/to/myergo.conf:/etc/myergo.conf \
        ergoplatform/ergo:v2.0.3 /etc/myergo.conf

This will connect to Ergo mainnet or testnet respecting your configuration passed in `myergo.conf`. Every default config value would be overwritten with corresponding value in `myergo.conf`. This also would store your data in named Docker volume `ergo` (if you change default data location in your config file, do not forget mount `ergo` volume to new location in container) and open ports `9007` and `9052` on host system. Note that `9052` is used for API, so it is ok not to open it into whole Internet. Also, Ergo node works normally under NAT, so you can keep closed your `9007` port too, hence other nodes could not discover and connect to yours one, only your node could initiate connections.
