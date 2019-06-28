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

You can check our [Wiki](https://github.com/ergoplatform/ergo/wiki/Set-up-a-full-node) page for node installation and configuration guide.

Also, reference with [Node Configuration File](https://github.com/ergoplatform/ergo/wiki/Node-Configuration-File) wiki page for creating your own configuration file.


## Build from sources

In order to build the Ergo node from sources you need JDK (>= 1.8) and SBT to be installed on your machine.

In order to simply get a single jar run: `sbt assembly` - assembly would appear in `target/scala-2.12/` directory.
 
If you want to create a package for a specific platform with launching scripts the one of the following 
packager commands could be chosen (depending on desired system type you want to build for):
 - `universal:packageBin` - Generates a universal zip file
 - `universal:packageZipTarball` - Generates a universal tgz file
 - `debian:packageBin` - Generates a deb
 - `docker:publishLocal` - Builds a Docker image using the local Docker server
 - `rpm:packageBin` - Generates an rpm
 - `universal:packageOsxDmg` - Generates a DMG file with the same contents as the universal zip/tgz.
 - `windows:packageBin` - Generates an MSI
 
 The final build command should look like: `sbt <packager_command>`, example: `sbt universal:packageBin`.
 A resulted package could be found in the `target/scala-2.12/<platform_type>` directory.

## Running the node

The node could be started in a few different ways:
 
 - In case you have only a jar: `java -jar /path/to/ergo-<version>.jar --<networkId> -c /path/to/local.conf`
 - Using start script from sbt-native-packager: `sh /path/to/bin/ergo  --<networkId> -c /path/to/local.conf`
 
Available `networkId` options: `mainnet`, `testnet`, `devnet`. 

## Docker Quick Start

Ergo has officially supported Docker package. To run last Ergo version in mainnet as a console application with logs printed to console:

    sudo docker run --rm -p 9030:9030 -p 9053:9053 -v /path/on/host/to/ergo/data:/home/ergo/.ergo ergoplatform/ergo --mainnet

This will connect to Ergo mainnet with default config and open ports `9030` and `9053` on host system. All data will be stored in your host directory `/path/on/host/to/ergo/data`.

To run specific Ergo version `<VERSION>` as a service with custom config `/path/on/host/system/to/myergo.conf`:

    sudo docker run -d \
        -p 9030:9030 \
        -p 9053:9053 \
        -v /path/on/host/to/ergo/data:/home/ergo/.ergo \
        -v /path/on/host/system/to/myergo.conf:/etc/myergo.conf \
        ergoplatform/ergo:<VERSION> --<networkId> -c /etc/myergo.conf

Available versions can be found on [Ergo Docker image page](https://hub.docker.com/r/ergoplatform/ergo/tags), for example, `v2.2.0`.

This will connect to Ergo mainnet or testnet respecting your configuration passed in `myergo.conf` and network flag `--<networkId>`. Every default config value would be overwritten with corresponding value in `myergo.conf`. This command also would store your data in `/path/on/host/to/ergo/data` on host system, and open ports `9030` (node communication) and `9053` (REST API) on host system.

Ergo node works normally behind NAT, so you can keep closed your `9030` port, hence other nodes could not discover and connect to yours one, only your node could initiate connections.

It is also a good practice to keep closed REST API port `9053`, and connect to your node from inside another container in the same Docker network (this case not covered by this short quick start manual).

## Acknowledgements

<img src="https://www.yourkit.com/images/yklogo.png" align="right" />

YourKit supports open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of <a href="https://www.yourkit.com/java/profiler/">YourKit Java Profiler</a>
and <a href="https://www.yourkit.com/.net/profiler/">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications.
