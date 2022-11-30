# Ergo

This repository contains the reference implementation of the
Ergo Platform protocol, which is an alternative to
the [Bitcoin protocol](https://bitcoin.org/bitcoin.pdf).

Ergo Platform website: [https://ergoplatform.org/](https://ergoplatform.org/)

## Differences from Bitcoin

* Powerful contracts in the multi-stage extended UTXO model (see [ErgoScript whitepaper](https://ergoplatform.org/docs/ErgoScript.pdf)) 
* Memory-hard Proof-of-Work function [Autolykos2](https://docs.ergoplatform.com/ErgoPow.pdf)
* Support for stateless clients (asymmetric, based on [https://eprint.iacr.org/2016/994](https://eprint.iacr.org/2016/994)),
[NiPoPoWs](https://eprint.iacr.org/2017/963.pdf), hybrid modes
* [Alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter), which is more powerful than Bitcoin Script but also safe against
heavy validation attacks
* Alternative fee model with [mandatory storage-rent component](https://fc18.ifca.ai/bitcoin/papers/bitcoin18-final18.pdf )

## Specifications

A [White Paper](https://ergoplatform.org/docs/whitepaper.pdf) with a brief description is available. A Yellow Paper with detailed specification is underway and will be available shortly. At the moment, there are [drafts of the Yellow Paper](https://github.com/ergoplatform/ergo/tree/master/papers/yellow) available,
and currently the reference implementation code should be considered as the specification.

## Building and Running Node and UI

See [documentation](https://docs.ergoplatform.com/node/install/)

## Testing

There are three kinds of tests: 

1) Unit and property tests, run them with `sbt test` command.
2) Integration tests, they require for Docker to be installed, then run `sudo sbt it:test`.
3) Bootstrapping tests, very slow as they are checking that the node is indeed catching up with the main network in 
different regimes, they require for Docker too, run as `sudo sbt it2:test`.

## Open project in IDE

Your can use [IntelliJ IDEA](https://www.jetbrains.com/idea/) (Community or Ultimate edition) or 
[VSCode](https://code.visualstudio.com/) + [Metals](https://scalameta.org/metals/).
Before opening the project in IDE make sure it can be built with sbt. 
You may need to fix dependency resolution errors first.

After that you can open the project folder in Idea (File / Open)
which will run Project Import Wizard. The wizard will use SBT configuration
(build.sbt file) to generate Idea's project configuration files.
You can open `File / Project Structure...` dialog to see project configuration.
If everything is successful you can compile the project in IDE. 

## Contributions

Ergo is open-source and open movement, always in need for testers and developers! Please feel free
to discuss development in [Ergo Discord](https://discord.gg/kj7s7nb), #development channel. 

## FAQ
[Frequently Asked Questions](FAQ.md)

