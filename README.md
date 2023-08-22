# Ergo

This repository contains the reference client (aka the node) implementation for 
[Ergo, a cryptocurrency protocol](https://ergoplatform.org/) aiming to be secure peer-to-peer environment programmable
scarce money. 

The reference client implementation is done in Scala. There are parts of the protocol done in other languages (such as [sigma-rust](https://github.com/ergoplatform/sigma-rust), Rust implementation of ErgoScript cryptocurrency scripting language), but this is the only full-fledged Ergo client at the moment.

## Differences from Bitcoin

Ergo is UTXO Proof-of-Work cryptocurrency like Bitcoin, but there are a lot of differences (and Ergo was designed 
and implemented from scratch).

* Powerful contracts in the multi-stage extended UTXO model (see [ErgoScript whitepaper](https://ergoplatform.org/docs/ErgoScript.pdf)) 
* Memory-hard Proof-of-Work function [Autolykos2](https://docs.ergoplatform.com/ErgoPow.pdf)
* Support for stateless clients (asymmetric, based on [https://eprint.iacr.org/2016/994](https://eprint.iacr.org/2016/994)),
[NiPoPoWs](https://eprint.iacr.org/2017/963.pdf), hybrid modes
* [Alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter), which is more powerful than Bitcoin Script but also safe against
heavy validation attacks
* Alternative fee model with [mandatory storage-rent component](https://fc18.ifca.ai/bitcoin/papers/bitcoin18-final18.pdf ) (aka demurrage)

## Specifications

* [white paper](https://ergoplatform.org/docs/whitepaper.pdf) - a brief description of the protocol
* [ErgoScript white paper](https://ergoplatform.org/docs/ErgoScript.pdf) - describes ErgoScript, a Cryptocurrency Scripting Language Supporting Noninteractive Zero-Knowledge Proofs used in Ergo

More papers can be found at [https://docs.ergoplatform.com/documents/](https://docs.ergoplatform.com/documents/).

## Security assumptions

This client relies on some assumptions in regards with its environment:

* execution environment is trusted. While seed is stored in encrypted files, and the client's 
  wallet tries to remove secret key from memory as soon as possible when it is not needed, the
  client has no protection from side-channel attacks, memory scans etc.
* clocks should be more or less synchronized. If timestamp of a block is more than 20 minutes
  in future, the block will be temporarily rejected. The client does not use NTP or other time
  syncing protocols.

## Building and Running Node and UI

See [documentation](https://docs.ergoplatform.com/node/install/). 

By default, the node is processing all the blocks since genesis. There are other options available, which could be especially useful on limited hardware.

* bootstrapping with UTXO set snapshot - similar to snap-sync in Ethereum. To enable: 
```

ergo {
  ...
  node.utxoBootstrap = true
  ...
}
```

* bootstrapping with UTXO set snapshot can be combined with NiPoPoW (non-interactive proofs-of-proof-of-work) for 
syncing headers-chain (in logarithmic time vs ordinary SPV sync for headers), to do that:
```

ergo{
  ...
  node.nipopow.nipopowBootstrap = true
  node.utxo.utxoBootstrap = true
  ...
}
```

* stateless mode allows to have full-node security without holding UTXO set even! Details are provided in ["Improving Authenticated Dynamic Dictionaries,
  with Applications to Cryptocurrencies" paper](https://eprint.iacr.org/2016/994.pdf). This mode allows to download and validate arbitrary-sized suffix of the blockchain. Config example:

```
ergo {
   ...
   node.stateType = "digest"
   node.blocksToKeep = 2160 # store and process last three days only
   node.nipopow.nipopowBootstrap = true   # compatible with NiPoPoWs 
   ...
}
```




*More info on different modes of node operation can be found at [https://docs.ergoplatform.com/node/modes/](https://docs.ergoplatform.com/node/modes/)*

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

