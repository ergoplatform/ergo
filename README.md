# Ergo

Welcome to the official repository for the [Ergo Platform](https://ergoplatform.org/). This repository contains the reference client, also known as the node, for Ergo. Ergo is a cryptocurrency protocol that has been designed to offer a secure environment for peer-to-peer transactions. It supports programmable scarce money (Ergo) and a wide range of financial tools.

The reference client is primarily written in Scala. While certain components of the protocol are implemented in other languages (for instance, [sigma-rust](https://github.com/ergoplatform/sigma-rust) is a Rust-based implementation of the ErgoScript cryptocurrency scripting language), the reference client provides the most complete and comprehensive implementation of the Ergo protocol.

## Key Features of Ergo

Ergo, while sharing some commonalities with Bitcoin as a UTXO Proof-of-Work cryptocurrency, stands out due to its unique design and features. It has been built from the ground up, introducing several innovative elements:

* **ErgoScript**: A powerful contract language in the multi-stage extended UTXO model. More details can be found in the [ErgoScript whitepaper](https://ergoplatform.org/docs/ErgoScript.pdf).
* **Autolykos2**: A memory-hard Proof-of-Work function, providing enhanced security. Learn more about it [here](https://docs.ergoplatform.com/ErgoPow.pdf).
* Support for Stateless Clients: Ergo supports asymmetric stateless clients, based on [this paper](https://eprint.iacr.org/2016/994), and includes features like NiPoPoWs and hybrid modes.
* **Advanced Transactional Language**: Ergo introduces an [alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter) that is more powerful than Bitcoin Script and is designed to be safe against heavy validation attacks.
* **Innovative Fee Model**: Ergo implements an alternative fee model with a [mandatory storage-rent component](https://fc18.ifca.ai/bitcoin/papers/bitcoin18-final18.pdf ) (also known as demurrage).

## Specifications

* [white paper](https://ergoplatform.org/docs/whitepaper.pdf) - a brief description of the protocol
* [ErgoScript white paper](https://ergoplatform.org/docs/ErgoScript.pdf) - describes ErgoScript, a Cryptocurrency Scripting Language Supporting Noninteractive Zero-Knowledge Proofs used in Ergo

More papers can be found at [docs.ergoplatform.com/documents](https://docs.ergoplatform.com/documents/).

## Security Assumptions

The Ergo client operates under certain assumptions about its environment:

* The execution environment is trusted. Although the seed is stored in an encrypted file, and the client's wallet attempts to purge the secret key from memory as soon as it is no longer needed, the client does not have defenses against side-channel attacks, memory scans, etc.
* Clocks are expected to be synchronized to a reasonable degree. If a block's timestamp is more than 20 minutes into the future, the block will be temporarily rejected. The client does not utilize NTP or other time synchronization protocols.

## Building and Running the Node and UI

For instructions on how to build and run the node and UI, refer to the [official documentation](https://docs.ergoplatform.com/node/install/). 

By default, the node processes all blocks from the genesis block. However, there are other options available that may be more suitable for hardware with limited resources.

* **Bootstrapping with a UTXO set snapshot:** This works similarly to Ethereum's snap-sync. The node first downloads a UTXO set snapshot from a secure point in the past, then downloads blocks following the UTXO set snapshot and applies them to the set. For more details and security proofs, refer to the ["Multi-mode cryptocurrency systems" paper](https://eprint.iacr.org/2018/129.pdf). To enable this feature add the following to your configuration file: 
```

ergo {
  ...
  node.utxoBootstrap = true
  ...
}
```

* The UTXO set snapshot bootstrapping can be further optimized by combining it with NiPoPoW (Non-Interactive Proofs of Proof-of-Work). This method allows for syncing the headers-chain in logarithmic time, as opposed to the linear time required by the standard SPV sync for headers. For more details, refer to the [NiPoPoW paper](https://eprint.iacr.org/2017/963.pdf).
```

ergo{
  ...
  node.nipopow.nipopowBootstrap = true
  node.utxo.utxoBootstrap = true
  ...
}
```

* The stateless mode provides full-node security without the need to hold the entire UTXO set. This is achieved through the methods detailed in the ["Improving Authenticated Dynamic Dictionaries, with Applications to Cryptocurrencies" paper](https://eprint.iacr.org/2016/994.pdf). In this mode, it's possible to download and validate an arbitrary-sized suffix of the blockchain. Here's an example of how to configure this mode:

```
ergo {
   ...
   node.stateType = "digest"
   node.blocksToKeep = 2160 # store and process last three days only
   node.nipopow.nipopowBootstrap = true   # compatible with NiPoPoWs 
   ...
}
```


For more detailed information on different modes of node operation, please visit [docs.ergoplatform.com/node/modes](https://docs.ergoplatform.com/node/modes).

## Testing Procedures

Ergo utilizes three types of tests: 

1) Unit and property tests: These can be run using the `sbt test` command.
2) Integration tests: These tests require Docker to be installed. Run them with the `sudo sbt it:test` command.
3) Bootstrapping tests: These tests are time-consuming as they verify that the node is syncing with the main network in various modes. Docker is also required for these tests. Run them with the `sudo sbt it2:test` command.

## Setting up the Project in an IDE

You can use either [IntelliJ IDEA](https://www.jetbrains.com/idea/) (Community or Ultimate edition) or [VSCode](https://code.visualstudio.com/) with the [Metals](https://scalameta.org/metals/) extension. 

Ensure that the project can be built with sbt before opening it in an IDE. You may need to resolve any dependency errors first.

To open the project in IntelliJ IDEA, select File / Open and navigate to the project folder. This will initiate the Project Import Wizard, which uses the SBT configuration (build.sbt file) to generate the project configuration files for IDEA. You can view the project configuration in the `File / Project Structure...` dialog. If the import is successful, you should be able to compile the project in the IDE. 

## Modules

This repository has modular structure, so only parts which are needed for an application could be used:

* [avldb](avldb/README.md) - implementation of authenticated AVL+ tree used in Ergo, with persistence
* [ergo-core](ergo-core/README.md) - functionality needed for an SPV client (P2P messages, block section stuctures, PoW, NiPoPoW)
* ergo-wallet - Java and Scala functionalities to sign and verify transactions

## Contributing to Ergo

Ergo is an open-source project and we welcome contributions from developers and testers! Join the discussion over [Ergo Discord](https://discord.gg/kj7s7nb) in #development channel, or Telegram: https://t.me/ErgoDevelopers. Please also check out our [Contributing documentation](https://docs.ergoplatform.com/contribute/).

## Frequently Asked Questions

For common queries, please refer to our [Frequently Asked Questions](FAQ.md) page.

