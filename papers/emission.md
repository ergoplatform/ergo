Emission  Retargeting Soft-Fork Proposal
========================================

* Author: kushti
* Status: Proposed
* Created: 17-Dec-2021
* Last edited: 20-Dec-2020
* License: CC0
* Forking: Soft Fork


Motivation 
----------

Long-term security of the Ergo protocol, including crypto-economic security, is always of highest priority. 
 One of the hottest topic in this field is (in)stability of cryptocurrency protocols without stable block 
 rewards coming from emission (see e.g. [1] for details). 
 
It was planned during the launch of the Ergo network that after end of emission miners will be rewarded with 
transaction fees and also with storage rent. However, it is hard to estimate yet how successfully will storage rent replace
emission. 

Thus it was proposed on the ErgoForum informally (in [2] and [3]) to prolong emission (with preserving total supply) 
via a soft-fork([4]). This EIP is concluding previous discussions and provides details on emission soft-fork 
design and implementation.

Updated Emission Schedule
-------------------------

Starting from block #XXX,XXX (first block of 684th epoch), new emission rules applied on top of rules described in the 
Ergo Whitepaper.

Before end of the current emission (before block 2,080,800):

* if block reward is not less than 15 ERG, send 12 ERG from it to the reemission contract
* otherwise, block reward R is less than 15 ERG, send R - 3 ERG from it to the reemission contract

After end of the current emission (from block 2,080,800):

* pay 3 ERG each block from the re-emission contract,

where the re-emission contract is working like emission one, but does not have emission curve encoded, only flat payment.


Updated Emission Details
------------------------



General Design
--------------

Emission in Ergo is done via a contract, which is existing since before genesis block. Changing emission then in
an elegant way is tricky. 

This EIP is proposing to use existing contract in combination with checks done in the core protocol which are mandatory 
only for mining nodes. Non-updated nodes will successfully validate blocks which are valid for the new nodes (both 
checking and not checking new rules) after activation of the new rules. Thus this change is soft-fork. 

This EIP offers following procedure for that:

* inject two new tokens into emission contract. First token is a singleton one issued to mark emission box (could be used
for tracking emission box efficiently) . Second token is reemission token, it will go to mining reward boxes, and 
amount of reemission tokens in a mining rewards box shows how many ERG a miner should send to the re-emission contract 
when spending the box. Any miner can do the injection. The injection will happen on activation height.

* a new kind of contract, the re-emission Contract is paying 3 ERG each block after end of emission

* a new kind of contract, pay-to-reemission contract. Miners enforced to pay to this proxy contract. The contract is 
similar to existing fee contract, but with no time-lock. 

* new consensus-level checks are verifying that a proper amount of re-emission token is going from the emission box to 
a miner rewards box, and also that during spending a box which is not emission box but contains re-emission tokens, 
the tokens are being burnt and the same amount of nanoERG is locked by the reemission contract. 
The checks can be switched off via a soft-fork.

 

Contracts
-------------------

**Reemission contract**:

```scala
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 0.toByte)

    val correctNftId = EQ(firstTokenId, ByteArrayConstant(reemissionNftId))
    
    // output of the reemission contract
    val reemissionOut = ByIndex(Outputs, IntConstant(0))
    
    // output to pay miner
    val minerOut = ByIndex(Outputs, IntConstant(1))
    
    // miner's output must have script which is time-locking reward for miner's pubkey
    // box height must be the same as block height
    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(monetarySettings.minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )
    
    // reemission output's height must be the same as block height
    val heightCorrect = EQ(boxCreationHeight(reemissionOut), Height)
    
    // reemission output's height is greater than reemission input
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    
    // check that height is greater than end of emission (>= 2,080,800 for the mainnet)
    val afterEmission = GE(Height, IntConstant(emissionPeriod))
    
    // reemission contract must be preserved
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(reemissionOut))
    
    // miner's reward
    val coinsToIssue = monetarySettings.oneEpochReduction // 3 ERG
    val correctCoinsIssued = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(reemissionOut)))
    
    // when reemission contract box got merged with other boxes
    val sponsored = AND(
        GT(ExtractAmount(reemissionOut), ExtractAmount(Self)),
        LE(ExtractAmount(ByIndex(Outputs, IntConstant(1))), LongConstant(10000000)), // 0.01 ERG
        EQ(SizeOf(Outputs), 2)
    )
    
    AND(
        correctNftId,
        sameScriptRule,
        heightCorrect,
        OR(
          sponsored,
          AND(
            correctMinerOutput,
            afterEmission,
            heightIncreased,
            correctCoinsIssued
          )
        )
    )
```

**Pay-to-Reemission contract**:

```scala
    val reemissionOut = ByIndex(Outputs, IntConstant(0))

    val rOutTokens = OptionGet(ExtractRegisterAs(reemissionOut, R2)(SCollection(STuple(SCollection(SByte), SLong))))

    val firstTokenId = SelectField(ByIndex(rOutTokens, IntConstant(0)), 0.toByte)

    EQ(firstTokenId, ByteArrayConstant(reemissionNftId))
```

Voting for the Soft-Fork
------------------------

TODO: 

Activation Details
------------------

On emission height, emission NFT and reemission tokens to be injected into the emission contract by spending
a box we are calling injection box.

The injection box is protected by the script 

```
{
    INPUTS(0).value > 30000000L * 1000000000L &&
       SELF.id == INPUTS(1).id 
}
```

so spendable by anyone who can spend 30M ERG at least provided in the first input. 

API Methods Changed
-------------------

* /emission/at
* /wallet/balances
* /wallet/balances/withUnconfirmed

Testnet Data
------------ 

Emission contract NFT id: **00594148f3b4205ec8d33f9f664b1baae20252df3592c8dbff5e9bdc30c77c44**

Re-emission token id: **004b1528123ef62ce2bbb7036ad2dd553e6a64252f86746a706729fa253b24cd**

Reemission contract NFT id: **001b81ceed43f4328754e368fc6a34c367ab8e00d1272be33c565bf247ad5748**

Activation height: ****


Mainnet Data
------------

Emission contract NFT id: *not set yet*

Re-emission token id: *not set yet*

Reemission contract NFT id: *not set yet*


References
----------

1. Carlsten, Miles, et al. "On the instability of bitcoin without the block reward." Proceedings of the 2016 ACM SIGSAC Conference on Computer and Communications Security. 2016.
2. Ergo Emission: details, retargeting via a soft-fork. https://www.ergoforum.org/t/ergo-emission-details-retargeting-via-a-soft-fork/2778
3. Emission Soft-Fork Proposal. https://www.ergoforum.org/t/emission-soft-fork-proposal/2996/27
4. Zindros, Dionysis. "Soft Power: Upgrading Chain Macroeconomic Policy Through Soft Forks." International Conference on Financial Cryptography and Data Security. 2021.