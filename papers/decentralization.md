# A New Quest for Decentralization, Part I: Technical Aspects

While most active conversations today in the space are about wider adoption of the blockchain technology (which often means
selling out to the Wall St.) and competition with systems like Visa and Mastercard (which often means giving up with 
decentralization or introducing unclear security assumptions in the name of efficiency), there is the obvious need to 
revisit the roots of the cryptocurrency movement, which are mostly about decentralization. Many questions to be answered in clear here. Is it 
okay when 90% of mining power in Bitcoin [can gather in one room](https://twitter.com/lopp/status/673398201307664384)? 
Is it okay when 2 or 3 mining pools control majority of hashing power, so can do censorship? Is it okay when almost all
the new nodes avoid processing a blockchain from its genesis block? Is it okay when a Proof-of-Work coin developers are
doing a hard-fork changing the consensus algorithm to make it GPU-friendly again? Can we summarize all the issues with
decentralization? Can we cover most of issues with technical means?

Decentralization is about many issues lying in many fields, of technical, social, and hybrid kinds. Researchers and 
developers are trying to find technical solutions, preferably, elegant and efficient. However, for many issues 
such solutions are not known, thus social solutions are also needed. 

In this article I will cover only technical aspects of decentralization, namely, decentralization of mining and 
decentralization of verification.

## Decentralization of Mining

The two biggest concerns about decentralization of mining are specialized hardware (such as ASICs) and centralized 
pools. 

With ASICs, a big player capable to invest enough money into R&D can get unfair advantage from privately 
owned efficient hardware. In principle, for any kind of computational activity it is always possible to develop 
specialized hardware performing better than commodity computing units, such and CPUs and GPUs. However, for different 
computational tasks R&D efforts and possible outcome could vary a lot. Reasoning behind a search for a perfect (or 
close enough to perfect) could be quite complex (see e.g. 30 pages long [Equihash paper](http://ledgerjournal.org/ojs/index.php/ledger/article/view/48)).

For most of Proof-of-Work cryptocurrencies (including Bitcoin, Ethereum, ZCash), 2 to 4 centralized mining pools  
control majority of mining power. This could mean easy censorship or frontrunning on applications (for example, 
reordering exchange orders), as in centralized pools only pool decides block candidate for the whole pool to work on.
As a possible outcome, non-outsourceable mining schemes can prevent centralized pools formation. Only Ergo Platform
is known for deploying a practical non-outsourceable Proof-of-Work scheme (based on a supposedly memory-harder 
problem from the Equihash paper) called Autolykos.   

As an example where social decentralization issues meet the decentralization of mining, sometimes developers of 
Proof-of-Work are introducing hard-forks to make a Proof-of-Work algorithm GPU-friendly again once ASICs are going to 
dominate in the mining market for the coin, however, it is always not quite clear why totally legit activity is banned
 and why developers (along with some users) can do hard-fork for this particular reason. 

## Decentralization of Verification

Decentralization of verification is about possibility to check validity of blockchain history. Such check provides
confidence that nothing bad (i.e not conforming to a protocol) was injected to the blockchain and thus give a user 
a right to reject malicious chain even if it has absorbed more work than alternatives. There were many talks about
such the right in the Bitcoin community when it was partly hot about User-Activated Soft Fork (UASF) idea, and 
recent article ["Who secures Bitcoin?"](https://medium.com/@BitcoinErrorLog/who-secures-bitcoin-95b19bbcda3c) is
summarizing this way of thinking well. 

If verification can be done in reasonable time only by an entity able to spend millions on renting a datacenter, 
obviously a network is not decentralized. Ideally, it should be possible to check integrity of the whole blockchain 
on commodity hardware, like a decent laptop.

However, new blockchains also tend to absorb more and more features, and they are not coming for free. Then the 
huge topic in the research community is about how to make possible to check integrity of the whole blockchain
with pruned blocks or system state (or both) under plausible assumptions. Possible solutions here are about bootstrapping 
state snapshot and blockchain suffix on top of it (popular in Ethereum protocol clients, and formalized in [an academic
paper even](https://eprint.iacr.org/2018/129.pdf)), stateless clients (partially stateless, as implemented in Ergo Platform
 or fully stateless which do exist only in research papers currently). 

