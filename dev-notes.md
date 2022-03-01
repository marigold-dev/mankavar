# Introduction

The goal is to have the perfect blockchain. It should scale with the number of nodes, and reach all the theoretical optima.

The current design relies on 5 (!!!) different elements:
- Tendermint for the base consensus (in a classical L1 vs L2 form)
- Optimistic Rollups (ORUs) for externalized execution
- Parallel VM for parallelized execution
- Data Availability Sharding (DAS) for distributed storage
- Plasma for distributed&incentivized verification

Fundamentally, the system is imagined like this:
- Tendermint + DAS to get consensus on an operation set that grows with the size of the network
- A single ORU to offload the execution of those operations
- Parallel VM so that operations touching different parts of the memory can be run in parallel
- Plasma so that operations touching differents parts of the memory can be verified in parallel

The main pros are:
- You are allowed to be 100% optimistic: everything that happens, happens in the ORU. So the finality of the ORU does not matter, nothing goes back to the main chain or whatever.
- To get more TPS, add more nodes for the data, and more compute for the execution
- The system is resilient to bad data being inserted: a single bad commit does not break everything. Only the parts of the memory that no one cared to check.

In this system, the main bottlenecks are:
- Synchronization. You need to wait for the Executor to have all the ops before it can start executing them. You need to wait for all the operations before ordering them.
- Centralization. The system incentivizes having one big Executor. Even if the system is resilient to the Executor trying to pass in bad data, it is not resilient to the Executor getting DOS'd. There will necessarily be a kerkuffle at this point. It would be better if execution was spread among multiple Executors, or if there was an incentive for other people to maintain an Executor (inflation funding to solve crypto challenges that requires having all the online state? norms of changing the executor every month?).
- Accidents. Because there is a lot of stake on the Executor, it missing but a single bit in the entire execution can be extremely costly.

# Methodology

The whole thing is complicated.
Without clear separation of concerns, it will be impossible to test and benchmark things.
As such, it is very important to make the codebase as modular and abstract as possible.

As a direct consequence: Everything is Broken.
There are **tests**, here and there, that are mostly meant as scaffolding to build upon. Expect nothing beyond the tests to work.

# Theory

Let's explain various pieces, in theory, completely disconnected from their implementation here.

## Stuff with already existing good explanations

### Tendermint

Tendermint is a PBFT-like consensus algorithm:
- It requires a 2/3rd honest majority.
- If a 1/3rd of the nodes are dead, it stops.
- It has 2 block finality, which each block taking 3 (+1) rounds, each taking a little more than the bottom quartile of network ping.
- Its finality can be made adaptative, so it depends on the ping of the network.
- If the changes in validators are witnessable by the block headers, it is a mostly objective consensus. You can witness the consensus without being part of it, and have good light clients and bridges.

More on [its whitepaper](https://tendermint.com/static/docs/tendermint.pdf)

### Rollups (RU)

RUs are a scalability solution for blockchains:
- All the operations are available (usually, by having them posted on the main chain)
- Executors execute those operations. And then post commitments to the results of those operations on the mainchain.
- Validators check that those commitments are correct. If one of the commitment is not correct, validators have a deadline by which they must have submitted a proof they the commitment was incorrect.
- Past the deadline, we consider the commitments final.

More on [EthHub](https://docs.ethhub.io/ethereum-roadmap/layer-2-scaling/optimistic_rollups/)

There are multiple ways to replay stuff:
- Literally replay the entire transition from H1 to H2 (TORU)
  - This is the most straigthforward one
  - This requires data availability for the data touched between H1 and H2, which can be huge
  - This can be very fast: replay is non-interactive
- Find the smallest increment that can be replayed (SCORU)
  - Very slow: replay is very interactive
  - Requires no data-availability (only between an increment, which is bounded)
- Don't replay (ZKRU)
  - Expensive
  - Extremely fast: no replay

### Data Availability Sharding (DAS)

DAS is an enhancer for ORUs. With ORUs, all blockchain nodes need to download all operations. With DAS, only enough nodes download each operation to ensure rendundancy.
It works with cryptomagic and has a relatively low space overhead. Unfortunately, it costs quite a bit of runtime, the more data needs to be sharded.

More on [a Hackmd from Vitalik](https://hackmd.io/@vbuterin/sharding_proposal)

## Plasma

Plasma is an approach that was popular a couple of years ago. [It is now mostly dead, and was replaced by ORU instead.](https://cointelegraph.com/news/did-ethereum-silently-give-up-on-plasma)

The main idea behind Plasma was:
- Represents assets in a UTXO format instead of a ledger one
- Have people owning a UTXO keep track of their belonging
- If the Plasma operator misrepresents the ownership of your UTXO, withdraw it before his misrepresentation is finalized

The main problems behind Plasma were:
1. The mass exit problem: what if the operator misrepresents the ownership of **all** the UTXOs? Then everyone should withdraw them. The problem is that the network won't have the bandwidth to allow it, and some will necessarily loose their assets in the process.
2. The memory ownership problem: each piece of memory belongs to single person. When you only do transactions with UTXOs, it's ok, but this prevents more advanced smart-contracts from taking place. For instance, who should own the state of a Uniswap DEX?

If you think about it, ORUs solve both those problems: basically, everyone is responsible for all the space.

The goal here is thus to synthesize ORUs and Plasma. (Plosru? ORSMA?)

Strongly recommended readings here are about Plasma MVP, a simple version of Plasma. The first one is [from LearnPlasma](https://www.learnplasma.org/en/learn/mvp.html), and the second one from [EthResearch](https://ethresear.ch/t/minimal-viable-plasma/426).


# Current Architecture

## Virtual Machine
The VM is split into its own folder, and knows **nothing** about blockchains. As a VM, it is fairly simple:
- It is mostly a regular ASM, like a simplified x86
- It has an array of instructions for its program, 4 registers, 1 instruction pointer, and 1 stack
- The registers, the instruction pointer and the cells of the stack are all Int64.
- It has instructions to set registers, value on the stack, and special calls.

Even if the VM is independent, in practice, we still want it to interact with blockchains. To do so, the VM:
- Has an extra instruction called `custom`. When calling `eval`, you must pass in a function meant to deal with that case.
- Has an extra row in its state called `memory`. When calling `eval`, you must pass in an initial memory.

## Consensus vs Node vs Network

There is no first-class notion of Consensus in the code base! After thinking about it for a while, I realized Consensus was not actually relevant from a software standpoint. It is merely a property about the internal state of a bunch of nodes.

However, there is an abstract interface for Nodes. But nothing in this interface ensures that a node actually implements a consensus algorithm.
This interface is used by the Network Simulator.
Then, the goal will be to have it used by a Network Interface, so that Nodes can communicate with real-life networks.

# TODOs

- Consensus
  - X Dummy Consensus
  - Tendermint
    - X Basic
    - X Block validation
    - X Pipeline Block validation
    - Way more tests
    - Use Logs
    - Logs all bad Option.get
- Node
  - X Dummy Consensus Node
  - X Tendermint Node
  - X Dead Node
  - Other behaviors
    - Faulty Nodes
    - Single Deviation from regular node Node
    - Bad state hashes
    - Byzantine Nodes
  - More features
    - X Mempool
    - Gossip
- Network Simulator
  X Dummy Network
  - Gossip algorithm
  - Change topology of the network on the fly
- Smart Contracts Runtime Environment
  - Memory Blocks instead of cells
- VM
  - X Basic VM
  - X Parametrized with externals
  - X Basic higher level lang compiling to the VM
  - Make it parallel
  - Trace
  - Refutation
- ORU
  - Abstract Rollups
  - Extend Tendermint
    - X Dune Virtual Libraries
- Storage
  - X Everything in memory and simulated for now
- Network
  - Build a network interface