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
  X Dummy Consensus
  - Tendermint
    X Basic
    - Way more tests
    - Block validation
    - Pipeline Block balidation
    - Use Logs
    - Logs all bad Option.get
  - Something more alien, like Avalanche
- Node
  X Dummy Consensus Node
  X Tendermint Node
  X Dead Node
  - Other behaviors
    - Faulty Nodes
    - Single Deviation from regular node Node
    - Byzantine Nodes
  - More features
    - Mempool
    - Gossip
- Network Simulator
  X Dummy Network
  - Gossip algorithm
  - Change topology of the network on the fly
- VM
  X Basic VM
  X Parametrized with externals
  X Basic higher level lang compiling to the VM
  - Make it parallel
  - Trace
  - Refutation
- ORU
  - Abstract Rollups
  - Extend Tendermint
    - Duplicate Tendermint's code?
    - Functorize Tendermint's code?
    - Dune Virtual Libraries?
- Storage
  - Everything in memory and simulated for now
- Network
  - Build a network interface