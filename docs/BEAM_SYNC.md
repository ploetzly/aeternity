### Goals
 - Have node operational in minutes not days
 - Save disk space - have sparse node that never fully syncs

### Possible approaches

 - Original ethereum style beam sync. Sparse sync up to near the top, then perform normal operations from that point. Missing state tree elements are fetched as needed.
 - Fancy ethereum style beam sync. Add block witness to allow ahead of time batch fetching of required state tree items at a height.
 - Sync empty keyblocks until near the top then explicitly sync the entire reachable parts of state trees at that height. Then carry on applying new transactions on top. The node would forever not have the missing microblocks
 - 


### Requirements on node

### At startup and syncing

1. Read config at startup of sync - sync > mode => sequential | beam. Store this choice permanently
2. Sync spine first. Insert keyblocks without microblocks and without running transactions. Mark blocks as incomplete
3. Full sync from Top height - 101, running all transactions fetching missing state as needed
    - Nodes need API to provide state on demand ( must be from non gc'd node?)
4. Normal operation
    - Option to only ever keep last 100 blocks locally or to then carry on syncing from origin as normal

### Gossip protocol changes

Nodes need access to the POI in order to process blocks that should be run against incomplete state trees.

So once spine sync is complete and we switch to processing Tx and gossiped blocks we need the poi with the associated blocks

So, either poi is sent with all gossiped blocks, or we provide a way to request poi, or nodes know that a node is beam synced and sends poin including blocks to those nodes only

We also need a way to request a the state tree for a list of keys

## Potential Optimisations
During beam sync fetch block witness (list of affected state tree hashes) with each keyblock so, the tree can be fetched in parallel and/or in bulk.

## Changes to sync logic

During first pass insert keyblocks without microblocks and mark as incomplete in the #key_header.extra map
 - that can just be like today dropping the microblocks - {fill_pool,..} and {get_generation,..}

When spine sync is finished needs to enter a new sync phase.. or maybe when sync gets close to top switch mode to include the microblocks and run the transactions in them.

Sync resuming after restart.. Start syncing from the last fully complete block?
- What happens if that is far below the latest top of the network?
- Should we beam sync until more recent blocks and then go back to origin and do a full sync?
- Or should we always aim for only keeping recent state tree entries

## State
Who needs to know that we are currently beam syncing?

- aec_peer_connection processes
    - Must respond to ping messages with a difficulty that is not available from the chain - {error,not_rooted} - Send 0 difficulty? 0 would prevent other node attempting to sync with us
    - 
- aec_conductor
    - At startup even if mining is disabled if a beneficiary is configured it will create key block candidates.
    - After adding each synced block aec_conductor will do preempt_on_new_top and create a new key block candidate. During beam sync this is simply not possible because the state trees are not available

So. Do not attempt to generate key blocks during beam sync

## Tx Pool

The transaction pool can't meaningfully operate in a node that doesn't have the 
state trees because it always wants to follow the top and start applying transactions (this can't be quite right..)

### Operations performed by the aec_tx_pool process:

At startup:
 - Open somedatabases
 - Update all existing entries in the mempool
 - Register with gproc ()
 - Subscribe to top_changed events
 - Record the current top_height of the chain in the state

During operation:



### Operations performed by the aec_tx_pool_sync process:

All operations in this process are driven by API calls from the aec_peer_connection process for a peer.
 - On receipt of a MSG_PING from another node - aec_peer_connection triggers a connect api call that sends MSG_TX_POOL_SYNC_INIT to the peer
 - On receipt of a MSG_TX_POOL_SYNC_INIT message from another node

To disable TX pool sync during chain sync it seems reasonable to not send our
MSG_TX_POOL_SYNC_INIT and to reply with {error, ??????} to requests
 Q.What error codes are allowed?
 Q. What will a node do if it receives an error in response? Blow up? Stop syncing completely?



During sync the TX pool can be safely disabled.

Two processes involved: tx_pool and tx_pool_sync

Either don't run the ae_tx_pool process and return {error, not_running} to all requests to it
OR
run the process, but track sync status to decide what to reply

### Run ae_tx_pool but track sync progress

At startup request sync progress from ae_sync. If not 'synced' don't send 'MSG_TX_POOL_SYNC_INIT' to any peers

In response to incoming MSG_TX_POOL_SYNC_INIT messages reply with some suitable error

During sync other nodes will still send us Gossip Tx and Microblocks which will fill the mempool. Could drop these?

## Transition from beam mode to fullmode

Configured distance from top to start full sync.

- 

Behaviour changes:
 - aec_peer_connection: Enable tx_pool_sync
 - aec_conductor: Still don't start mining (for now)
 - aec_sync: Start including microblocks
 - 


Call tree for applying txs

aec_conductor:handle_add_block(Block, Hash, Prev, #state{top_block_hash = TopBlockHash} = State, Origin) ->
    aec_chain_state:insert_block_conductor(Block, Origin) ->
    internal_insert_normal(Node, Block, Origin) ->
    internal_insert_transaction(Node, Block, Origin, InsertCtx)
    update_state_tree(Node, State2, Ctx)
    {Trees, _Fees, Events} =
        apply_node_transactions(Node, TreesIn, ForkInfo, State) ->
        
    OR
    {DifficultyOut, Events} = 
        apply_and_store_state_trees(Node, TreesIn, ForkInfo, State)
    
    apply_micro_block_transactions(Node, FeesIn, Trees1)
    aec_block_micro_candidate, apply_block_txs_strict, [Txs, Trees, Env] ->
        aec_trees:apply_txs_on_state_trees(Txs, Trees, Env);
        tx_process(Tx, Trees, Env, Opts)
        aetx:process(Tx, Trees, Env) ->
            CB:process(Tx, Trees1, Env) ->
                aeprimop:eval(Instructions, Trees, Env).
                eval_instructions([I|Left], S, Opts)
                eval_one({Op, Args}, S)
                cache_write_through(S1, Opts),

                // S1 here is #aeprimop_state{}

State tree usage
===

Create Tx =>
    Accounts:
    - Update Creation Account nonce
    - Insert contract id with amount
    Calls:
    - Insert with Key = <<Contract[PubKey|Id], CallId>>
    Contracts:
    - Insert <<ContractPubKey>> -> Serialized Contract
    - Insert <<ContractPubKey, 16>> -> <<0>> to establish seperate subtree
    - Insert <<ContractPubKey, 16, StoreSuffix>> for each store node
    - Insert crazy map stuff

Goal
===

The block witness must list every state tree entry that is required to process all the Tx in a block. This means:

- Every entry that is read from the on disk state trees that existed at the start of the block

It excludes:
 - newly created items
 - items created in one Tx and subsequently read by another Tx in the same block

With exactly the right amount of state at the start of a block we can run all the smart contracts in that block. The resulting state treees will not however have the same Root hash as if those contracts had been run on the complete tree. Newly created entries will potentially have different paths to the root.

This means we can't rely on the values we have calculated for the next block until we have fetched the state trees for the start of the next block. These trees might re-work the structure of the existing tree.

Alternative approaches
===

If the goal is to be able to use the chain as a safe wallet for a small number of accounts, or for calling to specific contracts we could run a node that only syncs the state tree for locally requested state.

