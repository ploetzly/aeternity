%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

-define(OFFSET, 101).
-define(FOLLOW_PC_TOP, follow_parent_chain_top).
-define(FOLLOW_CHILD_TOP, sync_child_chain).

%%%===================================================================
%%% Test cases
%%%===================================================================

follow_parent_chain_strategy_test_() ->
    {foreach,
     fun() ->
            meck:new(aec_chain, []),
            meck:expect(aec_chain, top_height, fun() -> 0 end),
            mock_parent_connector()
     end,
     fun(_) ->
            meck:unload(aec_chain),
            unmock_parent_connector()
     end,
     [ {"Cache the first 100 blocks", fun cache_first_100/0},
       {"Cache blocks while deleting older ones", fun gc_older_blocks/0},
       {"Fill gaps in the parent chain", fun fill_gaps/0},
       {"Fill gaps triggers GC", fun fill_gaps_triggers_gc/0}
     ]}.

%%%===================================================================
%%% Test cases
%%%===================================================================

cache_first_100() ->
    Test =
        fun(StartHeight, CacheMaxSize) ->
            {ok, _Pid} = start_cache(StartHeight, CacheMaxSize, ?FOLLOW_PC_TOP),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := 0,
                    top_offset         := ?OFFSET,
                    max_size           := CacheMaxSize,
                    blocks             := EmptyBlocks, 
                    top_height         := 0}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(EmptyBlocks), 0),
            lists:foreach(
                fun(Height) ->
                    Height0 = max(0, Height - 1),
                    Block = block_by_height(Height),
                    {ok, #{ child_start_height := StartHeight,
                            child_top_height   := 0,
                            top_offset         := ?OFFSET,
                            max_size           := CacheMaxSize,
                            blocks             := Blocks0,
                            top_height         := Height0}} = ?TEST_MODULE:get_state(),
                    %% blocks start from height 0, so the total count is
                    %% the top height + 1; since Height is the next top:
                    ?assertEqual(map_size(Blocks0), Height),
                    {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(Height),
                    ok = ?TEST_MODULE:post_block(Block),
                    {ok, #{ child_start_height := StartHeight,
                            child_top_height   := 0,
                            top_offset         := ?OFFSET,
                            max_size           := CacheMaxSize,
                            blocks             := Blocks,
                            top_height         := Height}} = ?TEST_MODULE:get_state(),
                    Block = maps:get(Height, Blocks),
                    ?assertEqual(map_size(Blocks), Height + 1),
                    {ok, Block} = ?TEST_MODULE:get_block_by_height(Height)
                end,
                lists:seq(0, 100)),
            ?TEST_MODULE:stop()
        end,
    %% test with cache sizes greater than 100, we will test GC in a different test
    Test(?OFFSET + 0, 1000),
    Test(?OFFSET + 1337, 1234), %% initially the start height plays no role at which blocks we cache
    ok.

gc_older_blocks() ->
    CacheMaxSize = 20,
    {ok, _Pid} = start_cache(0, CacheMaxSize, ?FOLLOW_PC_TOP),
    %% post CacheMaxSize blocks - the state is growing
    lists:foreach(
        fun(Height) ->
            {ok, #{blocks := Blocks0}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(Blocks0), Height),
            Block = block_by_height(Height),
            ok = ?TEST_MODULE:post_block(Block),
            {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(Blocks), Height + 1)
        end,
        lists:seq(0, CacheMaxSize - 1)),
    %% ensure all blocks are still present
    lists:foreach(
        fun(Height) ->
            Block = block_by_height(Height),
            ?assertMatch({ok, Block}, ?TEST_MODULE:get_block_by_height(Height))
        end,
        lists:seq(0, CacheMaxSize - 1)),
    %% push blocks one by one, while checking one is GCed each time
    lists:foreach(
        fun(Height) ->
            DeleteHeight = Height - CacheMaxSize,
            {ok, _DBlock} = ?TEST_MODULE:get_block_by_height(DeleteHeight),
            Block = block_by_height(Height),
            ok = ?TEST_MODULE:post_block(Block),
            {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(DeleteHeight),
            {ok, Block} = ?TEST_MODULE:get_block_by_height(Height),
            {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(Blocks), CacheMaxSize)
        end,
        lists:seq(CacheMaxSize, CacheMaxSize + 100)),
    ?TEST_MODULE:stop(),
    ok.

fill_gaps() ->
    CacheMaxSize = 2000, %% we will test GC when catching up gaps in a different test
    {ok, _Pid} = start_cache(0, CacheMaxSize, ?FOLLOW_PC_TOP),
    %% the state is empty
    {ok, #{blocks := EmptyBlocks}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(EmptyBlocks), 0),
    %% while the state is empty, post a block with a height 1 - since genesis
    %% (height = 0) is missing, it must fetch it to fill the gap
    ok = ?TEST_MODULE:post_block(block_by_height(1)),
    %% give the cache some time to fetch it
    timer:sleep(10),
    {ok, _} = ?TEST_MODULE:get_block_by_height(0),
    {ok, _} = ?TEST_MODULE:get_block_by_height(1),
    %% skip a few blocks, the cache catches up still
    DistantBlockHeight = 10,
    ok = ?TEST_MODULE:post_block(block_by_height(DistantBlockHeight)),
    %% give the cache some time to fetch it
    timer:sleep(10),
    lists:foreach(
        fun(Height) ->
            Block = block_by_height(Height),
            {ok, Block} = ?TEST_MODULE:get_block_by_height(Height)
        end,
        lists:seq(0, DistantBlockHeight)),
    %% no GC triggered
    {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(Blocks), DistantBlockHeight + 1),
    ?TEST_MODULE:stop(),
    ok.

fill_gaps_triggers_gc() ->
    CacheMaxSize = 10, %% we will test GC when catching up gaps in a different test
    {ok, _Pid} = start_cache(0, CacheMaxSize, ?FOLLOW_PC_TOP),
    %% the state is empty
    {ok, #{blocks := EmptyBlocks}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(EmptyBlocks), 0),
    %% skip a few blocks, the cache catches up still
    TestWithDistantBlock =
        fun(DistantBlockHeight) ->
            ?assertEqual(true, DistantBlockHeight > CacheMaxSize),
            ok = ?TEST_MODULE:post_block(block_by_height(DistantBlockHeight)),
            %% give the cache some time to fetch it
            timer:sleep(100),
            lists:foreach(
                fun(Height) ->
                    {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(Height)
                end,
                lists:seq(0, DistantBlockHeight - CacheMaxSize)),
            lists:foreach(
                fun(Height) ->
                    Block = block_by_height(Height),
                    {ok, Block} = ?TEST_MODULE:get_block_by_height(Height)
                end,
                lists:seq(DistantBlockHeight - CacheMaxSize + 1, DistantBlockHeight)),
            {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(Blocks), CacheMaxSize)
        end,
    TestWithDistantBlock(12),
    TestWithDistantBlock(13),
    TestWithDistantBlock(15),
    TestWithDistantBlock(20),
    %% test a huge gap
    TestWithDistantBlock(40),
    ?TEST_MODULE:stop(),
    ok.


%%%===================================================================
%%% Helper functions 
%%%===================================================================

start_cache(StartHeight, MaxSize) ->
    Args = [StartHeight, MaxSize],
    gen_server:start({local, ?TEST_MODULE}, ?TEST_MODULE, Args, []).

start_cache(StartHeight, MaxSize, Strategy) ->
    Args = [StartHeight, MaxSize, Strategy],
    gen_server:start({local, ?TEST_MODULE}, ?TEST_MODULE, Args, []).

height_to_hash(Height) when Height < 0 -> height_to_hash(0); 
height_to_hash(Height) when is_integer(Height) -> <<Height:32/unit:8>>.

hash_to_height(Hash) ->
    MeaningfulBytes = [B || B <- binary_to_list(Hash), B =/= 0],
    {Height, _} =
        lists:foldr( %% NB: we go right to left!
            fun(B, {AccumHeight, ByteIdx}) ->
                {B * trunc(math:pow(8, ByteIdx)) + AccumHeight, ByteIdx + 1}
            end,
            {0, 0},
            MeaningfulBytes),
    Height.

block_by_height(Height) ->
    Hash = height_to_hash(Height),
    PrevHash = height_to_hash(Height - 1),
    aec_parent_chain_block:new(Hash, Height, PrevHash).

block_by_hash(Hash) ->
    Height = hash_to_height(Hash),
    block_by_height(Height).

mock_parent_connector() ->
    meck:new(aec_parent_connector, []),
    meck:expect(aec_parent_connector, fetch_block_by_hash,
                fun(Hash) ->
                    spawn(
                        fun() ->
                            Block = block_by_hash(Hash),
                            ?TEST_MODULE:post_block(Block)
                        end)
                    end),
    meck:expect(aec_parent_connector, fetch_block_by_height_blocking,
                fun(Height) ->
                    Block = block_by_height(Height),
                    {ok, Block}
                end).

unmock_parent_connector() ->
    meck:unload(aec_parent_connector).
