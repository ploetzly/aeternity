%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

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
            mock_parent_connector(),
            mock_events()
     end,
     fun(_) ->
            unmock_events(),
            meck:unload(aec_chain),
            unmock_parent_connector()
     end,
     [ {"Cache the first 100 blocks", fun cache_first_100/0},
       {"Cache blocks while deleting older ones", fun gc_older_blocks/0},
       {"Fill gaps in the parent chain", fun fill_gaps/0},
       {"Fill gaps triggers GC", fun fill_gaps_triggers_gc/0},
       {"Child top change does not impact cache", fun child_top_change_while_following_pc/0}
     ]}.

follow_child_chain_strategy_test_() ->
    {foreach,
     fun() ->
            meck:new(aec_chain, []),
            meck:expect(aec_chain, top_height, fun() -> 0 end),
            mock_parent_connector(),
            mock_events()
     end,
     fun(_) ->
            unmock_events(),
            meck:unload(aec_chain),
            unmock_parent_connector()
     end,
     [ {"Cache all the blocks above current child height", fun cache_all_above_child_height/0}

     ]}.

%%%===================================================================
%%% Test cases
%%%===================================================================

cache_first_100() ->
    Confirmations = 1,
    Test =
        fun(StartHeight, CacheMaxSize) ->
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize, ?FOLLOW_PC_TOP),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := 0,
                    pc_confirmations   := Confirmations,
                    max_size           := CacheMaxSize,
                    blocks             := EmptyBlocks, 
                    top_height         := 0}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(EmptyBlocks), 0),
            lists:foreach(
                fun(Height) ->
                    Height0 = max(0, Height - 1),
                    Block = block_by_height(Height),
                    {ok, #{ child_start_height := StartHeight,
                            child_top_height   := ChildTop,
                            pc_confirmations   := Confirmations,
                            max_size           := CacheMaxSize,
                            blocks             := Blocks0,
                            top_height         := Height0}} = ?TEST_MODULE:get_state(),
                    %% blocks start from height 0, so the total count is
                    %% the top height + 1; since Height is the next top:
                    ?assertEqual(map_size(Blocks0), Height),
                    {error, not_in_cache} = ?TEST_MODULE:get_block_by_height(Height),
                    ok = ?TEST_MODULE:post_block(Block),
                    {ok, #{ child_start_height := StartHeight,
                            child_top_height   := ChildTop,
                            pc_confirmations   := Confirmations,
                            max_size           := CacheMaxSize,
                            blocks             := Blocks,
                            top_height         := Height}} = ?TEST_MODULE:get_state(),
                    Block = maps:get(Height, Blocks),
                    ?assertEqual(map_size(Blocks), Height + 1),
                    {error, {not_enough_confirmations, Block}} = ?TEST_MODULE:get_block_by_height(Height),
                    case Height - Confirmations of
                        MaturedH when MaturedH > -1 ->
                            MatureBlock = block_by_height(MaturedH),
                            {ok, MatureBlock} = ?TEST_MODULE:get_block_by_height(MaturedH);
                        _ -> pass
                    end
                end,
                lists:seq(0, 100 - Confirmations)),
            ?TEST_MODULE:stop()
        end,
    %% test with cache sizes greater than 100, we will test GC in a different test
    Test(0, 1000),
    Test(1337, 1234), %% initially the start height plays no role at which blocks we cache
    ok.

gc_older_blocks() ->
    CacheMaxSize = 20,
    Confirmations = 1,
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
            {ok, #{top_height := ParentTop}} = ?TEST_MODULE:get_state(),
            IsMature = Height =< ParentTop - Confirmations,
            case ?TEST_MODULE:get_block_by_height(Height) of
                {ok, Block} when IsMature -> ok;
                {error, {not_enough_confirmations, Block}} when not IsMature -> ok
            end
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
            {error, {not_enough_confirmations, Block}} = ?TEST_MODULE:get_block_by_height(Height),
            {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
            ?assertEqual(map_size(Blocks), CacheMaxSize)
        end,
        lists:seq(CacheMaxSize, CacheMaxSize + 100)),
    ?TEST_MODULE:stop(),
    ok.

fill_gaps() ->
    Confirmations = 1,
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
    {error, {not_enough_confirmations, _}} = ?TEST_MODULE:get_block_by_height(1),
    %% skip a few blocks, the cache catches up still
    DistantBlockHeight = 10,
    ok = ?TEST_MODULE:post_block(block_by_height(DistantBlockHeight)),
    %% give the cache some time to fetch it
    timer:sleep(10),
    lists:foreach(
        fun(Height) ->
            Block = block_by_height(Height),
            {ok, #{top_height := ParentTop}} = ?TEST_MODULE:get_state(),
            IsMature = Height =< ParentTop - Confirmations,
            case ?TEST_MODULE:get_block_by_height(Height) of
                {ok, Block} when IsMature -> ok;
                {error, {not_enough_confirmations, Block}} when not IsMature -> ok
            end
        end,
        lists:seq(0, DistantBlockHeight)),
    %% no GC triggered
    {ok, #{blocks := Blocks}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(Blocks), DistantBlockHeight + 1),
    ?TEST_MODULE:stop(),
    ok.

fill_gaps_triggers_gc() ->
    CacheMaxSize = 10,
    Confirmations = 1,
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
                    {ok, #{top_height := ParentTop}} = ?TEST_MODULE:get_state(),
                    IsMature = Height =< ParentTop - Confirmations,
                    case ?TEST_MODULE:get_block_by_height(Height) of
                        {ok, Block} when IsMature -> ok;
                        {error, not_in_cache} -> -233 = Height;
                        {error, {not_enough_confirmations, Block}} when not IsMature -> ok
                    end
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

child_top_change_while_following_pc() ->
    CacheMaxSize = 100,
    {ok, CachePid} = start_cache(0, CacheMaxSize, ?FOLLOW_PC_TOP),
    %% the state is empty
    {ok, #{ child_start_height := StartHeight,
            child_top_height   := 0,
            blocks             := EmptyBlocks, 
            top_height         := 0}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(EmptyBlocks), 0),
    ChildTop1 = 10,
    child_new_top(CachePid, ChildTop1),
    %% the state is still empty, the child top had changed
    {ok, #{ child_start_height := StartHeight,
            child_top_height   := ChildTop1,
            blocks             := EmptyBlocks, 
            top_height         := 0}} = ?TEST_MODULE:get_state(),
    SomeHeight = 10,
    ok = ?TEST_MODULE:post_block(block_by_height(SomeHeight)),
    timer:sleep(10),
    {ok, #{ child_start_height := StartHeight,
            child_top_height   := ChildTop1,
            blocks             := NonEmptyBlocks, 
            top_height         := SomeHeight}} = ?TEST_MODULE:get_state(),
    ?assertEqual(map_size(NonEmptyBlocks), SomeHeight + 1),
    ?TEST_MODULE:stop(),
    ok.

cache_all_above_child_height() ->
    CacheMaxSize = 20,
    StartHeight = 200,
    {ok, CachePid} = start_cache(StartHeight, CacheMaxSize), %% by default - follow child top
    timer:sleep(10),
    %% the cache is full
    ExpectedTopHeight1 = StartHeight - 1,
    {ok, #{ child_start_height := StartHeight,
            child_top_height   := 0} = Res1} = ?TEST_MODULE:get_state(),
    test_follow_child_cache_consistency(Res1),
    ChildTop1 = 10,
    child_new_top(CachePid, ChildTop1),
    timer:sleep(10),
    {ok, #{ child_start_height := StartHeight,
            child_top_height   := ChildTop1,
            top_height         := CacheSize}} = ?TEST_MODULE:get_state(),
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
    meck:expect(aec_parent_connector, request_block_by_height,
                fun(Height) ->
                    spawn(
                        fun() ->
                            Block = block_by_height(Height),
                            ?TEST_MODULE:post_block(Block)
                        end)
                    end),
    meck:expect(aec_parent_connector, fetch_block_by_height,
                fun(Height) ->
                    Block = block_by_height(Height),
                    {ok, Block}
                end).

unmock_parent_connector() ->
    meck:unload(aec_parent_connector).

mock_events() ->
    meck:new(aec_events, []),
    meck:expect(aec_events, subscribe,
                fun(top_changed) -> ok end),
    ok.

unmock_events() ->
    meck:unload(aec_events).

child_new_top(CachePid, Height) ->
    CachePid ! {gproc_ps_event, top_changed, #{info => #{block_type => key,
                                                         height => Height}}}.

test_follow_child_cache_consistency(#{ child_start_height := StartHeight,
                                       child_top_height   := ChildTop,
                                       blocks             := Blocks,
                                       max_size           := CacheMaxSize,
                                       top_height         := TopHeight}) ->
    ?assertEqual(CacheMaxSize, map_size(Blocks)),
    CacheExpectedStart = min(ChildTop + StartHeight, TopHeight - CacheMaxSize),
    ?assertEqual(CacheExpectedStart, lists:min(maps:keys(Blocks))),
    CacheExpectedEnd = CacheExpectedStart + CacheMaxSize - 1,
    ?assertEqual(CacheExpectedEnd, lists:max(maps:keys(Blocks))),
    lists:foreach(
        fun(Height) -> {true, Height} = {maps:is_key(Height, Blocks), Height} end,
        lists:seq(CacheExpectedEnd, CacheExpectedEnd)),
    ok.
