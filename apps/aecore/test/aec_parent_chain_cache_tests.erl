%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_parent_chain_cache
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_parent_chain_cache).

%%%===================================================================
%%% Test cases
%%%===================================================================

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
     [  {"Cache all the blocks above current child height", fun cache_all_above_child_height/0},
        {"Post cachable parent top", fun post_cachable_parent_top/0},
        {"Post non cachable parent top", fun post_non_cachable_parent_top/0},
        {"Post child top in the middle of caching heights", fun post_child_top_in_the_middle_of_cachable_heights/0}
     ]}.

%%%===================================================================
%%% Test cases
%%%===================================================================

cache_all_above_child_height() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, _CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(10),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight = ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_cachable_parent_top() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(10),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) - 2,
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(10),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_non_cachable_parent_top() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(10),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) + 10,
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(10),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

post_child_top_in_the_middle_of_cachable_heights() ->
    Test =
        fun(CacheMaxSize, StartHeight, ChildTop0) ->
            meck:expect(aec_chain, top_height, fun() -> ChildTop0 end),
            {ok, CachePid} = start_cache(StartHeight, CacheMaxSize),
            timer:sleep(10),
            %% the cache is waiting for a new top, the cache is up to the target top
            ExpectedTopHeight =  ChildTop0 + StartHeight,
            MaxCachableHeight =
                fun(CurrentChildTop) -> CurrentChildTop + StartHeight + CacheMaxSize end,
            {ok, #{ child_start_height := StartHeight,
                    top_height         := ExpectedTopHeight,
                    child_top_height   := ChildTop0}} = ?TEST_MODULE:get_state(),
            %% post some top in the cache's range
            ParentTop = MaxCachableHeight(ChildTop0) + 10,
            ?TEST_MODULE:post_block(block_by_height(ParentTop)),
            timer:sleep(10),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop0,
                    top_height         := ParentTop}} = ?TEST_MODULE:get_state(),
            ChildTop1 = ChildTop0 + 10,
            child_new_top(CachePid, ChildTop1),
            timer:sleep(10),
            {ok, #{ child_start_height := StartHeight,
                    child_top_height   := ChildTop1,
                    top_height         := ParentTop} = Res} = ?TEST_MODULE:get_state(),
            assert_child_cache_consistency(Res),
            ?TEST_MODULE:stop()
        end,
    Test(20, 200, 0),
    Test(20, 200, 50),
    ok.

%%%===================================================================
%%% Helper functions 
%%%===================================================================

start_cache(StartHeight, MaxSize) ->
    Args = [StartHeight, MaxSize],
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
                end),
    meck:expect(aec_parent_connector, request_top,
                fun() -> ok end).

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

assert_child_cache_consistency(#{ child_start_height := StartHeight,
                                  child_top_height   := ChildTop,
                                  blocks             := Blocks,
                                  max_size           := CacheMaxSize,
                                  top_height         := TopHeight}) ->
    ?assertEqual(CacheMaxSize, map_size(Blocks)),
    CacheExpectedStart = min(ChildTop + StartHeight, TopHeight - CacheMaxSize + 1),
    ?assertEqual(CacheExpectedStart, lists:min(maps:keys(Blocks))),
    CacheExpectedEnd = CacheExpectedStart + CacheMaxSize - 1,
    ?assertEqual(CacheExpectedEnd, lists:max(maps:keys(Blocks))),
    lists:foreach(
        fun(Height) -> {true, Height} = {maps:is_key(Height, Blocks), Height} end,
        lists:seq(CacheExpectedEnd, CacheExpectedEnd)),
    ok.
