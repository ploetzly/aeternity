-module(aec_state_snap).
-behavior(gen_server).

-export([ snap_heights/0
        , tree_roots/1
        , leaves/4 ]).

-export([ clear_cache/0
        , hash_at_height/1
        , snap_heights/1 ]).

-export([ start_link/0 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-compile(export_all).

-record(st, { announced
            , snap_heights = 1000 :: non_neg_integer()
            , cache = #{} }).

-record(snap_wip, { trees
                  , cont = init }).

-record(tree_roots, { height }).
-record(snap_heights, {}).
-record(leaves, {hash, tree, root, keys}).

-define(TIMEOUT, 30000).

%% TODO: The lager:info() entries are for initial debugging only. Remove.

test_fetch(Height) ->
    {ok, Hash} = hash_at_height(Height),
    snap_heights(1),
    lager:info("Hash = ~p", [Hash]),
    {ok, Roots} = tree_roots(Height),
    lager:info("Roots = ~p", [Roots]),
    Hashes = aec_trees:deserialize_to_tree_hashes(Roots),
    lager:info("Hashes = ~p", [Hashes]),
    lists:foreach(fun(X) ->
                          test_create_and_fetch_tree(X, Height, Hash)
                  end, Hashes),
    ok.

test_create_and_fetch_tree({HashName, RootHash}, Height, BHash) ->
    TreeName = tree_name(HashName),
    T0 = aeu_mtrees:empty(),
    lager:info("Created empty tree of type ~p", [TreeName]),
    Res = fetch_slices(BHash, TreeName, RootHash, T0),
    lager:info("Fetched = ~p", [Res]),
    ok.

fetch_slices(BHash, TreeName, RootHash, Tree) ->
    case RootHash of
        empty ->
            lager:info("Tree ~p is empty - not fetching", [TreeName]),
            Tree;
        _ ->
            fetch_slices(<<>>, BHash, TreeName, RootHash, Tree)
    end.

fetch_slices(From, BHash, TreeName, RootHash, Tree) ->
    lager:info("Fetching [~p] of ~p", [From, TreeName]),
    case leaves(BHash, TreeName, RootHash, [From]) of
        #{leaves := []} ->
            lager:info("Done fetching ~p", [TreeName]),
            {ok, RootHash1} = aeu_mtrees:root_hash(Tree),
            lager:info("RootHash1 = ~p", [RootHash1]),
            lager:info("Same root hash (~p)? ~p", [TreeName, RootHash1 == RootHash]),
            ok;
        #{leaves := Leaves, poi := Poi} ->
            lager:info("Leaves = ~p", [Leaves]),
            lager:info("POI = ~p", [Poi]),
            verify_poi(Leaves, Poi),
            lager:info("POI verfied", []),
            Tree1 = lists:foldl(fun insert_leaf/2, Tree, Leaves),
            {LastKey, _} = lists:last(Leaves),
            fetch_slices(LastKey, BHash, TreeName, RootHash, Tree1)
    end.

insert_leaf({Key, Value}, Tree) ->
    aeu_mtrees:insert(Key, Value, Tree).

verify_poi([], _) ->
    ok;
verify_poi(Leaves, Poi) ->
    KVL = case Leaves of
              [_] -> Leaves;
              [A,_|_] ->
                  [A, lists:last(Leaves)]
          end,
    DeserializedPoi = deseralize_poi(Poi),
    [ok = aec_poi:verify(K, V, DeserializedPoi)
     || {K, V} <- KVL],
    ok.

tree_name(contracts_hash) -> contracts;
tree_name(calls_hash    ) -> calls;
tree_name(channels_hash ) -> channels;
tree_name(ns_hash       ) -> names;
tree_name(ns_cache_hash ) -> ns_cache;
tree_name(oracles_hash  ) -> oracles;
tree_name(oracles_cache_hash) -> oracles_cache;
tree_name(accounts_hash ) -> accounts.

%%% Configurable data

max_distance_from_top() ->
    %% 0 means 'from genesis'. A value N > 0 means N generations from the top
    config([<<"sync">>, <<"snapshots">>, <<"max_distance">>]).

max_leaves() ->
    config([<<"sync">>, <<"snapshots">>, <<"max_leaves">>]).

snapshots_enabled() ->
    config([<<"sync">>, <<"snapshots">>, <<"enabled">>]).

config(Key) ->
    {ok, Value} = aeu_env:find_config(Key, [user_config, schema_default]),
    Value.

%%% End configurable data

tree_roots(Height) ->
    call(#tree_roots{height = Height}).

snap_heights() ->
    call(#snap_heights{}).

leaves(Hash, Tree, Root, Keys) ->
    call(#leaves{hash = Hash, tree = Tree, root = Root, keys = Keys}).

clear_cache() ->
    call(clear_cache).

hash_at_height(Height) ->
    call({hash_at_height, Height}).

snap_heights(Multiple) when is_integer(Multiple), Multiple > 0 ->
    call({snap_heights, Multiple}).

call(Req) ->
    gen_server:call(?MODULE, Req, ?TIMEOUT).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Config:
%% sync:
%%    snapshots:
%%        enabled: boolean
init([]) ->
    aec_events:subscribe(chain_sync),
    {ok, #st{announced = undefined}}.


handle_call(#tree_roots{height = Height}, _From, St) ->
    {Reply, St1} = tree_roots_int(Height, St),
    {reply, Reply, St1};
handle_call(#snap_heights{}, _From, St) ->
    {Heights, St1} = snap_heights_(St),
    {reply, Heights, St1};
handle_call(#leaves{hash = Hash, tree = Tree, root = Root, keys = Keys} = Req, _From, St) ->
    case get_tree_at_height(Req, St) of
        {{value, T}, St1} ->
            lager:info("Tree found", []),
            {Result, St2} = get_leaves_int(T, Req, St1),
            lager:info("Result = ~p", [Result]),
            {reply, Result, St2};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call(clear_cache, _From, St) ->
    {reply, ok, St#st{cache = #{}}};
handle_call({hash_at_height, Height}, _From, St) ->
    {Res, St1} = hash_at_height(Height, St),
    {reply, Res, St1};
handle_call({snap_heights, Hs}, _From, St) ->
    if is_integer(Hs), Hs > 0 ->
            {reply, ok, St#st{snap_heights = Hs}};
       true ->
            {reply, {error, invalid}, St}
    end;
handle_call(_Req, _From, St) ->
    {error, unknown_call, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    case snapshots_enabled() of
        true ->
            announce_capability();
        false ->
            ignore
    end,
    {noreply, St};
handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, State, _Extra) ->
    {ok, State}.


announce_capability() ->
    FinalizedOrTop = aec_db:ensure_dirty(fun get_finalized_or_top/0),
    Cap = #{ from_height => max(1, FinalizedOrTop - max_distance_from_top())
           , to_height => FinalizedOrTop },
    aec_capabilities:set_capability(snap, Cap).

%% TODO: update top height modulo 1000 or so?
get_finalized_or_top() ->
    case aec_db:get_finalized_height() of
        undefined ->
            aec_chain:top_height();
        FHeight ->
            FHeight
    end.

tree_roots_int(Height, #st{cache = Cache} = St) ->
    {HashRes, St1} = hash_at_height(Height, St),
    case HashRes of
        {ok, Hash} ->
            case trees_of_hash(Height, Hash, St1) of
                {{value, Trees}, St2} ->
                    Serialized = aec_trees:serialize_for_db(Trees),
                    {{ok, Serialized}, St2};
                {none, St2} ->
                    {error, St2}
            end;
        error ->
            {error, St1}
    end.

tree_hash_bin(T, Trees) ->
    case aec_trees:tree_hash(T, Trees) of
        {ok, Hash} ->
            Hash;
        {error, empty} ->
            <<>>
    end.

trees_of_hash(Height, Hash, St) ->
    case find_in_cache(Height, tree_roots, St) of
        {ok, Res} ->
            {Res, St};
        error ->
            store_in_cache(
              Height, trees_of_hash, St,
              fun() ->
                      db_find_block_state(Hash)
              end)
    end.

db_find_block_state(Hash) ->
    aec_db:ensure_dirty(
      fun() ->
              aec_db:find_block_state(Hash, _Dirty = true)
      end).

hash_at_height(Height, St) ->
    maybe_cached(Height, hash, St, fun() ->
                                           hash_at_height_(Height)
                                   end).

hash_at_height_(Height) ->
    aec_db:ensure_dirty(
      fun() ->
              aec_chain_state:get_key_block_hash_at_height(Height)
      end).

height_of_hash(Hash, St) ->
    case aec_db:dirty_find_header(Hash) of
        {value, Hdr} ->
            Height = aec_headers:height(Hdr),
            %% Ensure that cache is consistent
            case find_in_cache(Height, hash, St) of
                {ok, {value, Hash}} ->
                    {{ok, Height}, St};
                {ok, _} ->
                    lager:debug("Cache inconsistent at ~p - clear", [Height]),
                    #st{cache = Cache} = St,
                    Cache1 = Cache#{Height => #{hash => {ok, Hash}}},
                    {{ok, Height}, St#st{cache = Cache1}}
            end;
        none ->
            {{error, unknown_hash}, St}
    end.

get_tree_at_height(#leaves{hash = Hash, root = Root, tree = TreeName}, St) ->
    {HeightRes, St1} = height_of_hash(Hash, St),
    case HeightRes of
        {ok, Height} ->
            {SnapHeights, St2} = snap_heights_(St1),
            case lists:member(Height, SnapHeights) of
                true ->
                    case trees_of_hash(Height, Hash, St2) of
                        {{value, Trees}, St3} ->
                            {Tree, ActualRootHash} = tree(TreeName, Trees),
                            if Root == ActualRootHash ->
                                    {{value, Tree}, St3};
                               true ->
                                    lager:info("Root hash mismatch: Claimed=~p, Actual=~p",
                                               [Root, ActualRootHash]),
                                    {error, root_hash_mismatch}
                            end;
                        {none, St3} ->
                            {error, no_such_tree}
                    end;
                false ->
                    {error, invalid_height}
            end;
        none ->
            {error, unknown_hash}
    end.

tree(accounts, Ts) ->
    {aec_trees:accounts(Ts), ok(aec_trees:accounts_hash(Ts))};
tree(calls, Ts) ->
    {aec_trees:calls(Ts), ok(aec_trees:calls_hash(Ts))};
tree(contracts, Ts) ->
    {aec_trees:contracts(Ts), ok(aec_trees:contracts_hash(Ts))};
tree(NS, Ts) when NS==ns; NS==names ->
    {ns_tree(Ts), ok(aec_trees:ns_hash(Ts))};
tree(ns_cache, Ts) ->
    {ns_cache_tree(Ts), ok(aec_trees:ns_cache_hash(Ts))};
tree(channels, Ts) ->
    {aec_trees:channels(Ts), ok(aec_trees:channels_hash(Ts))};
tree(oracles, Ts) ->
    {oracles_tree(Ts), ok(aec_trees:oracles_hash(Ts))};
tree(oracles_cache, Ts) ->
    {oracles_cache_tree(Ts), ok(aec_trees:oracles_cache_hash(Ts))}.

ns_tree(Ts) ->
    NS = aec_trees:ns(Ts),
    Hash = aens_state_tree:root_hash(NS),
    DB = aens_state_tree:ns_db(NS),
    aeu_mtrees:new_with_backend(Hash, DB).

ns_cache_tree(Ts) ->
    NS = aec_trees:ns(Ts),
    Hash = aens_state_tree:cache_root_hash(NS),
    DB = aens_state_tree:cache_db(NS),
    aeu_mtrees:new_with_backend(Hash, DB).

oracles_tree(Ts) ->
    Os = aec_trees:oracles(Ts),
    Hash = aeo_state_tree:root_hash(Os),
    DB = aeo_state_tree:oracles_db(Os),
    aeu_mtrees:new_with_backend(Hash, DB).

oracles_cache_tree(Ts) ->
    Os = aec_trees:oracles(Ts),
    Hash = aeo_state_tree:cache_root_hash(Os),
    DB = aeo_state_tree:cache_db(Os),
    aeu_mtrees:new_with_backend(Hash, DB).

-define(LEAVES_MAX, 2).  % Set very low for testing purposes
%%
get_leaves_int(Tree, Req, St) ->
    #leaves{hash = Hash, tree = TreeName, root = Root, keys = Keys} = Req,
    Leaves = case Keys of
                 [Start, Stop] ->
                     I = aeu_mp_trees:iterator_from(Start, Tree),
                     get_leaves_int_i(iter_next(I), Stop, ?LEAVES_MAX);
                 [Start] ->
                     I = aeu_mp_trees:iterator_from(Start, Tree),
                     get_leaves_int_i(iter_next(I), ?LEAVES_MAX)
             end,
    Poi = case Leaves of
              [] -> [];
              [{K,_}] ->
                  Hashes = [K],
                  create_poi(Tree, Hashes);
              [{K1,_},_|_] ->
                  K2 = element(1, lists:last(Leaves)),
                  Hashes = [K1, K2],
                  create_poi(Tree, Hashes)
          end,
    %% No caching for now, so St is left unchanged
    {#{ leaves => Leaves, poi => Poi}, St}.

iter_next(I) ->
    aeu_mp_trees:iterator_next(I).

get_leaves_int_i({Key, Value, I}, Num) when Num > 0 ->
    [{Key, Value} | get_leaves_int_i(iter_next(I), Num - 1)];
get_leaves_int_i(_, _) ->
    [].

get_leaves_int_i({Key, Value, I}, Stop, Num) when Key =< Stop, Num > 0 ->
    [{Key, Value} | get_leaves_int_i(iter_next(I), Stop, Num - 1)];
get_leaves_int_i(_, _, _) ->
    [].

-define(POI_VSN, 1).   %% TODO: move to shared location
create_poi(Tree, Hashes) ->
    Poi0 = aec_poi:new(ok(aeu_mtrees:root_hash(Tree))),
    Poi1 = lists:foldl(
             fun(Hash, Acc) ->
                     ok(aec_poi:add_poi(Hash, Tree, Acc))
             end, Poi0, Hashes),
    serialize_poi(Poi1).

serialize_poi(Poi) ->
    Format = aec_poi:serialization_format(Poi),
    aeser_chain_objects:serialize(trees_poi, ?POI_VSN, template(), [{leaves, [Format]}]).

deseralize_poi(Bin) ->
    Template = aec_poi:serialization_format_template(),
    [{leaves, [Deserialized]}] =
        aeser_chain_objects:deserialize(trees_poi, ?POI_VSN, [{leaves, [Template]}], Bin),
    aec_poi:from_serialization_format(Deserialized).

ok({ok, Value}) -> Value.

template() ->
    [{leaves, [aec_poi:serialization_format_template()]}].

maybe_cached(Height, Key, #st{cache = Cache} = St, Fun) ->
    case find_in_cache(Height, Key, St) of
        {ok, Value} ->
            {Value, St};
        error ->
            store_in_cache(Height, Key, St, Fun)
    end.

find_in_cache(Height, Key, #st{cache = Cache}) ->
    case maps:find(Height, Cache) of
        {ok, HCache} ->
            maps:find(Key, HCache);
        error ->
            error
    end.

store_in_cache(Height, Key, St, Fun) ->
    Value = Fun(),
    {Value, store_in_cache_(Height, Key, Value, St)}.

store_in_cache_(Height, Key, Value, #st{cache = Cache} = St) ->
    HCache = maps:get(Height, Cache, #{}),
    St#st{cache = Cache#{Height => HCache#{Key => Value}}}.

%% Return a list of at most the last 3 highest multiples of 1000 below the top.
snap_heights_(#st{cache = Cache, snap_heights = Hs} = St) ->
    Top = aec_db:ensure_dirty(fun get_finalized_or_top/0),
    Heights = snap_heights_(Top, Hs),
    {Heights, St#st{cache = maps:with(Heights, Cache)}}.

snap_heights_(Top, Hs) ->
    Mod = Top div Hs,
    [ N * Hs || N <- lists:seq(max(1, Mod - 3), Mod)].
