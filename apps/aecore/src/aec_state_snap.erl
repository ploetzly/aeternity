-module(aec_state_snap).
-behavior(gen_server).

-export([ snap_heights/0
        , tree_roots/1
        , leaves/4 ]).

-export([ start_link/0 ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(st, { announced
            , cache = #{} }).

-record(tree_roots, { height }).
-record(snap_heights, {}).
-record(leaves, {hash, tree, root, keys}).

-define(TIMEOUT, 30000).

tree_roots(Height) ->
    call(#tree_roots{height = Height}).

snap_heights() ->
    call(#snap_heights{}).

leaves(Hash, Tree, Root, Keys) ->
    call(#leaves{hash = Hash, tree = Tree, root = Root, keys = Keys}).


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
    {Heights, St1} = snap_heights(St),
    {reply, Heights, St1};
handle_call(#leaves{hash = Hash, tree = Tree, root = Root, keys = Keys} = Req, _From, St) ->
    case get_tree_at_height(Req, St) of
        {ok, T, St1} ->
            {Result, St2} = get_leaves(T, Req, St1),
            {reply, Result, St2};
        {error, _} = Error ->
            {reply, Error, St}
    end;
handle_call(_Req, _From, St) ->
    {error, unknown_call, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    %% For now, at least, we try to enable fork resistance as soon as we are in sync
    %% with at least one peer
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


snapshots_enabled() ->
    {ok, Enabled} = aeu_env:find_config([<<"sync">>, <<"snapshots">>, <<"enabled">>],
                                        [ user_config
                                        , schema_default ]),
    Enabled.

announce_capability() ->
    FinalizedOrTop = aec_db:ensure_dirty(fun get_finalized_or_top/0),
    Cap = #{ from_height => 1
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
            tree_roots_of_hash(Height, Hash, St1);
        error ->
            {error, St1}
    end.

tree_roots_of_hash(Height, Hash, St) ->
    case find_in_cache(Height, tree_roots, St) of
        {ok, Res} ->
            {Res, St};
        error ->
            store_in_cache(
              Height, tree_roots, St,
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
                    Cache1 = Cache#{Height => #{hash => {value, Hash}}},
                    {{ok, Height}, St#st{cache = Cache1}}
            end;
        none ->
            {{error, unknown_hash}, St}
    end.

get_tree_at_height(#leaves{hash = Hash, root = Root, tree = Tree}, St) ->
    {HeightRes, St1} = height_of_hash(Hash, St),
    case HeightRes of
        {ok, Height} ->
            case lists:member(Height, snap_heights()) of
                true ->
                    {RootsRes, St2} = tree_roots_of_hash(Height, Hash, St1);
                false ->
                    {error, invalid_height}
            end;
        none ->
            {error, unknown_hash}
    end.

get_leaves(Tree, Req, St) ->
    {error, nyi}.

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
snap_heights(#st{cache = Cache} = St) ->
    Top = aec_db:ensure_dirty(fun get_finalized_or_top/0),
    Heights = snap_heights_(Top),
    {Heights, St#st{cache = maps:with(Heights, Cache)}}.

snap_heights_(Top) ->
    Mod = Top div 1000,
    [ N * 1000 || N <- lists:seq(max(1, Mod - 3), Mod)].
