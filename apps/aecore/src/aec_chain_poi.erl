-module(aec_chain_poi).

-behavior(gen_server).


-export([ serialize/1 ]).

-export([ status/0
        , enable/0
        , disable/0 ]).

-export([ start_link/0 ]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-define(POI_VSN, 1).     % TODO: also defined in aec_trees.erl
-define(VALUE, <<"1">>).

-define(STATUS_KEY, {?MODULE, status}).
-define(ENABLED, enabled).
-define(DISABLED, disabled).

-define(IF_ENABLED(Expr), if_chain_poi_enabled(fun() -> Expr end)).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    aec_events:subscribe(chain_sync),
    case config([<<"chain">>, <<"poi">>, <<"enable">>]) of
        true ->
            {ok, #{status => awaiting_sync, cache => #{}}};
        false ->
            put_status(?DISABLED),
            {ok, #{status => disabled, cache => #{}}}
    end.

handle_call(_Req, _From, St) ->
    {error, unknown_request, St}.

handle_cast(enable, #{status := disabled, syncing := false} = St) ->
    St1 = spawn_prep(St),
    {noreply, St1};
handle_cast(disable, St) ->
    case St of
        #{status := disabled} ->
            {noreply, St};
        #{status := preparing, prep_pid := Pid} ->
            terminate_prep(Pid),
            {noreply, maps:remove(prep_pid, St#{status := disabled})};
        #{status := enabled} ->
            {noreply, St#{status := disabled}}
    end;
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({gproc_ps_event, chain_sync, #{info := {chain_sync_done, _}}}, St) ->
    case St of
        #{status := awaiting_sync} ->
            St1 = spawn_prep(St),
            {noreply, St1};
        _ ->
            {noreply, St}
    end;
handle_info({'DOWN', MRef, process, PrepPid, Reason}, St) ->
    case St of
        #{status := preparing, prep_pid := {PrepPid, MRef}} ->
            case Reason of
                {ok, ok} ->
                    lager:debug("Chain POI prep succeeded", []),
                    put_status(?ENABLED),
                    {noreply, maps:remove(prep_pid, St#{status := enabled})};
                Other ->
                    lager:error("Chain POI prep failed: ~p", [Other]),
                    put_status(?DISABLED),
                    {noreply, maps:remove(prep_pid, St#{status => disabled})}
            end;
        _ ->
            lager:debug("Ignoring unknown DOWN msg", []),
            {noreply, St}
    end;
handle_info(_, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

spawn_prep(St) ->
    St1 = check_config(St),
    PidMRef = spawn_monitor(fun() -> exit({ok, do_prep(St1)}) end),
    St#{status => preparing, prep_pid => PidMRef}.

do_prep(#{lag := Lag} = St) ->
    OurTop = locate_top(Lag),
    case find_mpt_top(Lag, OurTop) of
        error ->
            MPT0 = new_tree_at_genesis(),
            update_tree(#{height => 0, mpt => MPT0});
        Info when is_map(Info) ->
            MPT = tree_at_height(Info),
            update_tree(MPT)
    end.

terminate_prep({Pid, MRef}) ->
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    after 5000 ->
            error(timeout)
    end.

get_mpt(Hash) ->
    case aec_db:get_chain_poi_state(Hash) of
        {value, RootHash} ->
            {ok, aeu_mtrees:new_with_backend(
                   RootHash, aec_db_backends:chain_poi_backend())};
        none ->
            error
    end.

tree_at_height(#{root_hash := RootHash}) ->
    aeu_mtrees:new_with_backend(RootHash, aec_db_backends:chain_poi_backend()).

new_tree_at_genesis() ->
    GHash = aec_consensus:genesis_hash(),
    MPT0 = aeu_mtrees:empty_with_backend(aec_db_backends:chain_poi_backend()),
    MPT1 = aeu_mtrees:enter(GHash, ?VALUE, MPT0),
    aec_db:ensure_transaction(
      fun() ->
              MPT2 = aeu_mtrees:commit_to_db(MPT1),
              GRoot = aeu_mtrees:root_hash(MPT2),
              set_phash(GHash, GRoot),
              MPT2
      end).

update_tree(#{height := H, mpt := MPT}) ->
        error(nyi).

locate_top(Lag) ->
    Top = aec_chain:top_height(),
    Top - Lag.

%% Locate the latest height where we have a POI entry (root hash)
%% This is done using binary search (dirty access).
%% Maybe we should save the top poi height in the chain state?

find_mpt_top(Lag, OurTop) ->
    aec_db:ensure_dirty(fun() ->
                                find_mpt_top_(Lag, OurTop)
                        end).

find_mpt_top_(Lag, OurTop) ->
    case has_phash(0) of
        {true, GenInfo} ->
            bsearch_for_poi_hash(0, OurTop, GenInfo);
        false ->
            error
    end.

bsearch_for_poi_hash(A, B, Acc) when B > A ->
    case has_phash(B) of
        {true, Info} ->
            Info;
        false ->
            Mid = A + ((B - A) div 2),
            case has_phash(Mid) of
                {true, MidInfo} ->
                    bsearch_for_poi_hash(Mid, B-1, MidInfo);
                false ->
                    bsearch_for_poi_hash(A, Mid, Acc)
            end
    end;
bsearch_for_poi_hash(A, _, Acc) ->
    {A, Acc}.

has_phash(Height) when is_integer(Height) ->
    Hash = aec_chain_state:get_key_block_hash_at_height(Height),
    case aec_db:get_chain_poi_state(Hash) of
        {value, Val} ->
            {true, #{height => Height,
                     block_hash => Hash,
                     root_hash => Val}};
        none ->
            false
    end.

set_phash(BHash, Root) ->
    aec_db:set_chain_poi_state(BHash, Root).
    
check_config(St) ->
    Depth = config([<<"sync">>, <<"sync_allowed_height_from_top">>]),
    Lag = config([<<"chain">>, <<"poi">>, <<"lag">>], Depth),
    Multiple = config([<<"chain">>, <<"poi">>, <<"height_multiple">>]),
    St#{ lag => Lag
       , multiple => Multiple }.

config(Key) ->
    {ok, Value} = aeu_env:find_config(Key, [user_config, schema_default]),
    Value.

config(Key, Default) ->
    {ok, Value} = aeu_env:find_config(Key, [user_config, schema_default, {value, Default}]),
    Value.

serialize(Height) ->
    case poi_tree_at_height(Height) of
        {ok, Tree} ->
            Hashes = [genesis_hash(), hash_at_height(Height)],
            Poi0 = aec_poi:new(aec_mtrees:root_hash(Tree)),
            Poi1 = lists:foldl(
                     fun(Hash, Acc) ->
                             ok(aec_poi:add_poi(Hash, Tree, Acc))
                     end, Poi0, Hashes),
            Format = aec_poi:serialization_format(Poi1),
            Serialized =
                aeser_chain_objects:serialize(trees_poi, ?POI_VSN, template(), [{blocks, Format}]),
            {ok, Serialized};
        error ->
            {error, no_tree_at_height}
    end.

ok({ok, Value}) ->
    Value.

genesis_hash() ->
    aec_consensus:get_genesis_hash().

hash_at_height(Height) ->
    aec_chain_state:get_key_block_hash_at_height(Height).

poi_tree_at_height(Height) ->
    case status() of
        ?ENABLED ->
            foo
    end.

template() ->
    [{blocks, [aec_poi:serialization_format_template()]}].

if_chain_poi_enabled(F) ->
    case status() of
        ?ENABLED ->
            F();
        ?DISABLED ->
            {error, chain_poi_disabled}
    end.

status() ->
    persistent_term:get(?STATUS_KEY, ?DISABLED).

enable() ->
    put_status(?ENABLED),
    gen_server:cast(?MODULE, enable).

disable() ->
    put_status(?DISABLED),
    gen_server:cast(?MODULE, disable).

put_status(S) when S==?ENABLED;
                   S==?DISABLED ->
    persistent_term:put(?STATUS_KEY, S).
