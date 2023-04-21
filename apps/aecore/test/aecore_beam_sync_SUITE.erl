-module(aecore_beam_sync_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    start_first_node/1,
    mine_on_first/1,
    start_second_node/1
   ]).

-include_lib("common_test/include/ct.hrl").

-import(aec_test_utils, [running_apps/0, loaded_apps/0, restore_stopped_and_unloaded_apps/2]).

-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

all() ->
    [ {group, all_nodes} ].

groups() ->
    [
     {all_nodes, [sequence], [{group, two_nodes}]},
     {two_nodes, [sequence],
      [start_first_node,
       mine_on_first,
       start_second_node]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Config = aec_metrics_test_utils:make_port_map([dev1, dev2], Config0),
    Forks = aecore_suite_utils:forks(),
    
    DefCfg = #{
        <<"metrics">> => #{
            <<"rules">> => [
                #{<<"name">> => <<"ae.epoch.system.**">>,
                  <<"actions">> => <<"log">>},
                #{<<"name">> => <<"ae.epoch.aecore.**">>,
                  <<"actions">> => <<"log,send">>}
            ]
        },
        <<"chain">> => #{
            <<"persist">> => true,
            <<"hard_forks">> => Forks
        },
        <<"sync">> => #{
            <<"mode">> => <<"beam">>,
            <<"single_outbound_per_group">> => false
        },
        <<"mempool">> => #{
            <<"tx_ttl">> => 100
        },
        <<"mining">> => #{
            
            <<"micro_block_cycle">> => 100
        }
    },
    aecore_suite_utils:init_per_suite([dev1, dev2], DefCfg,
                                      [{add_peers, true}],
                                      [{symlink_name, "latest.sync"}, {test_module, ?MODULE}] ++ Config).

end_per_suite(Config) ->
    stop_devs(Config).

init_per_group(TwoNodes, Config) when
        TwoNodes =:= two_nodes; TwoNodes =:= semantically_invalid_tx;
        TwoNodes =:= mempool_sync; TwoNodes =:= run_benchmark ->
    Config1 = config({devs, [dev1, dev2]}, Config),
    InitialApps = {running_apps(), loaded_apps()},
    {ok, _} = application:ensure_all_started(exometer_core),
    ok = aec_metrics_test_utils:start_statsd_loggers(aec_metrics_test_utils:port_map(Config1)),
    [{initial_apps, InitialApps} | Config1];
init_per_group(all_nodes, Config) ->
    Config.


end_per_group(Group, Config) when Group =:= two_nodes;
                                  Group =:= three_nodes;
                                  Group =:= node_info;
                                  Group =:= peer_analytics;
                                  Group =:= semantically_invalid_tx;
                                  Group =:= run_benchmark ->
    ct:log("Metrics: ~p", [aec_metrics_test_utils:fetch_data()]),
    ok = aec_metrics_test_utils:stop_statsd_loggers(),
    stop_devs(Config),
    case proplists:lookup(initial_apps, Config) of
        none -> ok;
        {_, {OldRunningApps, OldLoadedApps}} ->
            ok = restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps)
    end,
    ok;
end_per_group(all_nodes, _Config) ->
   ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

start_first_node(Config) ->
    NodeCfg0 = aecore_suite_utils:node_config(dev1, Config),
    ct:log("Dev1 config before: ~p",[NodeCfg0]),
    {dev1, {_PrivKey, PubKey}} = lists:keyfind(dev1, 1, aecore_suite_utils:sign_keys()),
    NodeCfg = NodeCfg0#{<<"mining">> => #{<<"autostart">> => false,
                                <<"beneficiary">> => aeser_api_encoder:encode(account_pubkey, PubKey)},
                        <<"sync">> => #{<<"mode">> => <<"sequential">>}},
    aecore_suite_utils:create_config(dev1, Config, NodeCfg,
                                            []),
    ct:log("Dev1 config: ~p",[NodeCfg]),
    aecore_suite_utils:start_node(dev1, Config),
    connect(aecore_suite_utils:node_name(dev1)),
    ok = aecore_suite_utils:check_for_logs([dev1], Config),
    ok.

start_second_node(Config) ->
    [Dev1, Dev2] = [dev1, dev2],
    N1 = aecore_suite_utils:node_name(Dev1),
    N2 = aecore_suite_utils:node_name(Dev2),
    
    aecore_suite_utils:start_node(Dev2, Config, []),
    connect(N2),
    aecore_suite_utils:await_aehttp(N2),
    ct:log("Connected peers on dev2: ~p",
           [rpc:call(N2, aec_peers, connected_peers, [], 5000)]),
    B1 = rpc:call(N1, aec_chain, top_block, [], 5000),
    ok = aecore_suite_utils:check_for_logs([Dev2], Config),
    true = expect_block(N2, B1).

-define(MINE_RATE, 100).
mine_on_first(Config) ->
    [ Dev1 | _ ] = proplists:get_value(devs, Config),
    {_, Pub} = aecore_suite_utils:sign_keys(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    spend(Dev1, Pub),
    %% aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(Dev1),
    mine_a_key_block(Dev1),
    mine_a_micro_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    spend(Dev1, Pub),
    mine_a_key_block(Dev1),
    mine_a_micro_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    mine_a_key_block(Dev1),
    ok.

stop_devs(Config) ->
    Devs = proplists:get_value(devs, Config, []),
    stop_devs(Devs, Config).

stop_devs(Devs, Config) ->
    lists:foreach(
        fun(Node) ->
            {ok, DbCfg} = node_db_cfg(Node),
            aecore_suite_utils:stop_node(Node, Config),
            aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
        end,
        Devs),
    ok.

%% ==================================================
%% Private functions
%% ==================================================


expect_block(N, B) ->
    retry(fun() -> expect_block_(N, B) end,
          {?LINE, expect_block, N, B}).

expect_block_(N, B) ->
    Bn = rpc:call(N, aec_chain, top_block, [], 5000),
    case B =:= Bn of
        true ->
            Bal = get_balance(N),
            ct:log("Got block (~p); Height = ~p; Balance = ~p", [N, aec_blocks:height(Bn), Bal]),
            true;
        false ->
            {false, Bn}
    end.

retry(Test, Info) ->
    retry(Test, 5, Info).

retry(Test, Retries, Info) ->
    retry_(Test, #{prev => undefined,
                   retries => Retries,
                   tries => Retries,
                   info => Info}).

retry_(Test, #{tries := Tries} = S) when Tries > 0 ->
    case Test() of
        true ->
            true;
        false ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries -1});
        {false, V} ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries -1, prev => V})
    end;
retry_(_, S) ->
    ct:log("exhausted retries (~p)", [S]),
    ct:fail({retry_exhausted, S}).

%% ============================================================
%% Transaction support functions
%% ============================================================
spend(Node, Pub) ->
    NName = aecore_suite_utils:node_name(Node),
    {ok, Tx} = aecore_suite_utils:spend(NName, Pub, Pub, 1, ?SPEND_FEE),
    {ok, [Tx]} = rpc:call(NName, aec_tx_pool, peek, [infinity]),
    ct:log("Spend tx ~p", [Tx]),
    %%TxHashes = [aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx)) || STx <- [Tx]],
    %%aecore_suite_utils:mine_blocks_until_txs_on_chain(NName, TxHashes, 20),
    Tx.

mine_a_key_block(Node) ->
    NName= aecore_suite_utils:node_name(Node),
    {ok, [Block]} = mine_key_blocks(NName, 1),
    Top = rpc:call(NName, aec_chain, top_block, [], 5000),
    ct:log("top of chain ~p: ~p (mined ~p)", [Node, Top, Block]),
    {Top, Top} = {Top, Block},
    Top.

mine_key_blocks(Node, N) ->
    aecore_suite_utils:mine_blocks(Node, N, ?MINE_RATE, key, #{}).

mine_a_micro_block(Node) ->
    NName = aecore_suite_utils:node_name(Node),
    aecore_suite_utils:mine_blocks(NName, 1, ?MINE_RATE, micro, #{}).

get_balance(N) ->
    rpc:call(N, aec_mining, get_miner_account_balance, [], 5000).

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun(M, F, A)->
                        rpc:call(aecore_suite_utils:node_name(Node),
                                  M, F, A, 5000)
                    end),
    {ok, DbCfg}.

config({devs, Devs}, Config) ->
    [ {devs, Devs}
    , {nodes, [aecore_suite_utils:node_tuple(Dev) || Dev <- Devs]}
    | Config
    ].


connect(Node) ->
    aecore_suite_utils:connect_wait(Node, aesync).