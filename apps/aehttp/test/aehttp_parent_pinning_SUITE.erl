-module(aehttp_parent_pinning_SUITE).
%% mostly stolen from aehttp_stake_contract_SUITE.erl

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).
                           

-export([all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([ agent_write_parent_chain/1

    ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
%%-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(STAKING_CONTRACT, "MainStaking").
-define(POS_ELECTION_CONTRACT, "PoSElection").
-define(HC_ELECTION_CONTRACT, "HCElection").
-define(CONSENSUS_HC, hc).
-define(CONSENSUS_HC_BTC, hc_btc).
-define(CONSENSUS_HC_DOGE, hc_doge).
-define(CONSENSUS_POS, pos).
-define(CHILD_START_HEIGHT, 101).
-define(CHILD_CONFIRMATIONS, 0).
-define(REWARD_DELAY, 2).
-define(LAZY_INTERVAL, 10000).
-define(NODE1, dev1).
-define(NODE1_NAME, aecore_suite_utils:node_name(?NODE1)).

-define(NODE2, dev2).
-define(NODE2_NAME, aecore_suite_utils:node_name(?NODE2)).

-define(LAZY_NODE, dev8).
-define(LAZY_NODE_NAME, aecore_suite_utils:node_name(?LAZY_NODE)).



-define(OWNER_PUBKEY, <<42:32/unit:8>>).

-define(PARENT_CHAIN_NODE1, aecore_suite_utils:parent_chain_node(1)).
-define(PARENT_CHAIN_NODE1_NAME, aecore_suite_utils:node_name(?PARENT_CHAIN_NODE1)).
-define(PARENT_CHAIN_NETWORK_ID, <<"local_testnet">>).

-define(BTC_PARENT_CHAIN_PORT, 7013).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(INITIAL_STAKE, 1000000000000000000000000).

-define(PEEK_MSGQ, peek_msgq(?LINE)).

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>,
    "Alice"}).
%% ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>,
    "Bob"}).
%% ak_nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC

-define(LISA, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>,
    "Lisa"}).
%% ak_2XNq9oKtThxKLNFGWTaxmLBZPgP7ECEGxL3zK7dTSFh6RyRvaG

-define(DWIGHT, {
    <<8,137,159,99,139,175,27,58,77,11,191,52,198,199,7,50,133,195,184,219,
        148,124,4,5,44,247,57,95,188,173,95,35>>,
    <<107,251,189,176,92,221,4,46,56,231,137,117,181,8,124,14,212,150,167,
        53,95,94,50,86,144,230,93,222,61,116,85,96,8,137,159,99,139,175,27,58,
        77,11,191,52,198,199,7,50,133,195,184,219,148,124,4,5,44,247,57,95,
        188,173,95,35>>,
    "Dwight"}). %% Parent chain account
%% ak_4m5iGyT3AiahzGKCE2fCHVsQYU7FBMDiaMJ1YPxradKsyfCc9

-define(EDWIN, {
    <<212,212,169,78,149,148,138,221,156,80,4,156,9,139,144,114,243,122,20,
        103,168,43,42,244,93,118,38,98,71,34,199,94>>,
    <<81,177,15,108,16,183,128,229,4,114,166,227,47,125,145,21,68,196,185,
        115,42,198,168,204,220,206,200,58,12,32,56,98,212,212,169,78,149,148,
        138,221,156,80,4,156,9,139,144,114,243,122,20,103,168,43,42,244,93,
        118,38,98,71,34,199,94>>,
    "Edwin"}).  %% Parent chain account
%% ak_2cjUYDhaKaiyGvuswL6K96ooKZKtFZZEopgxc3hwR2Yqb8SWxd

-define(FORD, {
    <<157,139,168,202,250,128,128,7,45,18,214,147,85,31,12,182,220,213,173,
        237,6,147,239,41,183,214,34,113,100,122,208,14>>,
    <<105,184,53,188,53,158,124,5,171,89,28,64,41,203,59,179,66,53,26,132,
        75,116,139,24,228,4,200,223,25,224,76,127,157,139,168,202,250,128,128,
        7,45,18,214,147,85,31,12,182,220,213,173,237,6,147,239,41,183,214,34,
        113,100,122,208,14>>,
    "Ford"}).
%% ak_2CPHnpGxYw3T7XdUybxKDFGwtFQY7E5o3wJzbexkzSQ2BQ7caJ

-define(GENESIS_BENFICIARY, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

-define(BRI, <<"ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8">>).

all() -> [{group, hc}
         ].

groups() ->
    [{hc, [sequence], [agent_write_parent_chain]}
    ].

suite() -> [].

init_per_suite(Config0) ->
    case aect_test_utils:require_at_least_protocol(?IRIS_PROTOCOL_VSN) of
        {skip, _} = Skip -> Skip;
        ok ->
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.staking"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite([?NODE1, ?NODE2],
                                                        #{}, %% config is rewritten per suite
                                                        [],
                                                        Config),
            GenesisProtocol = 1,
            {ok, AccountFileName} = aecore_suite_utils:hard_fork_filename(?PARENT_CHAIN_NODE1, Config1, integer_to_list(GenesisProtocol), "accounts_test.json"),
            GenesisProtocolBin = integer_to_binary(GenesisProtocol),
            ParentCfg =
                #{  <<"chain">> =>
                        #{  <<"persist">> => false,
                            <<"hard_forks">> =>
                                #{  GenesisProtocolBin => #{<<"height">> => 0, <<"accounts_file">> => AccountFileName},
                                    integer_to_binary(?CERES_PROTOCOL_VSN) => #{<<"height">> => 1}
                                },
                            <<"protocol_beneficiaries">> =>
                                [<<?BRI/binary,":109::">>],
                            <<"consensus">> =>
                                #{ <<"0">> => #{<<"type">> => <<"ct_tests">>}}
                            },
                    <<"fork_management">> =>
                        #{<<"network_id">> => ?PARENT_CHAIN_NETWORK_ID},
                    %%<<"http">> => #{<<"external">> => #{<<"acceptors">> => 100}},
                    <<"http">> => #{<<"cache">> => #{<<"enabled">> => false}},
                    <<"mempool">> => #{<<"nonce_offset">> => 200},
                    <<"mining">> =>
                        #{<<"micro_block_cycle">> => 1,
                            <<"expected_mine_rate">> => 2000,
                            <<"autostart">> => false,
                            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
                            }},
            aecore_suite_utils:make_multi(Config1, [?PARENT_CHAIN_NODE1]),
            aecore_suite_utils:create_config(?PARENT_CHAIN_NODE1, Config1, ParentCfg, []),
            {_ParentPatronPriv, ParentPatronPub} = aecore_suite_utils:sign_keys(?PARENT_CHAIN_NODE1),
            ParentPatronPubEnc = aeser_api_encoder:encode(account_pubkey, ParentPatronPub),
            aecore_suite_utils:create_seed_file(AccountFileName,
                #{  ParentPatronPubEnc =>
                    100000000000000000000000000000000000000000000000000000000000000000000000,
                    encoded_pubkey(?DWIGHT) => 2100000000000000000000000000,
                    encoded_pubkey(?EDWIN) => 3100000000000000000000000000
                }),
            StakingContract = staking_contract_address(),
            ElectionContract = election_contract_address(),
            [{staking_contract, StakingContract},
                {election_contract, ElectionContract} | Config1]
    end.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
                proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(Group, Config0) ->
    VM = fate,
    Config1 = aect_test_utils:init_per_group(VM, Config0),
    case Group of
        hc -> init_per_group_custom(<<"hc">>, ?CONSENSUS_HC, Config1);
        _ -> Config1
    end.

init_per_group_custom(NetworkId, ?CONSENSUS_HC, Config) ->
    GenesisStartTime = aeu_time:now_in_msecs(),
    ElectionContract = election_contract_by_consensus(?CONSENSUS_HC),
    %% different runs use different network ids
    Env = [ {"AE__FORK_MANAGEMENT__NETWORK_ID", binary_to_list(NetworkId)}
            ],
    aecore_suite_utils:start_node(?PARENT_CHAIN_NODE1, Config),
    aecore_suite_utils:connect(?PARENT_CHAIN_NODE1_NAME, []),
    timer:sleep(1000),
    produce_blocks(?PARENT_CHAIN_NODE1, ?PARENT_CHAIN_NODE1_NAME,
                    parent, ?CHILD_START_HEIGHT, Config, ?CONSENSUS_HC),
    %% ?ALICE on the child chain, ?DWIGHT on the parent chain
    ReceiveAddress = encoded_pubkey(?FORD),
    NodeConfig1 = node_config(NetworkId,?NODE1, Config, [{?ALICE, ?DWIGHT}, {?BOB, ?EDWIN}], ReceiveAddress, ?CONSENSUS_HC,
                                true,  GenesisStartTime),
    NodeConfig2 = node_config(NetworkId,?NODE2, Config, [], ReceiveAddress, ?CONSENSUS_HC, true,
                                GenesisStartTime),
    build_json_files(ElectionContract, [NodeConfig1, NodeConfig2]),
    aecore_suite_utils:create_config(?NODE1, Config,
                                        NodeConfig1,
                                    [{add_peers, true} ]),
    aecore_suite_utils:create_config(?NODE2, Config,
                                        NodeConfig2,
                                    [{add_peers, true} ]),
    aecore_suite_utils:start_node(?NODE1, Config, Env),
    aecore_suite_utils:connect(?NODE1_NAME, []),
    aecore_suite_utils:start_node(?NODE2, Config, Env),
    aecore_suite_utils:connect(?NODE2_NAME, []),
    %% following not necessary for basic aeccagent test
    % ParentTopHeight0 = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
    % ct:log("Parent chain top height ~p", [ParentTopHeight0]),
    % timer:sleep(1000),
    Config1 = [{network_id, NetworkId}, {consensus, ?CONSENSUS_HC},
            {genesis_start_time, GenesisStartTime} | Config],
    % {ok, _} = produce_blocks(?NODE1, ?NODE1_NAME, child, 20, Config, ?config(consensus, Config1)),
    % ParentTopHeight = rpc(?PARENT_CHAIN_NODE1, aec_chain, top_height, []),
    % {ok, ParentBlocks} = get_generations(?PARENT_CHAIN_NODE1, 0, ParentTopHeight),
    % ct:log("Parent chain blocks ~p", [ParentBlocks]),
    % ChildTopHeight = rpc(?NODE1, aec_chain, top_height, []),
    % {ok, ChildBlocks} = get_generations(?NODE1, 0, ChildTopHeight),
    % ct:log("Child chain blocks ~p", [ChildBlocks]),
    Config1.


end_per_group(hc, Config) ->
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    aecore_suite_utils:stop_node(?PARENT_CHAIN_NODE1, Config);
end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    aect_test_utils:setup_testcase(Config),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, Consensus) ->
    node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, Consensus, false, 0).
    
node_config(NetworkId,Node, CTConfig, PotentialStakers, ReceiveAddress, Consensus, ProducingCommitments, GenesisStartTime) ->
    Stakers =
        case Consensus of
            ?CONSENSUS_POS ->
                lists:map(
                    fun(Who) ->
                        Pub = encoded_pubkey(Who),
                        Priv = list_to_binary(aeu_hex:bin_to_hex( privkey(Who))), %% TODO: discuss key management
                        #{<<"pub">> => Pub, <<"priv">> => Priv}
                    end,
                    PotentialStakers);
            ?CONSENSUS_HC ->
                lists:map(
                    fun({HCWho, PCWho}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))), %% TODO: discuss key management
                        PCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(PCWho))),
                        #{  <<"hyper_chain_account">> =>#{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv},
                            <<"parent_chain_account">> =>#{<<"pub">> => encoded_pubkey(PCWho), <<"priv">> => PCPriv}}
                    end,
                    PotentialStakers);
            _ when Consensus == ?CONSENSUS_HC_BTC; Consensus == ?CONSENSUS_HC_DOGE ->
                lists:map(
                    fun({HCWho, PCWho}) ->
                        HCPriv = list_to_binary(aeu_hex:bin_to_hex( privkey(HCWho))),
                        #{  <<"hyper_chain_account">> => #{<<"pub">> => encoded_pubkey(HCWho), <<"priv">> => HCPriv},
                            <<"parent_chain_account">> => #{<<"pub">> => PCWho} }
                    end,
                    PotentialStakers)
        end,
    ConsensusType =
        case Consensus of
            ?CONSENSUS_HC -> <<"hyper_chain">>;
            ?CONSENSUS_HC_BTC -> <<"hyper_chain">>;
            ?CONSENSUS_HC_DOGE -> <<"hyper_chain">>;
            ?CONSENSUS_POS -> <<"smart_contract">>
        end,
    SpecificConfig =
        case Consensus of
            ?CONSENSUS_POS -> #{};
            ?CONSENSUS_HC ->
                Port = aecore_suite_utils:external_api_port(?PARENT_CHAIN_NODE1),
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?CHILD_START_HEIGHT,
                        <<"confirmations">> => ?CHILD_CONFIRMATIONS,
                        <<"consensus">> =>
                            #{  <<"type">> => <<"AE2AE">>,
                                <<"network_id">> => ?PARENT_CHAIN_NETWORK_ID,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 100000000000000,
                                <<"amount">> => 9700
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"cache_size">> => 10,
                                <<"nodes">> => [ iolist_to_binary(io_lib:format("http://test:Pass@127.0.0.1:~p", [Port])) ]
                            },
                        <<"producing_commitments">> => ProducingCommitments
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"lazy_leader_trigger_time">> => ?LAZY_INTERVAL
                    };
            _ when Consensus == ?CONSENSUS_HC_BTC; Consensus == ?CONSENSUS_HC_DOGE ->
                PCType = case Consensus of
                            ?CONSENSUS_HC_BTC -> <<"AE2BTC">>;
                            ?CONSENSUS_HC_DOGE -> <<"AE2DOGE">>
                        end,
                #{  <<"parent_chain">> =>
                    #{  <<"start_height">> => ?CHILD_START_HEIGHT,
                        <<"confirmations">> => ?CHILD_CONFIRMATIONS,
                        <<"consensus">> =>
                            #{  <<"type">> => PCType,
                                <<"network_id">> => <<"regtest">>,
                                <<"spend_address">> => ReceiveAddress,
                                <<"fee">> => 95000,
                                <<"amount">> => 7500
                            },
                        <<"polling">> =>
                            #{  <<"fetch_interval">> => 100,
                                <<"nodes">> => [ iolist_to_binary(io_lib:format("http://test:Pass@127.0.0.1:~p", [?BTC_PARENT_CHAIN_PORT])) ]
                            },
                        <<"producing_commitments">> => ProducingCommitments
                        },
                    <<"genesis_start_time">> => GenesisStartTime,
                    <<"lazy_leader_trigger_time">> => ?LAZY_INTERVAL
                    }
        end,
    Protocol = aect_test_utils:latest_protocol_version(),
    {ok, ContractFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_contracts.json"),
    {ok, AccountFileName} = aecore_suite_utils:hard_fork_filename(Node, CTConfig, integer_to_list(Protocol), binary_to_list(NetworkId) ++ "_accounts.json"),
    #{<<"chain">> =>
            #{  <<"persist">> => false,
                <<"hard_forks">> => #{integer_to_binary(Protocol) => #{<<"height">> => 0,
                                                                        <<"contracts_file">> => ContractFileName,
                                                                        <<"accounts_file">> => AccountFileName}},
                <<"protocol_beneficiaries">> =>
                                [<<?BRI/binary,":109::">>],
                <<"consensus">> =>
                    #{<<"0">> => #{<<"type">> => ConsensusType,
                                <<"config">> =>
                                maps:merge(
                                    #{  <<"election_contract">> => aeser_api_encoder:encode(contract_pubkey, election_contract_address()),
                                        <<"rewards_contract">> => aeser_api_encoder:encode(contract_pubkey, staking_contract_address()),
                                        <<"contract_owner">> => aeser_api_encoder:encode(account_pubkey,?OWNER_PUBKEY),
                                        <<"expected_key_block_rate">> => 2000,
                                        <<"stakers">> => Stakers},
                                    SpecificConfig)
                                    }}},
        <<"fork_management">> =>
            #{<<"network_id">> => <<"this_will_be_overwritten_runtime">>},
        <<"logging">> => #{<<"level">> => <<"debug">>},
        <<"sync">> => #{<<"ping_interval">> => 5000},
        <<"mining">> =>
            #{<<"micro_block_cycle">> => 1,
            <<"autostart">> => true,
            %%<<"autostart">> => ProducingCommitments,
            <<"beneficiary_reward_delay">> => ?REWARD_DELAY
        }}.  %% this relies on certain nonce numbers


build_json_files(ElectionContract, NodeConfigs) ->
                Pubkey = ?OWNER_PUBKEY,
                {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
                ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),
                EncodePub =
                    fun(P) ->
                        binary_to_list(aeser_api_encoder:encode(account_pubkey, P))
                    end,
                %% create staking contract
                MinValidatorAmt = integer_to_list(trunc(math:pow(10,18) * math:pow(10, 6))), %% 1 mln AE
                MinStakeAmt = integer_to_list(trunc(math:pow(10,18) * 1)), %% 1 AE
                MinStakePercent = "30",
                OnlineDelay = "0",
                StakeDelay = "0",
                UnstakeDelay = "0",
                #{ <<"pubkey">> := StakingValidatorContract} = C0
                    = contract_create_spec("StakingValidator",
                                            [EncodePub(Pubkey), UnstakeDelay], 0, 1, Pubkey),
                {ok, ValidatorPoolAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                                          StakingValidatorContract),
                %% assert assumption
                ValidatorPoolAddress = validator_pool_contract_address(),
                #{ <<"pubkey">> := StakingContractPubkey
                    , <<"owner_pubkey">> := ContractOwner } = SC
                    = contract_create_spec(?STAKING_CONTRACT,
                                            [binary_to_list(StakingValidatorContract),
                                            MinValidatorAmt, MinStakePercent, MinStakeAmt,
                                            OnlineDelay, StakeDelay, UnstakeDelay],
                                            0, 2, Pubkey),
                {ok, StakingAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                                     StakingContractPubkey),
                %% assert assumption
                StakingAddress = staking_contract_address(),
                %% create election contract
                #{ <<"pubkey">> := ElectionContractPubkey
                    , <<"owner_pubkey">> := ContractOwner } = EC
                    = contract_create_spec(ElectionContract,
                                            [binary_to_list(StakingContractPubkey),
                                            "\"domat\""], 0, 3, Pubkey),
                {ok, ElectionAddress} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                                      ElectionContractPubkey),
                %% assert assumption
                ElectionAddress = election_contract_address(),
                {ok, SCId} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                            StakingContractPubkey),
                Call1 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "new_validator", [],
                                        ?INITIAL_STAKE, pubkey(?ALICE), 1),
                Call2 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "new_validator", [],
                                        ?INITIAL_STAKE, pubkey(?BOB), 1),
                Call3 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "new_validator", [],
                                        ?INITIAL_STAKE, pubkey(?LISA), 1),
                Call4  =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_online", [], 0, pubkey(?ALICE), 2),
                Call5  =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_online", [], 0, pubkey(?BOB), 2),
                Call6 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_online", [], 0, pubkey(?LISA), 2),
                Call7 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_name", ["\"Alice\""], 0, pubkey(?ALICE), 3),
                Call8 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_name", ["\"Bob\""], 0, pubkey(?BOB), 3),
                Call9 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_name", ["\"Lisa\""], 0, pubkey(?LISA), 3),
                Call10 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_description",
                                        ["\"Alice is a really awesome validator and she had set a description of her great service to the §work.\""], 0,
                                        pubkey(?ALICE), 4),
                Call11 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_avatar_url",
                                        ["\"https://aeternity.com/images/aeternity-logo.svg\""], 0,
                                        pubkey(?ALICE), 5),
                %% create a BRI validator in the contract so they can receive
                %% rewards as well
                %% TODO: discuss how we want to tackle this:
                %%  A) require the BRI account to be validator
                %%  B) allow pending stake in the contract that is not allocated
                %%  yet
                %%  C) something else
                {ok, BRIPub} = aeser_api_encoder:safe_decode(account_pubkey, ?BRI),
                Call12 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "new_validator", [],
                                        ?INITIAL_STAKE, BRIPub, 1),
                Call13 =
                    contract_call_spec(SCId, ?STAKING_CONTRACT,
                                        "set_validator_description",
                                        ["\"This validator is offline. She can never become a leader. She has no name set. She is receiving the BRI rewards\""],
                                        0, BRIPub, 2),
                %% keep the BRI offline
                AllCalls =  [Call1, Call2, Call3, Call4, Call5, Call6,
                     Call7, Call8, Call9, Call10, Call11, Call12, Call13],
                ProtocolBin = integer_to_binary(aect_test_utils:latest_protocol_version()),
                ContractsFileNames = [ContractsFileName  || #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"contracts_file">> := ContractsFileName}}}} <- NodeConfigs],
                AccountsFileNames = [AccountsFileName  || #{<<"chain">> := #{<<"hard_forks">> := #{ProtocolBin := #{<<"accounts_file">> := AccountsFileName}}}} <- NodeConfigs],
                aecore_suite_utils:create_seed_file(ContractsFileNames,
                    #{<<"contracts">> => [C0, SC, EC], <<"calls">> => AllCalls}),
                aecore_suite_utils:create_seed_file(AccountsFileNames,
                    #{  <<"ak_2evAxTKozswMyw9kXkvjJt3MbomCR1nLrf91BduXKdJLrvaaZt">> => 1000000000000000000000000000000000000000000000000,
                        encoded_pubkey(?ALICE) => 2100000000000000000000000000,
                        encoded_pubkey(?BOB) => 3100000000000000000000000000,
                        encoded_pubkey(?LISA) => 4100000000000000000000000000,
                        ?BRI => 2000000000000000000000000000
                     }),
                ok.
            
validator_pool_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 1).

staking_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 2).

election_contract_address() ->
    aect_contracts:compute_contract_pubkey(?OWNER_PUBKEY, 3).

election_contract_by_consensus(?CONSENSUS_HC) -> ?HC_ELECTION_CONTRACT.

contract_create_spec(Name, Args, Amount, Nonce, Owner) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey   = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner    = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode     = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{ <<"amount">> => Amount
            , <<"vm_version">> => VM
            , <<"abi_version">> => ABI
            , <<"nonce">> => Nonce
            , <<"code">> => EncodedCode
            , <<"call_data">> => EncodedCallData
            , <<"pubkey">> => EncodedPubkey
            , <<"owner_pubkey">> => EncodedOwner },
    Spec.

contract_call_spec(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Name, Fun, Args,
                                           Amount, From, Nonce)),
    %% Don't allow named contracts!?
    {contract, ContractPubKey} =
        aeser_id:specialize(aect_call_tx:contract_id(CallTx)),
    Spec =
        #{  <<"caller">>          => aeser_api_encoder:encode(account_pubkey,
                                                              aect_call_tx:caller_pubkey(CallTx))
          , <<"nonce">>           => aect_call_tx:nonce(CallTx)
          , <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
          , <<"abi_version">>     => aect_call_tx:abi_version(CallTx)
          , <<"fee">>             => aect_call_tx:fee(CallTx)
          , <<"amount">>          => aect_call_tx:amount(CallTx)
          , <<"gas">>             => aect_call_tx:gas(CallTx)
          , <<"gas_price">>       => aect_call_tx:gas_price(CallTx)
          , <<"call_data">>       => aeser_api_encoder:encode(contract_bytearray,
                                                              aect_call_tx:call_data(CallTx))},
    Spec.

contract_call(ContractPubkey, Name, Fun, Args, Amount, From) ->
    Nonce = next_nonce(?NODE1, From), %% no contract calls support for parent chain
    contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{  caller_id   => aeser_id:create(account, From)
          , nonce       => Nonce
          , contract_id => aeser_id:create(contract, ContractPubkey)
          , abi_version => ABI
          , fee         => 1000000 * ?DEFAULT_GAS_PRICE
          , amount      => Amount
          , gas         => 1000000
          , gas_price   => ?DEFAULT_GAS_PRICE
          , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.


produce_blocks(_Node, NodeName, parent = _NodeType, BlocksCnt, _Config, _Consensus) ->
    {ok, _} = aecore_suite_utils:mine_key_blocks(NodeName, BlocksCnt);
produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config, Consensus) ->
    produce_blocks(Node, NodeName, NodeType, BlocksCnt, Config, Consensus, correct_leader).

produce_blocks(Node, NodeName, child = _NodeType, BlocksCnt, _Config, ?CONSENSUS_HC, HCType) ->
    TopHeight0 = rpc(Node, aec_chain, top_height, []),
    produce_blocks_hc(Node, NodeName, BlocksCnt, HCType),
    TopHeight = rpc(Node, aec_chain, top_height, []),
    get_generations(Node, TopHeight0 + 1, TopHeight).

    get_generations(Node, FromHeight, ToHeight) ->
    ReversedBlocks =
        lists:foldl(
            fun(Height, Accum) ->
                case rpc(Node, aec_chain, get_generation_by_height, [Height, forward]) of
                    {ok, #{key_block := KB, micro_blocks := MBs}} ->
                        ReversedGeneration = lists:reverse(MBs) ++ [KB],
                        ReversedGeneration ++ Accum;
                    error -> error({failed_to_fetch_generation, Height})
                end
            end,
            [],
            lists:seq(FromHeight, ToHeight)),
    {ok, lists:reverse(ReversedBlocks)}.

produce_blocks_hc(_Node, _NodeName, BlocksCnt, _LeaderType) when BlocksCnt < 1 ->
    ok;
produce_blocks_hc(Node, NodeName, BlocksCnt, LeaderType) ->
    ParentNode = ?PARENT_CHAIN_NODE1,
    ParentNodeName = ?PARENT_CHAIN_NODE1_NAME,
    %% make sure the parent chain is not mining
    stopped = rpc:call(ParentNodeName, aec_conductor, get_mining_state, []),
    %% initial child chain state
    TopHeight = rpc(Node, aec_chain, top_height, []),
    ct:log("Producing a block with height ~p", [TopHeight + 1]),
    %% mine a single block on the parent chain
    case LeaderType of
        % lazy_leader ->
        %     {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
        %     {ok, [KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
        %     ct:log("Parent block mined ~p", [KB]),
        %     ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, ?LAZY_INTERVAL + 5000),
        %     CTop = rpc(Node, aec_chain, top_block, []),
        %     true = is_keyblock_lazy(CTop),
        %     ok;
        % abnormal_commitments_cnt ->
        %     {ok, _} = wait_for_at_least_commitments_in_pool(ParentNode, Node, 2),
        %     {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
        %     {ok, [KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
        %     ct:log("Parent block mined ~p", [KB]),
        %     ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
        %     CTop = rpc(Node, aec_chain, top_block, []),
        %     false = is_keyblock_lazy(CTop),
        %     ok;
        correct_leader ->
            % {ok, _} = wait_for_commitments_in_pool(ParentNode, Node, 2),
            % {ok, _} = aecore_suite_utils:mine_micro_block_emptying_mempool_or_fail(ParentNodeName),
            {ok, [_KB]} = aecore_suite_utils:mine_key_blocks(ParentNodeName, 1),
            % ct:log("Parent block mined ~p", [KB]),
            ok = aecore_suite_utils:wait_for_height(NodeName, TopHeight + 1, 10000), %% 10s per block
            CTop = rpc(Node, aec_chain, top_block, []),
            % false = is_keyblock_lazy(CTop),
            ok
    end,
    %% wait for the child to catch up
    produce_blocks_hc(Node, NodeName, BlocksCnt - 1, LeaderType).

wait_for_commitments_in_pool(Node, CNode, Cnt) ->
    wait_for_commitments_in_pool_(Node, CNode, fun(Pool) ->
                                                    TxsCnt = length(Pool),
                                                    Res = TxsCnt =:= Cnt,
                                                    case Res of
                                                        true ->
                                                            validate_expected_commitments(CNode, Pool);
                                                        false ->
                                                            pass
                                                    end,
                                                    Res
                                                end).

wait_for_at_least_commitments_in_pool(Node, CNode, Cnt) ->
    wait_for_commitments_in_pool_(Node, CNode, fun(Pool) ->
                                                    TxsCnt = length(Pool),
                                                    TxsCnt >= Cnt
                                                end).


wait_for_commitments_in_pool_(Node, ChildNode, CompareFun) ->
    wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, 100).

wait_for_commitments_in_pool_(Node, _ChildNode, CompareFun, Attempts) when Attempts < 1 ->
    {ok, Pool} = rpc(Node, aec_tx_pool, peek, [infinity]),
    case CompareFun(Pool) of
        true ->
            {ok, Pool};
        false ->
            error({run_out_of_attempts, length(Pool)})
    end;
wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, Attempts) ->
    TopHeader = rpc(Node, aec_chain, top_header, []),
    {ok, TopHash} = aec_headers:hash_header(TopHeader),
    TopHeight = aec_headers:height(TopHeader),
    {ok, ChildTopBlock} = rpc(ChildNode, aec_chain, top_key_block, []),
    {ok, CTopHash} = aec_blocks:hash_internal_representation(ChildTopBlock),
    CTopHeight = aec_blocks:height(ChildTopBlock),
    {ok, Pool} = rpc(Node, aec_tx_pool, peek, [infinity]),
    ct:log("Parent Height ~p, hash ~p, ~p commitments in pool ~p",
            [TopHeight, aeser_api_encoder:encode(key_block_hash, TopHash),
            length(Pool), Pool]),
    ct:log("Child Height ~p, hash ~p",
            [CTopHeight, aeser_api_encoder:encode(key_block_hash, CTopHash)]),
    case CompareFun(Pool) of
        true ->
            {ok, Pool};
        false ->
            timer:sleep(30),
            wait_for_commitments_in_pool_(Node, ChildNode, CompareFun, Attempts - 1)
    end.


validate_expected_commitments(Node, Commitments) ->
    TopH = rpc(Node, aec_chain, top_key_block_hash, []),
    NetworkId = rpc(Node, aec_governance, get_network_id, []),
    ExpectedCommitments =
        lists:map(
            fun(Staker) ->
                rpc(?NODE1, aec_parent_chain_block, encode_commitment_btc, [pubkey(Staker), TopH, NetworkId])
            end,
            [?ALICE, ?BOB]),
    ct:log("Child chain top hashes ~p", [ExpectedCommitments]),
    case lists:all(fun(SignedTx) ->
                        {spend_tx, SpendTx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
                        ct:log("Spend payload ~p", [aec_spend_tx:payload(SpendTx)]),
                        lists:member(aec_spend_tx:payload(SpendTx), ExpectedCommitments)
                    end,
                    Commitments) of
        true -> ok;
        false -> error(commitments_mismatch)
    end.

get_commitments(From, To) ->
    {ok, Blocks} = get_generations(?PARENT_CHAIN_NODE1, From, To + 1),
    MicroBlocks =
        lists:filter(fun(B) -> aec_blocks:type(B) =:= micro end, Blocks),
    maps:from_list(
        lists:map(
            fun(MB) -> {aec_blocks:height(MB), aec_blocks:txs(MB)} end,
            MicroBlocks)).


is_keyblock_lazy(KB) ->
    ct:log("Inspecting block ~p", [KB]),
    0 =:= aec_blocks:difficulty(KB).

wait_same_top() ->
    wait_same_top(?NODE1, ?NODE2).

wait_same_top(Node1, Node2) ->
    wait_same_top(Node1, Node2, 500).

wait_same_top(_Node1, _Node2, Attempts) when Attempts < 1 ->
    {error, run_out_of_attempts};
wait_same_top(Node1, Node2, Attempts) ->
    case {rpc(Node1, aec_chain, top_block, []), rpc(Node2, aec_chain, top_block, [])} of
        {KB, KB} -> {ok, KB};
        {KB1, KB2} ->
            ct:log("Node1 top: ~p\nNode2 top: ~p", [KB1, KB2]),
            timer:sleep(500),
            wait_same_top(Node1, Node2, Attempts - 1)
    end.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

pubkey({Pubkey, _, _}) -> Pubkey.

privkey({_, Privkey, _}) -> Privkey.

name({_, _, Name}) -> Name.


next_nonce(Node, Pubkey) ->
    case rpc(Node, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.


agent_write_parent_chain(_Config) ->
    Hash = "pinning_hash",
    ok = aec_parent_connector:pin_hash(Hash),
    ok.
