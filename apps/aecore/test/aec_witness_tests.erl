%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_witness_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(TEST_MODULE, aec_spend_tx).
-define(SENDER_PUBKEY, <<"_________sender_pubkey__________">>).
-define(SENDER_ID, aeser_id:create(account, ?SENDER_PUBKEY)).
-define(RECIPIENT_PUBKEY, <<"________recipient_pubkey________">>).
-define(RECIPIENT_ID, aeser_id:create(account, ?RECIPIENT_PUBKEY)).
-define(ORACLE_QUERYER_PUBKEY, <<"______oracle_queryer_pubkey______">>).

process_test_() ->
    {setup,
     fun() ->
        ok = meck:new(aec_governance, [passthrough]),
        meck:expect(aec_governance, minimum_gas_price, 1, 1)
     end,
     fun(_) -> meck:unload(aec_governance) end,
     [{"Check witness for valid spend tx",
       fun() ->
          SenderAccount =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 1000000,
                            nonce => 10}),
          RecipientAccount =
              new_account(#{pubkey => ?RECIPIENT_PUBKEY,
                            balance => 80,
                            nonce => 12}),
          StateTree0 =
              aec_test_utils:create_state_tree_with_accounts([SenderAccount, RecipientAccount]),

          {ok, SpendTx} =
              ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                 recipient_id => ?RECIPIENT_ID,
                                 amount => 50,
                                 fee => 20000,
                                 nonce => 11,
                                 payload => <<"foo">>}),
          <<"foo">> =
              aec_spend_tx:payload(
                  aetx:tx(SpendTx)),
          Env = aetx_env:tx_env(20),
          {ok, StateTree, Env1} = aetx:process(SpendTx, StateTree0, Env),

          Witness = aetx_env:witness(Env1),

          ?assertEqual([?SENDER_PUBKEY, ?RECIPIENT_PUBKEY],
                       lists:sort(
                           aec_witness:accounts(Witness))),

          ResultAccountsTree = aec_trees:accounts(StateTree),
          {value, ResultSenderAccount} =
              aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),
          {value, ResultRecipientAccount} =
              aec_accounts_trees:lookup(?RECIPIENT_PUBKEY, ResultAccountsTree),

          ?assertEqual(1000000 - 50 - 20000, aec_accounts:balance(ResultSenderAccount)),
          ?assertEqual(11, aec_accounts:nonce(ResultSenderAccount)),
          ?assertEqual(80 + 50, aec_accounts:balance(ResultRecipientAccount)),
          ?assertEqual(12, aec_accounts:nonce(ResultRecipientAccount))
       end},
      {"Check spend to oneself witness",
       fun() ->
          SenderAccount =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 1000000,
                            nonce => 10}),
          StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

          {ok, SpendTx} =
              ?TEST_MODULE:new(#{sender_id => ?SENDER_ID,
                                 recipient_id => ?SENDER_ID,
                                 amount => 50,
                                 fee => 20000,
                                 nonce => 11,
                                 payload => <<"foo">>}),
          Env = aetx_env:tx_env(20),
          {ok, StateTree, Env1} = aetx:process(SpendTx, StateTree0, Env),

          Witness = aetx_env:witness(Env1),

          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(Witness)),

          ResultAccountsTree = aec_trees:accounts(StateTree),
          {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

          ?assertEqual(1000000 - 50 - 20000 + 50, aec_accounts:balance(ResultAccount)),
          ?assertEqual(11, aec_accounts:nonce(ResultAccount))
       end},
      {"Check oracle witness",
       fun() ->
          Account =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 1000000,
                            nonce => 10}),
          RecipientAccount =
              new_account(#{pubkey => ?RECIPIENT_PUBKEY,
                            balance => 800000,
                            nonce => 12}),
          StateTree0 = aec_test_utils:create_state_tree_with_accounts([Account, RecipientAccount]),

          %% 1. Create an Oracle
          {ok, OracleTx} =
              aeo_register_tx:new(#{account_id => ?SENDER_ID,
                                    nonce => 11,
                                    query_format => <<>>,
                                    abi_version => ?ABI_NO_VM,
                                    response_format => <<>>,
                                    query_fee => 20000,
                                    oracle_ttl => {delta, 100},
                                    fee => 100000}),
          Env = aetx_env:tx_env(20),
          {ok, StateTree, Env1} = aetx:process(OracleTx, StateTree0, Env),

          Witness = aetx_env:witness(Env1),

          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(Witness)),
          ?assertEqual([?SENDER_PUBKEY], aec_witness:oracles(Witness)),

          ResultAccountsTree = aec_trees:accounts(StateTree),
          {value, ResultAccount} = aec_accounts_trees:lookup(?SENDER_PUBKEY, ResultAccountsTree),

          ?assertEqual(1000000 - 100000, aec_accounts:balance(ResultAccount)),
          ?assertEqual(11, aec_accounts:nonce(ResultAccount)),

          %% 2. Create an Oracle Query
          %% We now have an oracle in our state tree. Create a Tx that queries
          %% it. This should create a state tree write for the new query,
          %% plus a state tree read of the oracle itself.
          %% The witness should include both.
          {value, Oracle} =
              aeo_state_tree:lookup_oracle(?SENDER_PUBKEY, aec_trees:oracles(StateTree)),
          OracleId = aeo_oracles:id(Oracle),

          {ok, OracleQueryTx} =
              aeo_query_tx:new(#{sender_id => ?RECIPIENT_ID,
                                 nonce => 13,
                                 oracle_id => OracleId,
                                 query => <<>>,
                                 query_fee => 20000,
                                 query_ttl => {delta, 1},
                                 response_ttl => {delta, 1},
                                 fee => 100000}),

          EnvQ = aetx_env:tx_env(20),
          %% First a sanity check that our witness is really back to empty
          ?assertEqual([],
                       aec_witness:accounts(
                           aetx_env:witness(EnvQ))),

          {ok, StateTree1, EnvQ1} = aetx:process(OracleQueryTx, StateTree, EnvQ),

          WitnessQ = aetx_env:witness(EnvQ1),
          ?assertEqual([?RECIPIENT_PUBKEY], aec_witness:accounts(WitnessQ)),
          Sender = ?SENDER_PUBKEY,
          [Sender, <<Sender:32/binary, QueryKey:32/binary>>] =
              lists:sort(
                  aec_witness:oracles(WitnessQ)),

          %% 3. Extend the Oracle
          {ok, OracleExtendTx} =
              aeo_extend_tx:new(#{oracle_id => OracleId,
                                  nonce => 12,
                                  oracle_ttl => {delta, 200},
                                  fee => 200000}),
          EnvEQ = aetx_env:tx_env(30),

          ?assertEqual([],
                       aec_witness:accounts(
                           aetx_env:witness(EnvEQ))),

          {ok, StateTree2, EnvEQ1} = aetx:process(OracleExtendTx, StateTree1, EnvEQ),
          WitnessEQ = aetx_env:witness(EnvEQ1),
          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(WitnessEQ)),
          ?assertMatch([?SENDER_PUBKEY], aec_witness:oracles(WitnessEQ)),

          %% 4. Respond to the earlier defined Query
          QueryId =
              aeo_query_tx:query_id(
                  aetx:tx(OracleQueryTx)),

          {ok, ResponseTx} =
              aeo_response_tx:new(#{oracle_id => OracleId,
                                    nonce => 13,
                                    query_id => QueryId,
                                    response => <<"">>,
                                    response_ttl => {delta, 1},
                                    fee => 200000}),
          EnvQR = aetx_env:tx_env(32),

          ?assertEqual([],
                       aec_witness:accounts(
                           aetx_env:witness(EnvQR))),

          {ok, _StateTree3, EnvQR1} = aetx:process(ResponseTx, StateTree2, EnvQR),
          WitnessQR = aetx_env:witness(EnvQR1),
          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(WitnessQR)),
          ?assertMatch([?SENDER_PUBKEY, <<Sender:32/binary, QueryKey:32/binary>>],
                       aec_witness:oracles(WitnessQR))
       end},
      {"Check name service auction witness",
       fun() ->
          SenderAccount =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 4000000000000000000000 * aec_test_utils:min_gas_price(),
                            nonce => 10}),
          StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

          NameSalt = 8999,
          {ok, NameAscii} = aens_utils:to_ascii(<<"claimedname.chain">>),
          CHash = aens_hash:commitment_hash(NameAscii, NameSalt),

          {ok, PreclaimTx} =
              aens_preclaim_tx:new(#{account_id => ?SENDER_ID,
                                     commitment_id => aeser_id:create(commitment, CHash),
                                     fee => 20000,
                                     nonce => 11}),
          Env = aetx_env:tx_env(20),

          {ok, StateTree, Env1} = aetx:process(PreclaimTx, StateTree0, Env),

          %% Check commitment present
          {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(StateTree)),
          ?assertEqual(CHash, aens_commitments:hash(C)),

          Witness = aetx_env:witness(Env1),

          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(Witness)),
          ?assertEqual([CHash], aec_witness:ns(Witness)),

          %% Next up - Claim Tx
          NameFee = aec_governance:name_claim_fee(NameAscii, ?LIMA_PROTOCOL_VSN),
          {ok, ClaimTx} =
              aens_claim_tx:new(#{account_id => ?SENDER_ID,
                                  nonce => 12,
                                  name => NameAscii,
                                  name_salt => NameSalt,
                                  name_fee => NameFee,
                                  fee => 20000,
                                  ttl => 0}),
          EnvC = aetx_env:tx_env(21),

          {ok, StateTree1, EnvC1} = aetx:process(ClaimTx, StateTree, EnvC),

          %% Check commitment removed and name entry not present and auction entry added
          NHash = aens_hash:name_hash(NameAscii),
          AuctionHash = aens_hash:to_auction_hash(NHash),
          none = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(StateTree1)),
          none = aens_state_tree:lookup_name(NHash, aec_trees:ns(StateTree1)),
          {value, _A} = aens_state_tree:lookup_name_auction(AuctionHash, aec_trees:ns(StateTree1)),
          WitnessC = aetx_env:witness(EnvC1),
          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(WitnessC)),
          ?assertEqual([AuctionHash], aec_witness:ns(WitnessC))
       end},
      {"Check name service witness",
       fun() ->
          SenderAccount =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 4000000000000000000000 * aec_test_utils:min_gas_price(),
                            nonce => 10}),
          StateTree0 = aec_test_utils:create_state_tree_with_accounts([SenderAccount]),

          NameSalt = 8999,
          Name = gen_name(aec_governance:name_max_length_starting_auction() + 1),
          {ok, NameAscii} =
              aens_utils:to_ascii(
                  aens_test_utils:fullname(Name)),
          CHash = aens_hash:commitment_hash(NameAscii, NameSalt),

          {ok, PreclaimTx} =
              aens_preclaim_tx:new(#{account_id => ?SENDER_ID,
                                     commitment_id => aeser_id:create(commitment, CHash),
                                     fee => 20000,
                                     nonce => 11}),
          Env = aetx_env:tx_env(20),

          {ok, StateTree, Env1} = aetx:process(PreclaimTx, StateTree0, Env),

          %% Check commitment present
          {value, C} = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(StateTree)),
          ?assertEqual(CHash, aens_commitments:hash(C)),

          Witness = aetx_env:witness(Env1),

          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(Witness)),
          ?assertEqual([CHash], aec_witness:ns(Witness)),

          %% Next up - Claim Tx
          NameFee = aec_governance:name_claim_fee(NameAscii, ?LIMA_PROTOCOL_VSN),
          {ok, ClaimTx} =
              aens_claim_tx:new(#{account_id => ?SENDER_ID,
                                  nonce => 12,
                                  name => NameAscii,
                                  name_salt => NameSalt,
                                  name_fee => NameFee,
                                  fee => 20000,
                                  ttl => 0}),
          EnvC = aetx_env:tx_env(21),

          {ok, StateTree1, EnvC1} = aetx:process(ClaimTx, StateTree, EnvC),

          %% Check commitment removed and name entry added
          NHash = aens_hash:name_hash(NameAscii),
          none = aens_state_tree:lookup_commitment(CHash, aec_trees:ns(StateTree1)),
          {value, _N} = aens_state_tree:lookup_name(NHash, aec_trees:ns(StateTree1)),
          WitnessC = aetx_env:witness(EnvC1),
          %% We also create / write to a locked coins account
          LockedCoinsAccount = aec_governance:locked_coins_holder_account(),
          ?assertEqual([LockedCoinsAccount, ?SENDER_PUBKEY], aec_witness:accounts(WitnessC)),
          ?assertEqual([NHash], aec_witness:ns(WitnessC))
       end},
      {"Check witness for contract call",
       fun() ->
          OwnerAccount =
              new_account(#{pubkey => ?SENDER_PUBKEY,
                            balance => 1000000000 * 10000000000,
                            nonce => 10}),
          CallerAccount =
              new_account(#{pubkey => ?RECIPIENT_PUBKEY,
                            balance => 1000000000 * 10000000000,
                            nonce => 12}),
          StateTree0 =
              aec_test_utils:create_state_tree_with_accounts([OwnerAccount, CallerAccount]),

          {ok, IdContract} = compile_contract(witness),
          CallData = aefa_test_utils:make_calldata(init, {}),

          {ok, Tx} =
              aect_create_tx:new(#{owner_id => ?SENDER_ID,
                                   nonce => 11,
                                   code => IdContract,
                                   vm_version => aect_test_utils:latest_sophia_vm_version(),
                                   abi_version => aect_test_utils:latest_sophia_abi_version(),
                                   deposit => 10,
                                   amount => 200,
                                   gas => 1000,
                                   gas_price => aec_test_utils:min_gas_price(),
                                   call_data => CallData,
                                   fee => aec_test_utils:min_gas_price()}),
          ContractKey = aect_contracts:compute_contract_pubkey(?SENDER_PUBKEY, aetx:nonce(Tx)),
          InitCallId = aect_call:id(?SENDER_PUBKEY, aetx:nonce(Tx), ContractKey),
          Env = aetx_env:tx_env(20),
          {ok, StateTree, Env1} = aetx:process(Tx, StateTree0, Env),

          io:format(user, "Contract Key = ~p~n", [ContractKey]),

          dump_keys(StateTree),

          Witness = aetx_env:witness(Env1),
          ?assertEqual([?SENDER_PUBKEY], aec_witness:accounts(Witness)),
          CallKey = <<ContractKey/binary, InitCallId/binary>>,
          ?assertEqual([CallKey], aec_witness:calls(Witness)),

          CallDataUpdate = aefa_test_utils:make_calldata(update, {}),
          {ok, CallTx} =
              aect_call_tx:new(#{caller_id => ?RECIPIENT_ID,
                                 nonce => 13,
                                 contract_id => aeser_id:create(contract, ContractKey),
                                 abi_version => aect_test_utils:latest_sophia_abi_version(),
                                 fee => aec_test_utils:min_gas_price(),
                                 amount => 300,
                                 gas => 100000000,
                                 gas_price => aec_test_utils:min_gas_price(),
                                 call_data => CallDataUpdate}),
          EnvC = aetx_env:tx_env(22),
          {ok, StateTree1, Env2} = aetx:process(CallTx, StateTree, EnvC)
       end}]}.

new_account(Map) ->
    aec_accounts:set_nonce(
        aec_accounts:new(
            maps:get(pubkey, Map), maps:get(balance, Map, 0)),
        maps:get(nonce, Map, 0)).

gen_name(Length) ->
    << <<"x">> || _ <- lists:seq(1, Length) >>.

compile_contract(Name) ->
    Vsn = aect_test_utils:latest_sophia_version(),
    aect_test_utils:compile_contract(Vsn, Name).

dump_keys(StateTree) ->
    L = aect_state_tree:to_list(
            aec_trees:contracts(StateTree)),
    Keys = lists:map(fun({K, _}) -> K end, L),
    io:format(user, "Contracts Keys = ~p~n", [Keys]),
    L1 = aect_call_state_tree:to_list(
             aec_trees:calls(StateTree)),
    Keys1 = lists:map(fun({K, _}) -> K end, L1),
    io:format(user, "Calls Keys = ~p~n", [Keys1]),
    L2 = aec_accounts_trees:get_all_accounts_balances(
             aec_trees:accounts(StateTree)),
    io:format(user, "Accounts Keys = ~p~n", [L2]).
