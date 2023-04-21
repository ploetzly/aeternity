%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Crypto Foundation
%%% @doc Block witness collection and storage. Tracks all entries read
%%% from the state trees during block processing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_witness).

-export([new/0, accounts/1, add_account/2, calls/1, add_call/2, channels/1, add_channel/2,
         contracts/1, add_contract/2, ns/1, add_ns/2, oracles/1, add_oracle/2]).

-record(witness,
        {accounts :: aeu_mapset:mapset(aec_keys:pubkey()),
         calls :: aeu_mapset:mapset(aect_call:id()),
         channels :: aeu_mapset:mapset(aesc_channels:pubkey()),
         contracts :: aeu_mapset:mapset(aect_contracts:pubkey()),
         ns :: aeu_mapset:mapset(binary()),
         oracles :: aeu_mapset:mapset(binary())}).

-type witness() :: #witness{}.

new() ->
    #witness{accounts = aeu_mapset:new(),
             calls = aeu_mapset:new(),
             channels = aeu_mapset:new(),
             contracts = aeu_mapset:new(),
             ns = aeu_mapset:new(),
             oracles = aeu_mapset:new()}.

-spec accounts(witness()) -> [aec_keys:pubkey()].
accounts(Witness) ->
    aeu_mapset:to_list(Witness#witness.accounts).

-spec add_account(witness(), aec_keys:pubkey()) -> witness().
add_account(Witness, Account) ->
    Current = Witness#witness.accounts,
    Witness#witness{accounts = aeu_mapset:insert(Account, Current)}.

-spec calls(witness()) -> [aec_keys:pubkey()].
calls(Witness) ->
    aeu_mapset:to_list(Witness#witness.calls).

-spec add_call(witness(), aec_keys:pubkey()) -> witness().
add_call(Witness, Call) ->
    Current = Witness#witness.calls,
    Witness#witness{calls = aeu_mapset:insert(Call, Current)}.

-spec channels(witness()) -> [aesc_channels:pubkey()].
channels(Witness) ->
    aeu_mapset:to_list(Witness#witness.channels).

-spec add_channel(witness(), aesc_channels:pubkey()) -> witness().
add_channel(Witness, Channel) ->
    Current = Witness#witness.channels,
    Witness#witness{channels = aeu_mapset:insert(Channel, Current)}.

-spec contracts(witness()) -> [aect_contracts:pubkey()].
contracts(Witness) ->
    aeu_mapset:to_list(Witness#witness.contracts).

-spec add_contract(witness(), aect_contracts:pubkey()) -> witness().
add_contract(Witness, Contract) ->
    Current = Witness#witness.contracts,
    Witness#witness{contracts = aeu_mapset:insert(Contract, Current)}.

-spec ns(witness()) -> [binary()].
ns(Witness) ->
    aeu_mapset:to_list(Witness#witness.ns).

-spec add_ns(witness(), binary()) -> witness().
add_ns(Witness, Ns) ->
    Current = Witness#witness.ns,
    Witness#witness{ns = aeu_mapset:insert(Ns, Current)}.

-spec oracles(witness()) -> [binary()].
oracles(Witness) ->
    aeu_mapset:to_list(Witness#witness.oracles).

-spec add_oracle(witness(), binary()) -> witness().
add_oracle(Witness, Oracle) ->
    Current = Witness#witness.oracles,
    Witness#witness{oracles = aeu_mapset:insert(Oracle, Current)}.
