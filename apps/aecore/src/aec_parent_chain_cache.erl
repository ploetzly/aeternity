%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc
%%% Manage interaction with hyperchain parent chain
%%% @end
%%%-------------------------------------------------------------------

-module(aec_parent_chain_cache).

%% Functionality:
%% - cache the view of the parent chain's blocks. The `aec_parent_connector`
%% reports any new blocks and they are stored in a cache
%% - provides parent chain blocks to the consensus module on demand. If it
%% asks for an older block, it is being fetched as well. This would take some
%% more time as a couple of blocks are being queried
%% - cleans up older states
%% - keeps track of current child chain top and fetches releated blocks
%% - is fork aware
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/1, stop/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([post_block/1,
         get_block_by_height/1]).

-record(state,
    {
        start_height    = 0     :: non_neg_integer(),
        blocks          = #{}   :: #{non_neg_integer() => aec_parent_chain_block:block()}
    }).
-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(CACHE_SIZE, 2000).


%%%=============================================================================
%%% API
%%%=============================================================================
%% Start the parent chain cache process
-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Height) ->
    Args = [Height],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec post_block(aec_parent_chain_block:block()) -> ok.
post_block(Block) ->
    gen_server:cast(?SERVER, {post_block, Block}).

-spec get_block_by_height(non_neg_integer()) -> {ok, aec_parent_chain_block:block()}
                                              | {error, not_produced}.
get_block_by_height(Height) ->
    gen_server:call(?SERVER, {get_block_by_height, Height}).


%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([StartHeight]) ->
    {ok, #state{blocks = #{}}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({get_block_by_height, Height}, _From, State) ->
    Reply = get_block(Height, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({post_block, Block}, State0) ->
    State1 = insert_block(Block, State0),
    Height = aec_parent_chain_block:height(Block),
    State2 = delete_block(Height - ?CACHE_SIZE, State1),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(check_parent, #state{ } = State) ->
    {noreply, State#state{}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec insert_block(aec_parent_chain_block:block(), state()) -> state().
insert_block(Block, #state{blocks = Blocks} = State) ->
    Height = aec_parent_chain_block:height(Block),
    lager:info("ASDF INSERT BLOCK ~p ", [Block]),
    State#state{blocks = maps:put(Height, Block, Blocks)}.

-spec get_block(non_neg_integer(), state()) -> {ok, aec_parent_chain_block:block()} | {error, not_produced}.
get_block(Height, #state{blocks = Blocks} = State) ->
    lager:info("ASDF did NOT find a block ~p", [Height]),
    case maps:find(Height, Blocks) of
        {ok, _Block} = OK -> OK;
        error ->
            lager:debug("ASDF map ~p", [Blocks]),
            %% TODO: fetch the block
            {error, not_produced}
    end.
    
-spec delete_block(non_neg_integer(), state()) -> state().
delete_block(Height, #state{blocks = Blocks} = State) ->
    lager:info("ASDF DELETE BLOCK ~p ", [Height]),
    State#state{blocks = maps:remove(Height, Blocks)}.

