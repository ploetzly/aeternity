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
-export([start_link/2, stop/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([post_block/1,
         get_block_by_height/1]).

-export([get_state/0]).

-record(state,
    {
        start_height            :: non_neg_integer(),
        max_size                :: non_neg_integer(),
        blocks          = #{}   :: #{non_neg_integer() => aec_parent_chain_block:block()},
        top_height      = 0     :: non_neg_integer()
    }).
-type state() :: #state{}.

-define(SERVER, ?MODULE).


%%%=============================================================================
%%% API
%%%=============================================================================
%% Start the parent chain cache process
-spec start_link(non_neg_integer(), non_neg_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Height, Size) ->
    Args = [Height, Size],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?SERVER).

-spec post_block(aec_parent_chain_block:block()) -> ok.
post_block(Block) ->
    gen_server:cast(?SERVER, {post_block, Block}).

-spec get_block_by_height(non_neg_integer()) -> {ok, aec_parent_chain_block:block()}
                                              | {error, not_in_cache}.
get_block_by_height(Height) ->
    gen_server:call(?SERVER, {get_block_by_height, Height}).

-spec get_state() -> {ok, map()}.
get_state() ->
    gen_server:call(?SERVER, get_state).



%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init([any()]) -> {ok, #state{}}.
init([StartHeight, Size]) ->
    {ok, #state{start_height    = StartHeight,
                max_size        = Size,
                blocks          = #{}}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({get_block_by_height, Height}, _From, State) ->
    Reply = get_block(Height, State),
%    case Reply of
%        {error, _} ->
%            aec_parent_connector:fetch_block_by_height(Height);
%        {ok, _} -> pass
%    end,
    {reply, Reply, State};
handle_call(get_state, _From, State) ->
    Reply = {ok, state_to_map(State)},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({post_block, Block}, #state{top_height = TopHeight,
                                        max_size = MaxSize } = State0) ->
    %% the block received might be the top one or a previous one; we try GCing
    %% older blocks according to the top block only;
    %% if the previous block is missing, fetch it (if above the GC height)
    GCHeight = max(TopHeight - MaxSize, -1),
    BlockHeight = aec_parent_chain_block:height(Block),
    State1 =
        case BlockHeight > GCHeight of
            true ->
                case BlockHeight > TopHeight + MaxSize of
                    true -> 
                        %% we received a block far from the future, so we have
                        %% to GC all blocks
                        insert_block(Block, State0#state{blocks = #{}});
                    false ->
                        insert_block(Block, State0)
                end;
            false -> State0
        end,
    TryGCHeight = BlockHeight - MaxSize,
    State2 =
        case TryGCHeight >= 0 of
            true ->
                delete_block(TryGCHeight, State1);
            false -> State1
        end,
    PrevHeight = BlockHeight - 1,
    case PrevHeight > GCHeight of
        false -> pass;
        true ->
            %% check if the previous block is missing
            case get_block(BlockHeight - 1, State2) of
                {ok, _} -> pass;
                {error, _} -> %% missing block detected
                    lager:debug("Missing block with height ~p detected, fetching it", [PrevHeight]),
                    aec_parent_connector:fetch_block_by_hash(aec_parent_chain_block:prev_hash(Block))
            end
    end,
    {noreply, State2#state{top_height = max(TopHeight, BlockHeight)}};
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
    State#state{blocks = maps:put(Height, Block, Blocks)}.

-spec get_block(non_neg_integer(), state()) -> {ok, aec_parent_chain_block:block()} | {error, not_in_cache}.
get_block(Height, #state{blocks = Blocks}) ->
    case maps:find(Height, Blocks) of
        {ok, _Block} = OK -> OK;
        error ->
            %% TODO: fetch the block
            {error, not_in_cache}
    end.
    
-spec delete_block(non_neg_integer(), state()) -> state().
delete_block(Height, #state{blocks = Blocks} = State) ->
    State#state{blocks = maps:remove(Height, Blocks)}.

state_to_map(#state{start_height = StartHeight,
                    max_size     = MaxSize,
                    blocks       = Blocks, 
                    top_height   = TopHeight}) ->
    #{  start_height => StartHeight,
        max_size     => MaxSize,
        blocks       => Blocks, 
        top_height   => TopHeight}.

