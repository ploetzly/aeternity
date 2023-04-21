%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity Foundation
%%% @doc set implementation using maps as underlying mechanism
%%%      Note: from OTP-24 the stdlib sets module can be map based
%%%            so we could remove this eventually.
%%% @end
%%%-------------------------------------------------------------------
-module(aeu_mapset).

-export([new/0, from_list/1, insert/2, delete/2, to_list/1, is_member/2, merge/2]).

-export_type([mapset/0, mapset/1]).

-define(DUMMY, []).

-type mapset() :: mapset(_).
-opaque mapset(Element) :: #{Element => ?DUMMY}.

-spec new() -> mapset().
new() -> maps:new().

-spec from_list(List) -> Set when
      List :: [Element],
      Set :: mapset(Element).
from_list(List) when is_list(List) ->
    maps:from_keys(List, ?DUMMY).

-spec insert(Element, MapSet1) -> MapSet2 when
      MapSet1 :: mapset(Element),
      MapSet2 :: mapset(Element).
insert(Element, MapSet) ->
    maps:put(Element, ?DUMMY, MapSet).

-spec delete(Element, MapSet1) -> MapSet2 when
      MapSet1 :: mapset(Element),
      MapSet2 :: mapset(Element).
delete(Element, MapSet) ->
    maps:remove(Element, MapSet).

-spec to_list(MapSet) -> List when
      MapSet :: mapset(Element),
      List :: [Element].
to_list(MapSet) ->
    maps:keys(MapSet).

-spec is_member(Element, MapSet) -> boolean() when
      MapSet :: mapset(Element).
is_member(Val, MapSet) ->
    maps:is_key(Val, MapSet).

-spec merge(MapSet1, MapSet2) -> MapSet3 when
      MapSet1 :: mapset(Element),
      MapSet2 :: mapset(Element),
      MapSet3 :: mapset(Element).
merge(MapSet1, MapSet2) ->
    maps:merge(MapSet1, MapSet2).


