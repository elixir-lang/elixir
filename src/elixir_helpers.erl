% General helpers
-module(elixir_helpers).
-export([orddict_find/2, empty_tuple/0]).
-include("elixir.hrl").

orddict_find(Key, [{K,_}|_]) when Key < K -> nil;
orddict_find(Key, [{K,_}|D]) when Key > K -> orddict_find(Key, D);
orddict_find(_Key, [{_K,Value}|_]) -> Value;
orddict_find(_, []) -> nil.

empty_tuple() -> {}.