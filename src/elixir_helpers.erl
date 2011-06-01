% General helpers
-module(elixir_helpers).
-export([orddict_find/2, orddict_merge/2, empty_tuple/0]).
-include("elixir.hrl").

% Same as orddict:fetch() but returns nil instead of raising.
orddict_find(Key, [{K,_}|_]) when Key < K -> nil;
orddict_find(Key, [{K,_}|D]) when Key > K -> orddict_find(Key, D);
orddict_find(_Key, [{_K,Value}|_]) -> Value;
orddict_find(_, []) -> nil.

% Same as orddict:merge() but function defaults to return V2.
orddict_merge([{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 < K2 ->
    [E1|orddict_merge(D1, [E2|D2])];
orddict_merge([{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 > K2 ->
    [E2|orddict_merge([E1|D1], D2)];
orddict_merge([{K1,V1}|D1], [{_K2,V2}|D2]) ->	%K1 == K2
    [{K1,V2}|orddict_merge(D1, D2)];
orddict_merge([], D2) -> D2;
orddict_merge(D1, []) -> D1.

empty_tuple() -> {}.