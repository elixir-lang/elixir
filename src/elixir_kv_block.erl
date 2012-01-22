%% Helpers related to key-value blocks.
-module(elixir_kv_block).
-export([merge/2, merge/3, normalize/1, decouple/2, decouple/1]).
-include("elixir.hrl").

merge(Left, Right) -> merge(0, Left, Right).

%% Merge the given list of key-values on the left
%% to the orddict key-values on te right resulting
%% in an orddict.
merge(Line, Acc, [{Key,Value}|T]) ->
  NewAcc = orddict:update(Key, fun(Old) -> merge_each(Line, Old, Value) end, Value, Acc),
  merge(Line, NewAcc, T);
merge(_Line, Acc, []) -> Acc.

merge_each(_, { '__KVBLOCK__', Line, Old }, { '__KVBLOCK__', _, New }) -> { '__KVBLOCK__', Line, Old ++ New };
merge_each(_, Old, { '__KVBLOCK__', Line, New }) -> { '__KVBLOCK__', Line, [{[],Old}|New] };
merge_each(_, { '__KVBLOCK__', Line, Old }, New) -> { '__KVBLOCK__', Line, Old ++ [{[],New}] };
merge_each(Line, Old, New) -> { '__KVBLOCK__', Line, [{[],Old}, {[],New}] }.

%% Normalize the list of key-value so at the
%% end all values are key-value blocks
normalize(List) ->
  [{Key,normalize_each(Value)} || {Key,Value} <- List].

normalize_each({ '__KVBLOCK__', _, _} = Value) -> Value;
normalize_each(Value) -> { '__KVBLOCK__', 0, [{[],Value}] }.

%% Decouple clauses from kv_blocks. Assumes the given dict was already normalized.
decouple(List)      -> decouple(List, fun(X) -> X end).
decouple(List, Fun) -> decouple_each(Fun(normalize(List)), []).

decouple_each([{Key,{'__KVBLOCK__',_,Value}}|T], Clauses) ->
  Final = lists:foldl(fun({K,V}, Acc) -> [{Key,K,V}|Acc] end, Clauses, Value),
  decouple_each(T, Final);

decouple_each([], Acc) -> lists:reverse(Acc).
