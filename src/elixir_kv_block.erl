%% Helpers related to key-value blocks.
-module(elixir_kv_block).
-export([merge/2, merge/3, normalize/1, decouple/2, decouple/1, validate/4]).
-include("elixir.hrl").

merge(Left, Right) -> merge(0, Left, Right).

%% Merge the given list of key-values on the left
%% to the orddict key-values on the right resulting
%% in an orddict.
merge(Line, Acc, [{Key,Value}|T]) ->
  NewAcc = orddict:update(Key, fun(Old) -> merge_each(Line, Old, Value) end, Value, Acc),
  merge(Line, NewAcc, T);
merge(_Line, Acc, []) -> Acc.

merge_each(_, { '__kvblock__', Line, Old }, { '__kvblock__', _, New }) -> { '__kvblock__', Line, Old ++ New };
merge_each(_, Old, { '__kvblock__', Line, New }) -> { '__kvblock__', Line, [{[],Old}|New] };
merge_each(_, { '__kvblock__', Line, Old }, New) -> { '__kvblock__', Line, Old ++ [{[],New}] };
merge_each(Line, Old, New) -> { '__kvblock__', Line, [{[],Old}, {[],New}] }.

%% Normalize the list of key-value so at the
%% end all values are key-value blocks
normalize(List) ->
  [{Key,normalize_each(Value)} || {Key,Value} <- List].

normalize_each({ '__kvblock__', _, _} = Value) -> Value;
normalize_each(Value) -> { '__kvblock__', 0, [{[],Value}] }.

%% Decouple clauses from kv_blocks. Assumes the given dict was already normalized.
decouple(List)      -> decouple(List, fun(X) -> X end).
decouple(List, Fun) -> decouple_each(Fun(normalize(List)), []).

decouple_each([{Key,{'__kvblock__',_,Value}}|T], Clauses) ->
  Final = lists:foldl(fun({K,V}, Acc) -> [{Key,K,V}|Acc] end, Clauses, Value),
  decouple_each(T, Final);

decouple_each([], Acc) -> lists:reverse(Acc).

%% validate
validate(Line, {Key,[],_}, Count, S) when Count > 0 ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for ~s", [Key]);

validate(Line, {Key,List,_}, 0, S) when List /= [] ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid conditions for ~s", [Key]);

validate(Line, {Key,List,_}, 1, S) when length(List) > 1 ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid comma arguments for ~s", [Key]);

validate(_, _, _, _) -> ok.