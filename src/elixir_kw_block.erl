%% Helpers related to keywords blocks.
-module(elixir_kw_block).
-export([sort/1, normalize/1, normalize/2, decouple/1, validate/4]).
-include("elixir.hrl").

sort(List) -> lists:sort(fun sort/2, List).
sort({ A, _ }, { B, _ }) -> A =< B.

%% Normalize the list of keywords so at the
%% end all values are keywords blocks
normalize(List) -> normalize(0, List).
normalize(Line, List) ->
  [{Key,normalize_each(Line, Value)} || {Key,Value} <- List].

normalize_each(_Line, { '__kwblock__', _, _} = Value) -> Value;
normalize_each(Line, Value) -> { '__kwblock__', Line, [[],Value] }.

%% Decouple clauses from kv_blocks
decouple(List) -> decouple_each(normalize(List)).
decouple_each([{Key,{'__kwblock__',_,[K,V]}}|T]) -> [{Key,K,V}|decouple_each(T)];
decouple_each([]) -> [].

%% validate
validate(Line, {Key,[],_}, Count, S) when Count > 0 ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "no condition given for ~s", [Key]);

validate(Line, {Key,List,_}, 0, S) when List /= [] ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid conditions for ~s", [Key]);

validate(Line, {Key,List,_}, 1, S) when length(List) > 1 ->
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "invalid comma arguments for ~s", [Key]);

validate(_, _, _, _) -> ok.

