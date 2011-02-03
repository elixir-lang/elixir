-module(test_helper).
-include("elixir.hrl").
-export([test/0, unpack_string/1, unpack_regexp/1, run_and_remove/2, load_fixture/1, throw_elixir/1, throw_erlang/1]).

test() ->
  elixir:boot(),
  eunit:test([
    arithmetic_test,
    atom_test,
    dict_test,
    erlang_call_test,
    function_test,
    object_test,
    match_test,
    module_test,
    operator_test,
    regexp_test,
    string_test
  ]).

unpack_string(String) ->
  dict:fetch(list, String#elixir_object.data).

unpack_regexp(Regexp) ->
  Data = Regexp#elixir_object.data,
  { dict:fetch(list, Data), dict:fetch(options, Data), dict:fetch(compiled, Data) }.

% Execute a piece of code and purge given modules right after
run_and_remove(Fun, Modules) ->
  try
    Fun()
  after
    [code:purge(Module) || Module <- Modules]
  end.

% Helper to load files
load_fixture(Filename) ->
  Dirname = filename:dirname(?FILE),
  Fullpath = filename:join([Dirname, "fixtures", Filename]),
  elixir:load_file(Fullpath, []).

% Throws an error with the Erlang Abstract Form from the Elixir string
throw_elixir(String) ->
  erlang:error(io:format("~p~n", [elixir:parse(String, [])])).

% Throws an error with the Erlang Abstract Form from the Erlang string
throw_erlang(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  erlang:error(io:format("~p~n", [Form])).
