-module(test_helper).
-export([test/0, run_and_remove/2, read_fixture/1, throw_elixir/1, throw_erlang/1]).

test() ->
  elixir:boot(),
  eunit:test([
    arithmetic_test,
    builtin_test,
    erlang_call_test,
    function_test,
    object_test,
    match_test,
    module_test,
    string_test
  ]).

% Execute a piece of code and purge given modules right after
run_and_remove(Fun, Modules) ->
  try
    Fun()
  after
    [code:purge(Module) || Module <- Modules]
  end.

% Helper to load files
read_fixture(Filename) ->
  Dirname = filename:dirname(?FILE),
  Fullpath = filename:join([Dirname, "fixtures", Filename]),
  {ok, Bin} = file:read_file(Fullpath),
  binary_to_list(Bin).

% Throws an error with the Erlang Abstract Form from the Elixir string
throw_elixir(String) ->
  erlang:error(io:format("~p~n", [elixir:parse(String, [])])).

% Throws an error with the Erlang Abstract Form from the Erlang string
throw_erlang(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  erlang:error(io:format("~p~n", [Form])).
