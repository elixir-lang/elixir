-module(test_helper).
-export([test/0, run_and_remove/2, read_fixture/1]).

test() ->
  elixir:boot(),
  eunit:test([
    arithmetic_test,
    erlang_call_test,
    function_test,
    match_test,
    module_test,
    object_model_test
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