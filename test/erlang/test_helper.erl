-module(test_helper).
-export([test/0, run_and_remove/2, read_fixture/1]).

test() ->
  elixir:boot(),
  eunit:test({inorder, [
    arithmetic_test,
    constants_test,
    function_test,
    module_test,
    variables_test
  ]}).

% Execute a piece of code and purge given modules right after
run_and_remove(Fun, Modules) ->
  try
    Fun()
  after
    [remove_module(Module) || Module <- Modules]
  end.

remove_module(Module) ->
  code:purge(Module),
  ets:delete(ex_constants, Module).

% Helper to load files
read_fixture(Filename) ->
  Dirname = filename:dirname(?FILE),
  Fullpath = filename:join([Dirname, "fixtures", Filename]),
  {ok, Bin} = file:read_file(Fullpath),
  binary_to_list(Bin).