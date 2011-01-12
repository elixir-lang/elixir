-module(module_test).
-include_lib("eunit/include/eunit.hrl").

%% Module functions
%% TODO Assert 1 + module Foo or A = module Foo does not work
% function_calls_in_modules_test() ->
%   {5, _} = elixir:throw_elixir(read_fixture("basic.ex")).

module_bodies_are_executable_test() -> 
  F = fun() ->
    ?assertError({unbound_var, a}, elixir:eval("module Foo; a; end")),
    elixir:eval("module Bar; 1 + 2; end")
  end,
  run_and_purge(F, ['Bar']).

module_declarations_are_convered_into_erlang_modules_test() ->
  F = fun() ->
    elixir:eval("module Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('Bar')
  end,
  run_and_purge(F, ['Bar']).

module_declarations_preceeded_by_other_expressions_test() ->
  F = fun() ->
    elixir:eval("1 + 2\nmodule Bar; 1 + 2; end"),
    {file, "nofile"} = code:is_loaded('Bar')
  end,
  run_and_purge(F, ['Bar']).

% Execute a piece of code and purge given modules right after
run_and_purge(Fun, Modules) ->
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