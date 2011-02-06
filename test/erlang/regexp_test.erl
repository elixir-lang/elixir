-module(regexp_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Evaluate the Expr returning Regexp internal information.
eval_regexp(Expr) ->
  { Regexp, Binding } = elixir:eval(Expr),
  { test_helper:unpack_regexp(Regexp), Binding }.

base_regexp_test() ->
  {{<<"foo">>, [multiline], _}, []} = eval_regexp("~r{foo}"),
  {{<<"foo">>, [unicode, caseless, multiline], _}, []} = eval_regexp("~r{foo}iu"),
  {{<<"foo">>, [multiline], _}, []} = eval_regexp("~R{foo}"),
  {{<<"foo">>, [unicode, caseless, multiline], _}, []} = eval_regexp("~R{f#{'o}o}iu").

compiled_regexp_test() ->
  {{<<"foo">>, [unicode, caseless, multiline], Compiled}, []} = eval_regexp("~R{f#{'o}o}iu"),
  re_pattern = element(1, Compiled).

invalid_option_regexp_test() ->
  ?assertError({badarg, "unknown option \"h\""}, elixir:eval("~r{foo}h")).

