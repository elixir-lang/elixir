-module(control_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _, _ } = elixir:eval(Content, []),
  { Value, Binding }.

to_erl(String) ->
  Forms = elixir:'string_to_quoted!'(String, 1, <<"nofile">>, []),
  { Expr, _, _ } = elixir:quoted_to_erl(Forms, elixir:env_for_eval([])),
  Expr.

% Booleans

booleans_test() ->
  {nil, _} = eval("nil"),
  {true, _} = eval("true"),
  {false, _} = eval("false").

% If

if_else_kv_args_test() ->
  {1, _} = eval("if(true, do: 1)"),
  {nil, _} = eval("if(false, do: 1)"),
  {2, _} = eval("if(false, do: 1, else: 2)").

if_else_kv_blocks_test() ->
  {2, _} = eval("if(false) do\n1\nelse\n2\nend"),
  {2, _} = eval("if(false) do\n1\n3\nelse\n2\nend"),
  {2, _} = eval("if(false) do 1 else 2 end"),
  {2, _} = eval("if(false) do 1;else 2; end"),
  {3, _} = eval("if(false) do 1;else 2; 3; end").

vars_if_test() ->
  F = fun() ->
    {1, [{foo,1}]} = eval("if foo = 1 do; true; else false; end; foo"),
    eval("defmodule Bar do\ndef foo, do: 1\ndef bar(x) do\nif x do; foo = 2; else foo = foo; end; foo; end\nend"),
    {1, _} = eval("Bar.bar(false)"),
    {2, _} = eval("Bar.bar(true)")
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

multi_assigned_if_test() ->
  {3, _} = eval("x = 1\nif true do\nx = 2\nx = 3\nelse true\nend\nx"),
  {3, _} = eval("x = 1\nif true do\n^x = 1\nx = 2\nx = 3\nelse true\nend\nx"),
  {1, _} = eval("if true do\nx = 1\nelse true\nend\nx"),
  {nil, _} = eval("if false do\nx = 1\nelse true\nend\nx").

multi_line_if_test() ->
  {1, _} = eval("if true\ndo\n1\nelse\n2\nend").

% Try

try_test() ->
  {2, _} = eval("try do\n:foo.bar\ncatch\n:error, :undef -> 2\nend").

try_else_test() ->
  {true, _} = eval("try do\n1\nelse 2 -> false\n1 -> true\nrescue\nErlangError -> nil\nend"),
  {true, _} = eval("try do\n1\nelse {x,y} -> false\nx -> true\nrescue\nErlangError -> nil\nend"),
  {true, _} = eval("try do\n{1,2}\nelse {3,4} -> false\n_ -> true\nrescue\nErlangError -> nil\nend").

% Receive

receive_test() ->
  {10, _} = eval("send self(), :foo\nreceive do\n:foo -> 10\nend"),
  {20, _} = eval("send self(), :bar\nreceive do\n:foo -> 10\n_ -> 20\nend"),
  {30, _} = eval("receive do\nafter 1 -> 30\nend").

vars_receive_test() ->
  {10, _} = eval("send self(), :foo\nreceive do\n:foo ->\na = 10\n:bar -> nil\nend\na"),
  {nil, _} = eval("send self(), :bar\nreceive do\n:foo ->\nb = 10\n_ -> 20\nend\nb"),
  {30, _} = eval("receive do\n:foo -> nil\nafter\n1 -> c = 30\nend\nc"),
  {30, _} = eval("x = 1\nreceive do\n:foo -> nil\nafter\nx -> c = 30\nend\nc").

% Case

case_test() ->
  {true, []} = eval("case 1 do\n2 -> false\n1 -> true\nend"),
  {true, []} = eval("case 1 do\n{x,y} -> false\nx -> true\nend"),
  {true, []} = eval("case {1,2} do;{3,4} -> false\n_ -> true\nend").

case_with_do_ambiguity_test() ->
  {true,_} = eval("case atom_to_list(true) do\n_ -> true\nend").

case_with_match_do_ambiguity_test() ->
  {true,_} = eval("case x = atom_to_list(true) do\n_ -> true\nend").

case_with_unary_do_ambiguity_test() ->
  {false,_} = eval("! case atom_to_list(true) do\n_ -> true\nend").

multi_assigned_case_test() ->
  {3, _} = eval("x = 1\ncase true do\n true ->\nx = 2\nx = 3\n_ -> true\nend\nx"),
  {3, _} = eval("x = 1\ncase 1 do\n ^x -> x = 2\nx = 3\n_ -> true\nend\nx"),
  {1, _} = eval("case true do\ntrue -> x = 1\n_ -> true\nend\nx"),
  {nil, _} = eval("case true do\nfalse -> x = 1\n_ -> true\nend\nx").

vars_case_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: 1\ndef bar(x) do\ncase x do\ntrue -> foo = 2\nfalse -> foo = foo\nend\nfoo\nend\nend"),
    {1, _} = eval("Bar.bar(false)"),
    {2, _} = eval("Bar.bar(true)")
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

% Comparison

equal_test() ->
  {true,_} = eval(":a == :a"),
  {true,_} = eval("1 == 1"),
  {true,_} = eval("{1,2} == {1,2}"),
  {false,_} = eval("1 == 2"),
  {false,_} = eval("{1,2} == {1,3}").

not_equal_test() ->
  {false,_} = eval(":a != :a"),
  {false,_} = eval("1 != 1"),
  {false,_} = eval("{1,2} != {1,2}"),
  {true,_} = eval("1 != 2"),
  {true,_} = eval("{1,2} != {1,3}").

not_exclamation_mark_test() ->
  {false,_} = eval("! :a"),
  {false,_} = eval("!true"),
  {false,_} = eval("!1"),
  {false,_} = eval("![]"),
  {true,_} = eval("!nil"),
  {true,_} = eval("!false").

notnot_exclamation_mark_test() ->
  {true,_} = eval("!! :a"),
  {true,_} = eval("!!true"),
  {true,_} = eval("!!1"),
  {true,_} = eval("!![]"),
  {false,_} = eval("!!nil"),
  {false,_} = eval("!!false").

less_greater_test() ->
  {true,_} = eval("1 < 2"),
  {true,_} = eval("1 < :a"),
  {false,_} = eval("1 < 1.0"),
  {false,_} = eval("1 < 1"),
  {true,_} = eval("1 <= 1.0"),
  {true,_} = eval("1 <= 1"),
  {true,_} = eval("1 <= :a"),
  {false,_} = eval("1 > 2"),
  {false,_} = eval("1 > :a"),
  {false,_} = eval("1 > 1.0"),
  {false,_} = eval("1 > 1"),
  {true,_} = eval("1 >= 1.0"),
  {true,_} = eval("1 >= 1"),
  {false,_} = eval("1 >= :a").

integer_and_float_test() ->
  {true,_} = eval("1 == 1"),
  {false,_} = eval("1 != 1"),
  {true,_} = eval("1 == 1.0"),
  {false,_} = eval("1 != 1.0"),
  {true,_} = eval("1 === 1"),
  {false,_} = eval("1 !== 1"),
  {false,_} = eval("1 === 1.0"),
  {true,_} = eval("1 !== 1.0").

xor_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {false, _} = eval("true xor true"),
    {true, _} = eval("true xor false"),
    {true, _} = eval("false xor true"),
    {false, _} = eval("false xor false"),
    {false, _} = eval("Bar.foo xor Bar.foo"),
    {true, _} = eval("Bar.foo xor Bar.bar"),
    {false, _} = eval("Bar.bar xor Bar.bar"),
    {true, _} = eval("Bar.bar xor Bar.baz 1"),
    {false, _} = eval("Bar.bar xor Bar.baz 2"),
    ?assertError(badarg, eval("1 xor 2"))
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

and_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = eval("true and true"),
    {false, _} = eval("true and false"),
    {false, _} = eval("false and true"),
    {false, _} = eval("false and false"),
    {true, _} = eval("Bar.foo and Bar.foo"),
    {false, _} = eval("Bar.foo and Bar.bar"),
    {true, _} = eval("Bar.foo and Bar.baz 1"),
    {false, _} = eval("Bar.foo and Bar.baz 2"),
    {true, _} = eval("false and false or true"),
    {3, _} = eval("Bar.foo and 1 + 2"),
    {false, _} = eval("Bar.bar and :erlang.error(:bad)"),
    ?assertError({badarg, 1}, eval("1 and 2"))
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

or_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = eval("true or true"),
    {true, _} = eval("true or false"),
    {true, _} = eval("false or true"),
    {false, _} = eval("false or false"),
    {true, _} = eval("Bar.foo or Bar.foo"),
    {true, _} = eval("Bar.foo or Bar.bar"),
    {false, _} = eval("Bar.bar or Bar.bar"),
    {true, _} = eval("Bar.bar or Bar.baz 1"),
    {false, _} = eval("Bar.bar or Bar.baz 2"),
    {3, _} = eval("Bar.bar or 1 + 2"),
    {true, _} = eval("Bar.foo or :erlang.error(:bad)"),
    ?assertError({badarg, 1}, eval("1 or 2"))
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

not_test() ->
  {false, _} = eval("not true"),
  {true, _} = eval("not false"),
  ?assertError(badarg, eval("not 1")).

andand_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = eval("Kernel.&&(true, true)"),
    {true, _} = eval("true && true"),
    {false, _} = eval("true && false"),
    {false, _} = eval("false && true"),
    {false, _} = eval("false && false"),
    {nil, _} = eval("true && nil"),
    {nil, _} = eval("nil && true"),
    {false, _} = eval("false && nil"),
    {true, _} = eval("Bar.foo && Bar.foo"),
    {false, _} = eval("Bar.foo && Bar.bar"),
    {true, _} = eval("Bar.foo && Bar.baz 1"),
    {false, _} = eval("Bar.foo && Bar.baz 2"),
    {true, _} = eval("1 == 1 && 2 < 3"),
    {3, _} = eval("Bar.foo && 1 + 2"),
    {false, _} = eval("Bar.bar && :erlang.error(:bad)"),
    {2, _} = eval("1 && 2"),
    {nil, _} = eval("nil && 2")
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

andand_with_literal_test() ->
  {[nil, nil, nil], _} = eval("[nil && 2, nil && 3, nil && 4]").

oror_test() ->
  F = fun() ->
    eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = eval("Kernel.||(false, true)"),
    {true, _} = eval("true || true"),
    {true, _} = eval("true || false"),
    {true, _} = eval("false || true"),
    {false, _} = eval("false || false"),
    {false, _} = eval("nil || false"),
    {nil, _} = eval("false || nil"),
    {true, _} = eval("false || nil || true"),
    {true, _} = eval("Bar.foo || Bar.foo"),
    {true, _} = eval("Bar.foo || Bar.bar"),
    {false, _} = eval("Bar.bar || Bar.bar"),
    {true, _} = eval("Bar.bar || Bar.baz 1"),
    {false, _} = eval("Bar.bar || Bar.baz 2"),
    {false, _} = eval("1 == 2 || 2 > 3"),
    {3, _} = eval("Bar.bar || 1 + 2"),
    {true, _} = eval("Bar.foo || :erlang.error(:bad)"),
    {1, _} = eval("1 || 2"),
    {2, _} = eval("nil || 2"),
    {true, _} = eval("false && false || true")
  end,
  test_helper:run_and_remove(F, ['Elixir.Bar']).

% Optimized

optimized_if_test() ->
  { 'case', _, _,
    [{clause,_,[{atom,_,false}],[],[{atom,_,else}]},
     {clause,_,[{atom,_,true}],[],[{atom,_,do}]}]
  } = to_erl("if is_list([]), do: :do, else: :else").

optimized_andand_test() ->
  { 'case', _, _,
    [{clause,_,
      [{var,_,Var}],
      [[{op,_,'orelse',_,_}]],
      [{var,_,Var}]},
    {clause,_,[{var,_,'_'}],[],[{atom,0,done}]}]
  } = to_erl("is_list([]) && :done").

optimized_oror_test() ->
  { 'case', _, _,
    [{clause,1,
      [{var,1,_}],
      [[{op,1,'orelse',_,_}]],
      [{atom,0,done}]},
    {clause,1,[{var,1,Var}],[],[{var,1,Var}]}]
  } = to_erl("is_list([]) || :done").

no_after_in_try_test() ->
  { 'try', _, [_], [_], _, [] } = to_erl("try do :foo.bar() else _ -> :ok end").