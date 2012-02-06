-module(conditionals_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Booleans

booleans_test() ->
  {nil, _} = elixir:eval("nil"),
  {true, _} = elixir:eval("true"),
  {false, _} = elixir:eval("false").

% If

if_else_kv_args_test() ->
  {1, _} = elixir:eval("if(true, do: 1)"),
  {nil, _} = elixir:eval("if(false, do: 1)"),
  {2, _} = elixir:eval("if(false, do: 1, else: 2)").

if_else_kv_blocks_test() ->
  {2, _} = elixir:eval("if(false) do\n1\nelse:\n2\nend"),
  {2, _} = elixir:eval("if(false) do\n1\n3\nelse:\n2\nend"),
  {2, _} = elixir:eval("if(false) do 1;else: 2; end"),
  {3, _} = elixir:eval("if(false) do 1;else: 2; 3; end").

if_elsif_else_test() ->
  {3, _} = elixir:eval("if false do\n 1\nelsif: true\n3\nelse:\n2\nend"),
  {nil, _} = elixir:eval("if false do\n 1\nelsif: [true, 3]\nelse:\n2\nend").

vars_if_test() ->
  F = fun() ->
    {1, [{foo,1}]} = elixir:eval("if foo = 1 do; true; else: false; end; foo"),
    elixir:eval("defmodule Bar do\ndef foo, do: 1\ndef bar(x) do\nif x do; foo = 2; else: foo = foo; end; foo; end\nend"),
    {1, _} = elixir:eval("Bar.bar(false)"),
    {2, _} = elixir:eval("Bar.bar(true)")
  end,
  test_helper:run_and_remove(F, ['::Bar']).

multi_assigned_if_test() ->
  {3, _} = elixir:eval("x = 1\nif true do\nx = 2\nx = 3\nelse: true\nend\nx"),
  {3, _} = elixir:eval("x = 1\nif true do\n^x = 1\nx = 2\nx = 3\nelse: true\nend\nx"),
  {1, _} = elixir:eval("if true do\nx = 1\nelse: true\nend\nx"),
  {nil, _} = elixir:eval("if false do\nx = 1\nelse: true\nend\nx").

% Try

try_test() ->
  {2, _} = elixir:eval("try do\nErlang.foo.bar\ncatch: :error, :undef; 2\nend").

% Receive

receive_test() ->
  {10, _} = elixir:eval("self() <- :foo\nreceive do\nmatch: :foo\n10\nend"),
  {20, _} = elixir:eval("self() <- :bar\nreceive do\nmatch: :foo\n10\nelse: 20\nend"),
  {30, _} = elixir:eval("receive do\nafter: 1\n30\nend").

vars_receive_test() ->
  {10, _} = elixir:eval("self() <- :foo\nreceive do\nmatch: :foo\na = 10\nmatch: :bar\nend\na"),
  {nil, _} = elixir:eval("self() <- :bar\nreceive do\nmatch: :foo\nb = 10\nelse: 20\nend\nb"),
  {30, _} = elixir:eval("receive do\nmatch: :foo\nafter: 1\nc = 30\nend\nc").

% Case

case_test() ->
  {true, _} = elixir:eval("case 1 do\nmatch: 2; false\nmatch: 1; true\nend"),
  {true, [{x,1}]} = elixir:eval("case 1 do\nmatch: %{x,y}; false\nmatch: x; true\nend"),
  {true, _} = elixir:eval("case %{1,2} do;match: %{3,4}\nfalse\nelse: true\nend").

case_with_do_ambiguity_test() ->
  {true,_} = elixir:eval("case atom_to_list(true) do\nmatch: _; true\nend").

case_with_match_do_ambiguity_test() ->
  {true,_} = elixir:eval("case x = atom_to_list(true) do\nmatch: _; true\nend").

case_with_unary_do_ambiguity_test() ->
  {false,_} = elixir:eval("! case atom_to_list(true) do\nmatch: _; true\nend").

multi_assigned_case_test() ->
  {3, _} = elixir:eval("x = 1\ncase true do\n match: true\nx = 2\nx = 3\nelse: true\nend\nx"),
  {3, _} = elixir:eval("x = 1\ncase 1 do\n match: ^x\nx = 2\nx = 3\nelse: true\nend\nx"),
  {1, _} = elixir:eval("case true do\nmatch: true\nx = 1\nelse: true\nend\nx"),
  {nil, _} = elixir:eval("case true do\nmatch: false\nx = 1\nelse: true\nend\nx").

vars_case_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: 1\ndef bar(x) do\ncase x do\nmatch: true; foo = 2\nmatch: false; foo = foo\nend\nfoo\nend\nend"),
    {1, _} = elixir:eval("Bar.bar(false)"),
    {2, _} = elixir:eval("Bar.bar(true)")
  end,
  test_helper:run_and_remove(F, ['::Bar']).

% Comparison

equal_test() ->
  {true,_} = elixir:eval(":a == :a"),
  {true,_} = elixir:eval("1 == 1"),
  {true,_} = elixir:eval("%{1,2} == %{1,2}"),
  {false,_} = elixir:eval("1 == 2"),
  {false,_} = elixir:eval("%{1,2} == %{1,3}").

not_equal_test() ->
  {false,_} = elixir:eval(":a != :a"),
  {false,_} = elixir:eval("1 != 1"),
  {false,_} = elixir:eval("%{1,2} != %{1,2}"),
  {true,_} = elixir:eval("1 != 2"),
  {true,_} = elixir:eval("%{1,2} != %{1,3}").

not_exclamation_mark_test() ->
  {false,_} = elixir:eval("! :a"),
  {false,_} = elixir:eval("!true"),
  {false,_} = elixir:eval("!1"),
  {false,_} = elixir:eval("![]"),
  {true,_} = elixir:eval("!nil"),
  {true,_} = elixir:eval("!false").

notnot_exclamation_mark_test() ->
  {true,_} = elixir:eval("!! :a"),
  {true,_} = elixir:eval("!!true"),
  {true,_} = elixir:eval("!!1"),
  {true,_} = elixir:eval("!![]"),
  {false,_} = elixir:eval("!!nil"),
  {false,_} = elixir:eval("!!false").

less_greater_test() ->
  {true,_} = elixir:eval("1 < 2"),
  {true,_} = elixir:eval("1 < :a"),
  {false,_} = elixir:eval("1 < 1.0"),
  {false,_} = elixir:eval("1 < 1"),
  {true,_} = elixir:eval("1 <= 1.0"),
  {true,_} = elixir:eval("1 <= 1"),
  {true,_} = elixir:eval("1 <= :a"),
  {false,_} = elixir:eval("1 > 2"),
  {false,_} = elixir:eval("1 > :a"),
  {false,_} = elixir:eval("1 > 1.0"),
  {false,_} = elixir:eval("1 > 1"),
  {true,_} = elixir:eval("1 >= 1.0"),
  {true,_} = elixir:eval("1 >= 1"),
  {false,_} = elixir:eval("1 >= :a").

integer_and_float_test() ->
  {true,_} = elixir:eval("1 == 1"),
  {false,_} = elixir:eval("1 != 1"),
  {true,_} = elixir:eval("1 == 1.0"),
  {false,_} = elixir:eval("1 != 1.0"),
  {true,_} = elixir:eval("1 === 1"),
  {false,_} = elixir:eval("1 !== 1"),
  {false,_} = elixir:eval("1 === 1.0"),
  {true,_} = elixir:eval("1 !== 1.0").

and_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("true and true"),
    {false, _} = elixir:eval("true and false"),
    {false, _} = elixir:eval("false and true"),
    {false, _} = elixir:eval("false and false"),
    {true, _} = elixir:eval("Bar.foo and Bar.foo"),
    {false, _} = elixir:eval("Bar.foo and Bar.bar"),
    {true, _} = elixir:eval("Bar.foo and Bar.baz 1"),
    {false, _} = elixir:eval("Bar.foo and Bar.baz 2"),
    {true, _} = elixir:eval("1 == 1 and 2 < 3"),
    {true, _} = elixir:eval("false and false or true"),
    ?assertError(badarg, elixir:eval("1 and 2"))
  end,
  test_helper:run_and_remove(F, ['::Bar']).

or_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("true or true"),
    {true, _} = elixir:eval("true or false"),
    {true, _} = elixir:eval("false or true"),
    {false, _} = elixir:eval("false or false"),
    {true, _} = elixir:eval("Bar.foo or Bar.foo"),
    {true, _} = elixir:eval("Bar.foo or Bar.bar"),
    {false, _} = elixir:eval("Bar.bar or Bar.bar"),
    {true, _} = elixir:eval("Bar.bar or Bar.baz 1"),
    {false, _} = elixir:eval("Bar.bar or Bar.baz 2"),
    ?assertError(badarg, elixir:eval("1 or 2"))
  end,
  test_helper:run_and_remove(F, ['::Bar']).

xor_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {false, _} = elixir:eval("true xor true"),
    {true, _} = elixir:eval("true xor false"),
    {true, _} = elixir:eval("false xor true"),
    {false, _} = elixir:eval("false xor false"),
    {false, _} = elixir:eval("Bar.foo xor Bar.foo"),
    {true, _} = elixir:eval("Bar.foo xor Bar.bar"),
    {false, _} = elixir:eval("Bar.bar xor Bar.bar"),
    {true, _} = elixir:eval("Bar.bar xor Bar.baz 1"),
    {false, _} = elixir:eval("Bar.bar xor Bar.baz 2"),
    ?assertError(badarg, elixir:eval("1 xor 2"))
  end,
  test_helper:run_and_remove(F, ['::Bar']).

andalso_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("true andalso true"),
    {false, _} = elixir:eval("true andalso false"),
    {false, _} = elixir:eval("false andalso true"),
    {false, _} = elixir:eval("false andalso false"),
    {true, _} = elixir:eval("Bar.foo andalso Bar.foo"),
    {false, _} = elixir:eval("Bar.foo andalso Bar.bar"),
    {true, _} = elixir:eval("Bar.foo andalso Bar.baz 1"),
    {false, _} = elixir:eval("Bar.foo andalso Bar.baz 2"),
    {true, _} = elixir:eval("false andalso false orelse true"),
    {3, _} = elixir:eval("Bar.foo andalso 1 + 2"),
    {false, _} = elixir:eval("Bar.bar andalso error(:bad)"),
    ?assertError({badarg, 1}, elixir:eval("1 andalso 2"))
  end,
  test_helper:run_and_remove(F, ['::Bar']).

orelse_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("true orelse true"),
    {true, _} = elixir:eval("true orelse false"),
    {true, _} = elixir:eval("false orelse true"),
    {false, _} = elixir:eval("false orelse false"),
    {true, _} = elixir:eval("Bar.foo orelse Bar.foo"),
    {true, _} = elixir:eval("Bar.foo orelse Bar.bar"),
    {false, _} = elixir:eval("Bar.bar orelse Bar.bar"),
    {true, _} = elixir:eval("Bar.bar orelse Bar.baz 1"),
    {false, _} = elixir:eval("Bar.bar orelse Bar.baz 2"),
    {3, _} = elixir:eval("Bar.bar orelse 1 + 2"),
    {true, _} = elixir:eval("Bar.foo orelse error(:bad)"),
    ?assertError({badarg, 1}, elixir:eval("1 orelse 2"))
  end,
  test_helper:run_and_remove(F, ['::Bar']).

not_test() ->
  {false, _} = elixir:eval("not true"),
  {true, _} = elixir:eval("not false"),
  ?assertError(badarg, elixir:eval("not 1")).

andand_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("Elixir::Builtin.&&(true, true)"),
    {true, _} = elixir:eval("true && true"),
    {false, _} = elixir:eval("true && false"),
    {false, _} = elixir:eval("false && true"),
    {false, _} = elixir:eval("false && false"),
    {nil, _} = elixir:eval("true && nil"),
    {nil, _} = elixir:eval("nil && true"),
    {false, _} = elixir:eval("false && nil"),
    {true, _} = elixir:eval("Bar.foo && Bar.foo"),
    {false, _} = elixir:eval("Bar.foo && Bar.bar"),
    {true, _} = elixir:eval("Bar.foo && Bar.baz 1"),
    {false, _} = elixir:eval("Bar.foo && Bar.baz 2"),
    {true, _} = elixir:eval("1 == 1 && 2 < 3"),
    {3, _} = elixir:eval("Bar.foo && 1 + 2"),
    {false, _} = elixir:eval("Bar.bar && error(:bad)"),
    {2, _} = elixir:eval("1 && 2"),
    {nil, _} = elixir:eval("nil && 2"),
    {false, _} = elixir:eval("false && false or true")
  end,
  test_helper:run_and_remove(F, ['::Bar']).

oror_test() ->
  F = fun() ->
    elixir:eval("defmodule Bar do\ndef foo, do: true\ndef bar, do: false\n def baz(x), do: x == 1\nend"),
    {true, _} = elixir:eval("Elixir::Builtin.||(false, true)"),
    {true, _} = elixir:eval("true || true"),
    {true, _} = elixir:eval("true || false"),
    {true, _} = elixir:eval("false || true"),
    {false, _} = elixir:eval("false || false"),
    {false, _} = elixir:eval("nil || false"),
    {nil, _} = elixir:eval("false || nil"),
    {true, _} = elixir:eval("false || nil || true"),
    {true, _} = elixir:eval("Bar.foo || Bar.foo"),
    {true, _} = elixir:eval("Bar.foo || Bar.bar"),
    {false, _} = elixir:eval("Bar.bar || Bar.bar"),
    {true, _} = elixir:eval("Bar.bar || Bar.baz 1"),
    {false, _} = elixir:eval("Bar.bar || Bar.baz 2"),
    {false, _} = elixir:eval("1 == 2 || 2 > 3"),
    {3, _} = elixir:eval("Bar.bar || 1 + 2"),
    {true, _} = elixir:eval("Bar.foo || error(:bad)"),
    {1, _} = elixir:eval("1 || 2"),
    {2, _} = elixir:eval("nil || 2"),
    {true, _} = elixir:eval("false && false || true")
  end,
  test_helper:run_and_remove(F, ['::Bar']).
