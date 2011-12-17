-module(conditionals_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Booleans

booleans_test() ->
  {nil, _} = elixir:eval("nil"),
  {true, _} = elixir:eval("true"),
  {false, _} = elixir:eval("false").

if_kvargs_test() ->
  {1, _} = elixir:eval("if(true, do: 1)"),
  {nil, _} = elixir:eval("if(false, do: 1)"),
  {2, _} = elixir:eval("if(false, do: 1, else: 2)"),
  {2, _} = elixir:eval("if(false) do\n1\nelse:\n2\nend").
  % {3, _} = elixir:eval("if false; 1; elsif true; 3; else; 2; end"),
  % {3, _} = elixir:eval("if false\n 1\n elsif true\n 3\n else\n 2\n end"),
  % {3, _} = elixir:eval("if false then 1 elsif true then 3 else 2 end").

% unless_test() ->
%   {1, _} = elixir:eval("unless false; 1; end"),
%   {nil, _} = elixir:eval("unless true; 1; end"),
%   {2, _} = elixir:eval("unless true; 1; else 2; end"),
%   {2, _} = elixir:eval("unless true; 1; else; 2; end"),
%   {3, _} = elixir:eval("unless true; 1; elsif true; 3; else; 2; end").
% 
% vars_if_test() ->
%   F = fun() ->
%     {1, [{foo,1}]} = elixir:eval("if foo = 1; true; else; false; end; foo"),
%     elixir:eval("module Bar\ndef foo; 1; end\ndef bar(x); if x; foo = 2; else; foo = foo; end; foo; end\nend"),
%     {1, _} = elixir:eval("Bar.bar(false)"),
%     {2, _} = elixir:eval("Bar.bar(true)")
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% multi_assigned_if_test() ->
%   {3, _} = elixir:eval("x = 1\nif true\nx = 2\nx = 3\nelse true\nend\nx"),
%   {3, _} = elixir:eval("x = 1\nif true\n\~x = 1\nx = 2\nx = 3\nelse true\nend\nx"),
%   {1, _} = elixir:eval("if true\nx = 1\nelse true\nend\nx"),
%   {nil, _} = elixir:eval("if false\nx = 1\nelse true\nend\nx").
% 
% case_test() ->
%   {true, _} = elixir:eval("case 1 match 2 then false match 1 then true end"),
%   {true, [{x,1}]} = elixir:eval("case 1 match {x,y} then false match x then true end"),
%   {true, [{x,1},{y,2}]} = elixir:eval("case {1,2} match {x,y} then true match {1,x} then false end"),
%   {true, [{x,1},{y,2}]} = elixir:eval("case {1,2} match {x,y}\ntrue\nmatch {1,x}\nfalse\nend"),
%   {true, _} = elixir:eval("case {1,2} match {3,4}\nfalse\nelse true\nend"),
%   {true, _} = elixir:eval("case {1,2} match {3,4}, {1,2}\ntrue\nend").
% 
% multi_assigned_case_test() ->
%   {3, _} = elixir:eval("x = 1\ncase true match true\nx = 2\nx = 3\nelse true\nend\nx"),
%   {3, _} = elixir:eval("x = 1\ncase 1 match \~x\nx = 2\nx = 3\nelse true\nend\nx"),
%   {1, _} = elixir:eval("case true match true\nx = 1\nelse true\nend\nx"),
%   {nil, _} = elixir:eval("case true match false\nx = 1\nelse true\nend\nx").
% 
% vars_case_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; 1; end\ndef bar(x); case x match true then foo = 2 match false then foo = foo end; foo; end\nend"),
%     {1, _} = elixir:eval("Bar.bar(false)"),
%     {2, _} = elixir:eval("Bar.bar(true)"),
%     elixir:eval("module Baz\ndef foo; 1; end\ndef bar(x); case x match {foo,2} then \~foo = 2 match false then foo = foo end; foo; end\nend"),
%     {1, _} = elixir:eval("Baz.bar(false)"),
%     {2, _} = elixir:eval("Baz.bar({2, 2})"),
%     ?assertError({badmatch, 2}, elixir:eval("Baz.bar({1, 2})"))
%   end,
%   test_helper:run_and_remove(F, ['Bar', 'Baz']).
% 
% %% Comparison
% 
% equal_test() ->
%   {true,_} = elixir:eval("'a == 'a"),
%   {true,_} = elixir:eval("1 == 1"),
%   {true,_} = elixir:eval("~q(a) == ~q(a)"),
%   {true,_} = elixir:eval("{1,2} == {1,2}"),
%   {true,_} = elixir:eval("[1,2] == [1,2]"),
%   {false,_} = elixir:eval("'a == 'b"),
%   {false,_} = elixir:eval("1 == 2"),
%   {false,_} = elixir:eval("~q(a) == ~q(b)"),
%   {false,_} = elixir:eval("{1,2} == {1,3}"),
%   {false,_} = elixir:eval("[1,2] == [1,3]").
% 
% not_equal_test() ->
%   {false,_} = elixir:eval("'a != 'a"),
%   {false,_} = elixir:eval("1 != 1"),
%   {false,_} = elixir:eval("~q(a) != ~q(a)"),
%   {false,_} = elixir:eval("{1,2} != {1,2}"),
%   {false,_} = elixir:eval("[1,2] != [1,2]"),
%   {true,_} = elixir:eval("'a != 'b"),
%   {true,_} = elixir:eval("1 != 2"),
%   {true,_} = elixir:eval("~q(a) != ~q(b)"),
%   {true,_} = elixir:eval("{1,2} != {1,3}"),
%   {true,_} = elixir:eval("[1,2] != [1,3]").
% 
% not_exclamation_mark_test() ->
%   {false,_} = elixir:eval("!'a"),
%   {false,_} = elixir:eval("!true"),
%   {false,_} = elixir:eval("!1"),
%   {false,_} = elixir:eval("![]"),
%   {true,_} = elixir:eval("!nil"),
%   {true,_} = elixir:eval("!false").
% 
% notnot_exclamation_mark_test() ->
%   {true,_} = elixir:eval("!!'a"),
%   {true,_} = elixir:eval("!!true"),
%   {true,_} = elixir:eval("!!1"),
%   {true,_} = elixir:eval("!![]"),
%   {false,_} = elixir:eval("!!nil"),
%   {false,_} = elixir:eval("!!false").
% 
% less_greater_test() ->
%   {true,_} = elixir:eval("1 < 2"),
%   {true,_} = elixir:eval("1 < 'a"),
%   {false,_} = elixir:eval("1 < 1.0"),
%   {false,_} = elixir:eval("1 < 1"),
%   {true,_} = elixir:eval("1 <= 1.0"),
%   {true,_} = elixir:eval("1 <= 1"),
%   {true,_} = elixir:eval("1 <= 'a"),
%   {false,_} = elixir:eval("1 > 2"),
%   {false,_} = elixir:eval("1 > 'a"),
%   {false,_} = elixir:eval("1 > 1.0"),
%   {false,_} = elixir:eval("1 > 1"),
%   {true,_} = elixir:eval("1 >= 1.0"),
%   {true,_} = elixir:eval("1 >= 1"),
%   {false,_} = elixir:eval("1 >= 'a").
% 
% integer_and_float_test() ->
%   {true,_} = elixir:eval("1 == 1"),
%   {false,_} = elixir:eval("1 != 1"),
%   {true,_} = elixir:eval("1 == 1.0"),
%   {false,_} = elixir:eval("1 != 1.0"),
%   {true,_} = elixir:eval("1 === 1"),
%   {false,_} = elixir:eval("1 !== 1"),
%   {false,_} = elixir:eval("1 === 1.0"),
%   {true,_} = elixir:eval("1 !== 1.0").
% 
% and_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true and true"),
%     {false, _} = elixir:eval("true and false"),
%     {false, _} = elixir:eval("false and true"),
%     {false, _} = elixir:eval("false and false"),
%     {true, _} = elixir:eval("Bar.foo and Bar.foo"),
%     {false, _} = elixir:eval("Bar.foo and Bar.bar"),
%     {true, _} = elixir:eval("Bar.foo and Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.foo and Bar.baz 2"),
%     {true, _} = elixir:eval("1 == 1 and 2 < 3"),
%     {true, _} = elixir:eval("false and false or true"),
%     ?assertError(badarg, elixir:eval("1 and 2"))
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% or_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true or true"),
%     {true, _} = elixir:eval("true or false"),
%     {true, _} = elixir:eval("false or true"),
%     {false, _} = elixir:eval("false or false"),
%     {true, _} = elixir:eval("Bar.foo or Bar.foo"),
%     {true, _} = elixir:eval("Bar.foo or Bar.bar"),
%     {false, _} = elixir:eval("Bar.bar or Bar.bar"),
%     {true, _} = elixir:eval("Bar.bar or Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.bar or Bar.baz 2"),
%     ?assertError(badarg, elixir:eval("1 or 2"))
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% andalso_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true andalso true"),
%     {false, _} = elixir:eval("true andalso false"),
%     {false, _} = elixir:eval("false andalso true"),
%     {false, _} = elixir:eval("false andalso false"),
%     {true, _} = elixir:eval("Bar.foo andalso Bar.foo"),
%     {false, _} = elixir:eval("Bar.foo andalso Bar.bar"),
%     {true, _} = elixir:eval("Bar.foo andalso Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.foo andalso Bar.baz 2"),
%     {true, _} = elixir:eval("false andalso false orelse true"),
%     {3, _} = elixir:eval("Bar.foo andalso 1 + 2"),
%     {false, _} = elixir:eval("Bar.bar andalso Erlang.error('bad)"),
%     ?assertError({badarg, 1}, elixir:eval("1 andalso 2"))
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% orelse_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true orelse true"),
%     {true, _} = elixir:eval("true orelse false"),
%     {true, _} = elixir:eval("false orelse true"),
%     {false, _} = elixir:eval("false orelse false"),
%     {true, _} = elixir:eval("Bar.foo orelse Bar.foo"),
%     {true, _} = elixir:eval("Bar.foo orelse Bar.bar"),
%     {false, _} = elixir:eval("Bar.bar orelse Bar.bar"),
%     {true, _} = elixir:eval("Bar.bar orelse Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.bar orelse Bar.baz 2"),
%     {3, _} = elixir:eval("Bar.bar orelse 1 + 2"),
%     {true, _} = elixir:eval("Bar.foo orelse Erlang.error('bad)"),
%     ?assertError({badarg, 1}, elixir:eval("1 orelse 2"))
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% not_test() ->
%   {false, _} = elixir:eval("not true"),
%   {true, _} = elixir:eval("not false"),
%   ?assertError(badarg, elixir:eval("not 1")).
% 
% andand_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true && true"),
%     {false, _} = elixir:eval("true && false"),
%     {false, _} = elixir:eval("false && true"),
%     {false, _} = elixir:eval("false && false"),
%     {true, _} = elixir:eval("Bar.foo && Bar.foo"),
%     {false, _} = elixir:eval("Bar.foo && Bar.bar"),
%     {true, _} = elixir:eval("Bar.foo && Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.foo && Bar.baz 2"),
%     {true, _} = elixir:eval("1 == 1 && 2 < 3"),
%     {3, _} = elixir:eval("Bar.foo && 1 + 2"),
%     {false, _} = elixir:eval("Bar.bar && Erlang.error('bad)"),
%     {2, _} = elixir:eval("1 && 2"),
%     {nil, _} = elixir:eval("nil && 2"),
%     {false, _} = elixir:eval("false && false or true")
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% oror_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
%     {true, _} = elixir:eval("true || true"),
%     {true, _} = elixir:eval("true || false"),
%     {true, _} = elixir:eval("false || true"),
%     {false, _} = elixir:eval("false || false"),
%     {true, _} = elixir:eval("Bar.foo || Bar.foo"),
%     {true, _} = elixir:eval("Bar.foo || Bar.bar"),
%     {false, _} = elixir:eval("Bar.bar || Bar.bar"),
%     {true, _} = elixir:eval("Bar.bar || Bar.baz 1"),
%     {false, _} = elixir:eval("Bar.bar || Bar.baz 2"),
%     {false, _} = elixir:eval("1 == 2 || 2 > 3"),
%     {3, _} = elixir:eval("Bar.bar || 1 + 2"),
%     {true, _} = elixir:eval("Bar.foo || Erlang.error('bad)"),
%     {1, _} = elixir:eval("1 || 2"),
%     {2, _} = elixir:eval("nil || 2"),
%     {true, _} = elixir:eval("false && false || true")
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% begin_test() ->
%   {1,_} = elixir:eval("begin 1 end"),
%   {1,[{x,1}]} = elixir:eval("begin\nx=1\nend\nx"),
%   {1,[{x,1}]} = elixir:eval("begin;x=1;end;x").