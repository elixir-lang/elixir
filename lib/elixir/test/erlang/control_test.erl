%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

-module(control_test).
-include_lib("eunit/include/eunit.hrl").

to_erl(String) ->
  Forms = elixir:'string_to_quoted!'(String, 1, 1, <<"nofile">>, []),
  {Expr, _, _, _} = elixir:quoted_to_erl(Forms, elixir:env_for_eval([])),
  Expr.

cond_line_test() ->
  {'case', 1, _,
    [{clause, 2, _, _, _},
     {clause, 3, _, _, _}]
  } = to_erl("cond do\n  1 -> :ok\n  2 -> :ok\nend").

float_match_test() ->
  {'case', _, _,
    [{clause, _, [{op, _, '+', {float, _, +0.0}}], [], [{atom, _, pos}]},
     {clause, _, [{op, _, '-', {float, _, +0.0}}], [], [{atom, _, neg}]}]
  } = to_erl("case X do\n  +0.0 -> :pos\n  -0.0 -> :neg\nend").

% Optimized

optimized_if_test() ->
  {'case', _, _,
    [{clause, _, [{atom, _, false}], [], [{atom, _, 'else'}]},
     {clause, _, [{atom, _, true}], [], [{atom, _, do}]}]
  } = to_erl("if is_list([]), do: :do, else: :else").

optimized_andand_test() ->
  {'case', _, _,
    [{clause, _,
      [{var, _, Var}],
      [[{op, _, 'orelse', _, _}]],
      [{var, _, Var}]},
    {clause, _, [{var, _, '_'}], [], [{atom, 1, done}]}]
  } = to_erl("is_list([]) && :done").

optimized_oror_test() ->
  {'case', _, _,
    [{clause, 1,
      [{var, 1, _}],
      [[{op, _, 'orelse', _, _}]],
      [{atom, 1, done}]},
    {clause, 1, [{var, 1, Var}], [], [{var, 1, Var}]}]
  } = to_erl("is_list([]) || :done").

optimized_and_test() ->
  {'case',_, _,
   [{clause, _, [{atom, _, false}], [], [{atom, _, false}]},
    {clause, _, [{atom, _, true}], [], [{atom, _, done}]}]
  } = to_erl("is_list([]) and :done").

optimized_or_test() ->
  {'case', _, _,
    [{clause, _, [{atom, _, false}], [], [{atom, _, done}]},
     {clause, _, [{atom, _, true}], [], [{atom, _, true}]}]
  } = to_erl("is_list([]) or :done").

optimized_in_test() ->
  {'block', _,
    [{match,_,
      {var, _, '_'},
      {call, _, {remote, _, {atom, _, 'Elixir.IO'}, {atom, _, puts}}, [{atom, _, hi}]}},
    {atom, _, false}]
  } = to_erl("IO.puts(:hi) in []"),
  {'block', _,
    [{match,_,
      {var, _, '_1'},
      {call, _, {remote, _, {atom, _, 'Elixir.IO'}, {atom, _, puts}}, [{atom, _, hi}]}},
    {op, _, 'orelse',
      {op, _, '=:=', {var, _ , '_1'}, {integer, _, 1}},
      {op, _, '=:=', {var, _ , '_1'}, {integer, _, 2}}
      }]
  } = to_erl("IO.puts(:hi) in [1, 2]").

no_after_in_try_test() ->
  {'try', _, [_], [], [_], []} = to_erl("try do :foo.bar() catch _ -> :ok end").

optimized_inspect_interpolation_test() ->
    {bin, _,
     [{bin_element, _,
       {call, _, {remote, _,{atom, _, 'Elixir.Kernel'}, {atom, _, inspect}}, [_]},
       default, [binary]}]} = to_erl("\"#{inspect(1)}\"").

optimized_string_to_existing_atom_test() ->
  {block, _, [
   {match, _, {var, _, Var}, _},
   {'case', _, {var, _, Var},
    [{clause, _,
      [{bin, _, [{bin_element, _, {string, _, "foo"}, default, default}]}],
      [],
      [{atom, _, foo}]},
     {clause, _,
      [{bin, _, [{bin_element, _, {string, _, "bar"}, default, default}]}],
      [],
      [{atom, _, bar}]},
     {clause, _,
      [{var, _, '_'}],
      [],
      [{call, _, {remote, _, {atom, _, 'Elixir.String'}, {atom, _, '__to_existing_atom__'}}, [{var, _, Var}, _]}]}]
   }]} = to_erl("String.to_existing_atom(\"baz\", [:foo, :bar])").

optimized_list_to_existing_atom_test() ->
  {block, _, [
   {match, _, {var, _, Var}, _},
   {'case', _, {var, _, Var},
    [{clause, _,
      [{cons, _, {integer, _, $f}, {cons, _, {integer, _, $o}, {cons, _, {integer, _, $o}, {nil, _}}}}],
      [],
      [{atom, _, foo}]},
     {clause, _,
      [{cons, _, {integer, _, $b}, {cons, _, {integer, _, $a}, {cons, _, {integer, _, $r}, {nil, _}}}}],
      [],
      [{atom, _, bar}]},
     {clause, _,
      [{var, _, '_'}],
      [],
      [{call, _, {remote, _, {atom, _, 'Elixir.List'}, {atom, _, '__to_existing_atom__'}}, [{var, _, Var}, _]}]}]
   }]} = to_erl("List.to_existing_atom(~c\"baz\", [:foo, :bar])").
