-module(control_test).
-include_lib("eunit/include/eunit.hrl").

to_erl(String) ->
  Forms = elixir:'string_to_quoted!'(String, 1, 1, <<"nofile">>, []),
  {Expr, _, _} = elixir:quoted_to_erl(Forms, elixir:env_for_eval([])),
  Expr.

cond_line_test() ->
  {'case', 1, _,
    [{clause, 2, _, _, _},
     {clause, 3, _, _, _}]
  } = to_erl("cond do\n  1 -> :ok\n  2 -> :ok\nend").

% Optimized

optimized_if_test() ->
  {'case', _, _,
    [{clause, _, [{atom, _, false}], [], [{atom, _, else}]},
     {clause, _, [{atom, _, true}], [], [{atom, _, do}]}]
  } = to_erl("if is_list([]), do: :do, else: :else").

optimized_andand_test() ->
  {'case', _, _,
    [{clause, _,
      [{var, _, Var}],
      [[{op, _, 'orelse', _, _}]],
      [{var, _, Var}]},
    {clause, _, [{var, _, '_'}], [], [{atom, 0, done}]}]
  } = to_erl("is_list([]) && :done").

optimized_oror_test() ->
  {'case', _, _,
    [{clause, 1,
      [{var, 1, _}],
      [[{op, 1, 'orelse', _, _}]],
      [{atom, 0, done}]},
    {clause, 1, [{var, 1, Var}], [], [{var, 1, Var}]}]
  } = to_erl("is_list([]) || :done").

optimized_and_test() ->
  {'case',_, _,
   [{clause, _, [{atom, 0, false}], [], [{atom, 0, false}]},
    {clause, _, [{atom, 0, true}], [], [{atom, 0, done}]}]
  } = to_erl("is_list([]) and :done").

optimized_or_test() ->
  {'case', _, _,
    [{clause, _, [{atom, 0, false}], [], [{atom, 0, done}]},
     {clause, _, [{atom, 0, true}], [], [{atom, 0, true}]}]
  } = to_erl("is_list([]) or :done").

no_after_in_try_test() ->
  {'try', _, [_], [], [_], []} = to_erl("try do :foo.bar() catch _ -> :ok end").

optimized_inspect_interpolation_test() ->
    {bin, _,
     [{bin_element, _,
       {call, _, {remote, _,{atom, _, 'Elixir.Kernel'}, {atom, _, inspect}}, [_]},
       default, [binary]}]} = to_erl("\"#{inspect(1)}\"").

optimized_map_put_test() ->
  {map, _,
    [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}},
     {map_field_assoc, _, {atom, _, b}, {integer, _, 2}}]
  } = to_erl("Map.put(%{a: 1}, :b, 2)").

optimized_map_put_variable_test() ->
  {block, _,
    [_,
     {map, _, {var, _, _},
       [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}}]
     }]
  } = to_erl("x = %{}; Map.put(x, :a, 1)").

optimized_nested_map_put_variable_test() ->
  {block, _,
    [_,
     {map, _, {var, _, _},
       [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}},
        {map_field_assoc, _, {atom, _, b}, {integer, _, 2}}]
     }]
  } = to_erl("x = %{}; Map.put(Map.put(x, :a, 1), :b, 2)").

optimized_map_merge_test() ->
  {map, _,
    [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}},
     {map_field_assoc, _, {atom, _, b}, {integer, _, 2}},
     {map_field_assoc, _, {atom, _, c}, {integer, _, 3}}]
  } = to_erl("Map.merge(%{a: 1, b: 2}, %{c: 3})").

optimized_map_merge_variable_test() ->
  {block, _,
    [_,
     {map, _, {var, _, _},
       [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}}]
     }]
  } = to_erl("x = %{}; Map.merge(x, %{a: 1})").

optimized_map_update_and_merge_test() ->
  {block, _,
    [_,
     {map, _, {var, _, _},
       [{map_field_exact, _, {atom, _, a}, {integer, _, 2}},
        {map_field_assoc, _, {atom, _, b}, {integer, _, 3}}]
     }]
  } = to_erl("x = %{a: 1}; Map.merge(%{x | a: 2}, %{b: 3})"),
  {block, _,
    [_,
     {call, _, {remote, _, {atom, _, maps}, {atom, _, merge}},
       [{map, _,
          [{map_field_assoc, _, {atom, _, a}, {integer, _, 2}}]},
        {map, _, {var, _, _},
          [{map_field_exact, _, {atom, _, b}, {integer, _, 3}}]}]
     }]
  } = to_erl("x = %{a: 1}; Map.merge(%{a: 2}, %{x | b: 3})").

optimized_nested_map_merge_variable_test() ->
  {block, _,
    [_,
     {map, _, {var, _, _},
       [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}},
        {map_field_assoc, _, {atom, _, b}, {integer, _, 2}}]
     }]
  } = to_erl("x = %{}; Map.merge(Map.merge(x, %{a: 1}), %{b: 2})").
