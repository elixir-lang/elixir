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
      [[{op, 1, 'orelse', _, _}]],
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

optimized_map_set_new_test() ->
  {map, _,
    [
      {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.MapSet'}},
      {map_field_assoc, _,
        {atom, _, map},
        {map, _, [
          {map_field_assoc, _, {integer, _, 1}, {nil, _}},
          {map_field_assoc, _, {integer, _, 2}, {nil, _}},
          {map_field_assoc, _, {integer, _, 3}, {nil, _}}
        ]}
      }
    ]
  } = to_erl("MapSet.new([1, 2, 3])").

not_optimized_map_set_new_with_range_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.MapSet'}, {atom, _, new}}, [
      {map, _, [
        {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.Range'}},
        {map_field_assoc, _, {atom, _, first}, {integer, _, 1}},
        {map_field_assoc, _, {atom, _, last}, {integer, _, 3}},
        {map_field_assoc, _, {atom, _, step}, {integer, _, 1}}
      ]}
    ]
  } = to_erl("MapSet.new(1..3)").

map_set_new_with_failing_args_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.MapSet'}, {atom, _, new}}, [
      {atom, _, not_an_enumerable}
    ]
  } = to_erl("MapSet.new(:not_an_enumerable)").

optimized_date_shift_duration_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.Date'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {map, _, [
        {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.Duration'}},
        {map_field_assoc, _, {atom, _, day}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, hour}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, microsecond}, {tuple, _, [{integer, _, 0}, {integer, _, 0}]}},
        {map_field_assoc, _, {atom, _, minute}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, month}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, second}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, week}, {integer, _, 1}},
        {map_field_assoc, _, {atom, _, year}, {integer, _, 0}}
      ]}
    ]
  } = to_erl("Date.shift(:non_important, week: 1)").

not_optimized_date_shift_duration_unsupported_unit_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.Date'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {cons, _, {tuple, _, [{atom, _, hour}, {integer, _, 1}]}, {nil, _}}
    ]
  } = to_erl("Date.shift(:non_important, hour: 1)").

optimized_time_shift_duration_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.Time'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {map, _, [
        {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.Duration'}},
        {map_field_assoc, _, {atom, _, day}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, hour}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, microsecond}, {tuple, _, [{integer, _, 0}, {integer, _, 0}]}},
        {map_field_assoc, _, {atom, _, minute}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, month}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, second}, {integer, _, 2}},
        {map_field_assoc, _, {atom, _, week}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, year}, {integer, _, 0}}
      ]}
    ]
  } = to_erl("Time.shift(:non_important, second: 2)").

not_optimized_time_shift_duration_unsupported_unit_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.Time'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {cons, _, {tuple, _, [{atom, _, day}, {integer, _, 2}]}, {nil, _}}
    ]
  } = to_erl("Time.shift(:non_important, day: 2)").

optimized_date_time_shift_duration_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.DateTime'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {map, _, [
        {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.Duration'}},
        {map_field_assoc, _, {atom, _, day}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, hour}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, microsecond}, {tuple, _, [{integer, _, 0}, {integer, _, 0}]}},
        {map_field_assoc, _, {atom, _, minute}, {integer, _, 3}},
        {map_field_assoc, _, {atom, _, month}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, second}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, week}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, year}, {integer, _, 0}}
      ]}
    ]
  } = to_erl("DateTime.shift(:non_important, minute: 3)").

non_optimized_date_time_shift_duration_unknown_unit_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.DateTime'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {cons, _, {tuple, _, [{atom, _, unknown}, {integer, _, 3}]}, {nil, _}}
    ]
  } = to_erl("DateTime.shift(:non_important, unknown: 3)").

optimized_naive_date_time_shift_duration_test() ->
  {call, _,
    {remote, _, {atom, _, 'Elixir.NaiveDateTime'}, {atom, _, shift}}, [
      {atom, _, non_important},
      {map, _, [
        {map_field_assoc, _, {atom, _, '__struct__'}, {atom, _, 'Elixir.Duration'}},
        {map_field_assoc, _, {atom, _, day}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, hour}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, microsecond}, {tuple, _, [{integer, _, 0}, {integer, _, 0}]}},
        {map_field_assoc, _, {atom, _, minute}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, month}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, second}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, week}, {integer, _, 0}},
        {map_field_assoc, _, {atom, _, year}, {integer, _, 4}}
      ]}
    ]
  } = to_erl("NaiveDateTime.shift(:non_important, year: 4)").