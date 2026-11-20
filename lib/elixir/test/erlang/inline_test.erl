%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team

-module(inline_test).
-include_lib("eunit/include/eunit.hrl").

to_erl(String) ->
  Forms = elixir:'string_to_quoted!'(String, 1, 1, <<"nofile">>, []),
  {Expr, _, _, _} = elixir:quoted_to_erl(Forms, elixir:env_for_eval([])),
  Expr.

with_tuple_enumerable(Fun) ->
  test_helper:run_and_remove(fun() ->
    'Elixir.Code':eval_string(<<"
      defimpl Enumerable, for: Tuple do
        def reduce(tuple, acc, fun), do: Enumerable.List.reduce(Tuple.to_list(tuple), acc, fun)
        def count(_), do: {:error, __MODULE__}
        def member?(_, _), do: {:error, __MODULE__}
        def slice(_), do: {:error, __MODULE__}
      end
    ">>),
    Fun()
  end, ['Elixir.Enumerable.Tuple']).

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
     {map, _,
       {map, _, {var, _, _},
         [{map_field_exact, _, {atom, _, a}, {integer, _, 2}}]},
       [{map_field_assoc, _, {atom, _, b}, {integer, _, 3}}]
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
     {map, _,
       {map, _, {var, _, _},
         [{map_field_assoc, _, {atom, _, a}, {integer, _, 1}}]},
       [{map_field_assoc, _, {atom, _, b}, {integer, _, 2}}]
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

not_optimized_map_set_new_with_tuple_test() ->
  with_tuple_enumerable(fun() ->
    {call, _,
      {remote, _, {atom, _, 'Elixir.MapSet'}, {atom, _, new}},
      [{tuple, _, [{integer, _, 1}, {integer, _, 2}]}]
    } = to_erl("MapSet.new({1, 2})")
  end).

not_optimized_duration_new_with_tuple_test() ->
  with_tuple_enumerable(fun() ->
    {call, _,
      {remote, _, {atom, _, 'Elixir.Duration'}, {atom, _, 'new!'}},
      [{tuple, _, [{tuple, _, [{atom, _, week}, {integer, _, 1}]}, {tuple, _, [{atom, _, day}, {integer, _, 2}]}]}]
    } = to_erl("Duration.new!({{:week, 1}, {:day, 2}})")
  end).

not_optimized_date_shift_duration_with_tuple_test() ->
  with_tuple_enumerable(fun() ->
    {call, _,
      {remote, _, {atom, _, 'Elixir.Date'}, {atom, _, shift}}, [
        {atom, _, non_important},
        {tuple, _, [{tuple, _, [{atom, _, week}, {integer, _, 1}]}, {tuple, _, [{atom, _, day}, {integer, _, 2}]}]}
      ]
    } = to_erl("Date.shift(:non_important, {{:week, 1}, {:day, 2}})")
  end).
