Code.require_file "../test_helper.exs", __DIR__
alias IEx.Tracer.Pattern

defmodule Mod do
  def test(a, b \\ [], c \\ []), do: {a, b, c}
end

defmodule IEx.Tracer.PatternTest do
  use ExUnit.Case, async: true

  test :module do
    assert Pattern.compile((quote do: :lists), [:exported]) |> with_options == {{:lists,:_,:_}, :_, [], []}
    assert Pattern.compile((quote do: File), [:exported]) |> with_options   == {{File,:_,:_}, :_, [], []}
    assert Pattern.compile((quote do: File), []) |> with_options            == {{File,:_,:_}, :_, [], [:local]}
  end

  test :module_function do
    assert Pattern.compile(quote do: :lists.map)  |> simple == {{:lists,:map,:_}, :_, []}
    assert Pattern.compile(quote do: File.open)   |> simple == {{File,:open,:_}, :_, []}
    assert Pattern.compile(quote do: File.open/3) |> simple == {{File,:open,3}, :_, []}
  end

  test :mfa_represented_semself do
    assert Pattern.compile(quote do: Mod.test([1,2,3])) |> simple == {{Mod,:test,:_}, [[1,2,3]], []}
    assert Pattern.compile(quote do: Mod.test(1, 2, 3)) |> simple == {{Mod,:test,:_}, [1,2,3], []}
  end

  test :mfa_with_ast do
    assert Pattern.compile(quote do: Mod.test({1,2,3})) |> simple == {{Mod,:test,:_}, [{1,2,3}], []}
  end

  test :mfa_with_variables do
    assert Pattern.compile(quote do: Mod.test(a,2,3)) |> simple == {{Mod,:test,:_}, [:'$1',2,3], []}
    assert Pattern.compile(quote do: Mod.test({a,2,3})) |> simple == {{Mod,:test,:_}, [{:'$1',2,3}], []}
    assert Pattern.compile(quote do: Mod.test({a,b,3}, c)) |> simple == {{Mod,:test,:_}, [{:'$1',:'$2',3}, :'$3'], []}
  end


  test :mfa_with_condition_is do
    guards = [:is_atom, :is_binary, :is_list, :is_float, :is_integer, :is_boolean,
              :is_number, :is_map, :is_tuple, :is_pid, :is_port, :is_reference]
    for guard <- guards do
      assert Pattern.compile(quote do: Mod.test(a) when unquote(guard)(a)) |> simple == {{Mod,:test,:_}, [:'$1'], [{guard, :'$1'}]}
    end
  end

  test :complex_pattern do
    assert Pattern.compile(quote do: Mod.test(a, b, 1) when is_atom(a) or (is_binary(a) and is_tuple(b) and (elem(b, 0) == N))) |> simple
      == {{Mod, :test, :_}, [:'$1', :'$2', 1], [{:orelse, {:is_atom, :'$1'},
         {:andalso, {:andalso, {:is_binary, :'$1'}, {:is_tuple, :'$2'}}, {:==, {:element,1,:'$2'},N}}}]}
  end

  test :inject_variable do
    a = 1
    assert Pattern.compile(quote do: Mod.test(b) when b == a) |> simple
      == {{Mod,:test,:_}, [:'$1'], [{:==, :"$1", {:unquote, [], [{:a, [], IEx.Tracer.PatternTest}]}}]}
  end

  defp simple({mfa, [{args, conditions, _}], _}), do: {mfa, args, conditions}
  defp with_options({mfa, [{args, conditions, _}], options}), do: {mfa, args, conditions, options}
end

