Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.PatternTest do
  use ExUnit.Case, async: true
  import Module.Types.Infer, only: [new_var: 2]
  import Module.Types.Pattern
  alias Module.Types

  defmacrop quoted_pattern(patterns) do
    quote do
      {patterns, true} = unquote(Macro.escape(expand_head(patterns, true)))

      of_pattern(patterns, new_stack(), new_context())
      |> lift_result()
    end
  end

  defmacrop quoted_guard(vars, guards) do
    quote do
      {vars, guards} = unquote(Macro.escape(expand_head(vars, guards)))
      context = Enum.reduce(vars, new_context(), &(new_var(&1, &2) |> elem(1)))

      of_guard(guards, new_stack(), context)
    end
  end

  defp expand_head(patterns, guards) do
    {_, vars} =
      Macro.prewalk(patterns, [], fn
        {:_, _, context} = var, vars when is_atom(context) ->
          {var, vars}

        {:"::", _, [left, _right]}, vars ->
          {{:"::", [], [left, nil]}, vars}

        {name, _, context} = var, vars when is_atom(name) and is_atom(context) ->
          {var, [var | vars]}

        other, vars ->
          {other, vars}
      end)

    fun =
      quote do
        fn unquote(patterns) when unquote(guards) -> unquote(vars) end
      end

    {ast, _env} = :elixir_expand.expand(fun, __ENV__)
    {:fn, _, [{:->, _, [[{:when, _, [patterns, guards]}], _]}]} = ast
    {patterns, guards}
  end

  defp new_context() do
    Types.head_context("types_test.ex", TypesTest, {:test, 0})
  end

  defp new_stack() do
    %{
      Types.head_stack()
      | expr_stack: [{:foo, [], nil}]
    }
  end

  defp lift_result({:ok, type, context}) do
    {:ok, Types.lift_type(type, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
  end

  defmodule :"Elixir.Module.Types.InferTest.Struct" do
    defstruct foo: :atom, bar: 123, baz: %{}
  end

  describe "of_pattern/3" do
    test "error location" do
      line = __ENV__.line + 3

      assert {:error, {{:unable_unify, :integer, :binary, expr, traces}, location}} =
               quoted_pattern(<<foo::integer, foo::binary>>)

      assert location == [{"types_test.ex", line, {TypesTest, :test, 0}}]

      assert {:<<>>, _,
              [
                {:"::", _, [{:foo, _, nil}, {:integer, _, _}]},
                {:"::", _, [{:foo, _, nil}, {:binary, _, _}]}
              ]} = expr

      assert [
               {{:foo, _, nil},
                {:type, :binary, {:"::", _, [{:foo, _, nil}, {:binary, _, _}]},
                 {"types_test.ex", ^line}}},
               {{:foo, _, nil},
                {:type, :integer, {:"::", _, [{:foo, _, nil}, {:integer, _, _}]},
                 {"types_test.ex", ^line}}}
             ] = traces
    end

    test "literal" do
      assert quoted_pattern(true) == {:ok, {:atom, true}}
      assert quoted_pattern(false) == {:ok, {:atom, false}}
      assert quoted_pattern(:foo) == {:ok, {:atom, :foo}}
      assert quoted_pattern(0) == {:ok, :integer}
      assert quoted_pattern(0.0) == {:ok, :float}
      assert quoted_pattern("foo") == {:ok, :binary}
    end

    test "list" do
      assert quoted_pattern([]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([_]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([123]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123, 456]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123, _]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([_, 456]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([123 | []]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123, "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
      assert quoted_pattern([123 | ["foo"]]) == {:ok, {:list, {:union, [:integer, :binary]}}}

      # TODO: improper list?
      assert quoted_pattern([123 | 456]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123, 456 | 789]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123 | "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
      assert quoted_pattern([123 | _]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([_ | [456]]) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([_ | _]) == {:ok, {:list, :dynamic}}

      assert quoted_pattern([] ++ []) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([_] ++ _) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([123] ++ [456]) == {:ok, {:list, :integer}}
      assert quoted_pattern([123] ++ _) == {:ok, {:list, :dynamic}}
      assert quoted_pattern([123] ++ ["foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
    end

    test "tuple" do
      assert quoted_pattern({}) == {:ok, {:tuple, []}}
      assert quoted_pattern({:a}) == {:ok, {:tuple, [{:atom, :a}]}}
      assert quoted_pattern({:a, 123}) == {:ok, {:tuple, [{:atom, :a}, :integer]}}
    end

    test "map" do
      assert quoted_pattern(%{}) == {:ok, {:map, []}}
      assert quoted_pattern(%{a: :b}) == {:ok, {:map, [{{:atom, :a}, {:atom, :b}}]}}
      assert quoted_pattern(%{123 => a}) == {:ok, {:map, [{:integer, {:var, 0}}]}}

      assert quoted_pattern(%{123 => :foo, 456 => :bar}) ==
               {:ok, {:map, [{:integer, {:union, [{:atom, :bar}, {:atom, :foo}]}}]}}

      assert {:error, {{:unable_unify, :integer, {:atom, :foo}, _, _}, _}} =
               quoted_pattern(%{a: a = 123, b: a = :foo})
    end

    test "struct" do
      assert quoted_pattern(%:"Elixir.Module.Types.InferTest.Struct"{}) ==
               {:ok, {:map, [{{:atom, :__struct__}, {:atom, Module.Types.InferTest.Struct}}]}}

      assert quoted_pattern(%:"Elixir.Module.Types.InferTest.Struct"{foo: 123, bar: :atom}) ==
               {:ok,
                {:map,
                 [
                   {{:atom, :__struct__}, {:atom, Module.Types.InferTest.Struct}},
                   {{:atom, :foo}, :integer},
                   {{:atom, :bar}, {:atom, :atom}}
                 ]}}
    end

    test "struct var" do
      assert quoted_pattern(%var{}) == {:ok, {:map, [{{:atom, :__struct__}, :atom}]}}

      assert quoted_pattern(%var{foo: 123}) ==
               {:ok, {:map, [{{:atom, :__struct__}, :atom}, {{:atom, :foo}, :integer}]}}

      assert quoted_pattern(%var{foo: var}) ==
               {:ok, {:map, [{{:atom, :__struct__}, :atom}, {{:atom, :foo}, :atom}]}}
    end

    test "binary" do
      assert quoted_pattern(<<"foo"::binary>>) == {:ok, :binary}
      assert quoted_pattern(<<123::integer>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::little>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::integer>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::integer()>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::integer-little>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::little-integer>>) == {:ok, :binary}
      assert quoted_pattern(<<123::utf8>>) == {:ok, :binary}
      assert quoted_pattern(<<"foo"::utf8>>) == {:ok, :binary}

      assert quoted_pattern({<<foo::integer>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}
      assert quoted_pattern({<<foo::binary>>, foo}) == {:ok, {:tuple, [:binary, :binary]}}
      assert quoted_pattern({<<foo::utf8>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               quoted_pattern(<<foo::binary-0, foo::integer>>)
    end

    test "variables" do
      assert quoted_pattern(foo) == {:ok, {:var, 0}}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({foo, bar}) == {:ok, {:tuple, [{:var, 0}, {:var, 1}]}}

      assert quoted_pattern(_) == {:ok, :dynamic}
      assert quoted_pattern({_ = 123, _}) == {:ok, {:tuple, [:integer, :dynamic]}}
    end

    test "assignment" do
      assert quoted_pattern(x = y) == {:ok, {:var, 0}}
      assert quoted_pattern(x = 123) == {:ok, :integer}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({x = y}) == {:ok, {:tuple, [{:var, 0}]}}

      assert quoted_pattern(x = y = 123) == {:ok, :integer}
      assert quoted_pattern(x = 123 = y) == {:ok, :integer}
      assert quoted_pattern(123 = x = y) == {:ok, :integer}

      assert {:error, {{:unable_unify, {:tuple, [var: 0]}, {:var, 0}, _, _}, _}} =
               quoted_pattern({x} = x)
    end
  end

  test "of_guard/2" do
    assert {:ok, :boolean, context} = quoted_guard([x], is_tuple(x))
    assert Types.lift_type({:var, 0}, context) == :tuple

    assert {:ok, :dynamic, context} = quoted_guard([x], elem(x, 0))
    assert Types.lift_type({:var, 0}, context) == :tuple

    assert {:ok, {:atom, true}, context} = quoted_guard([], true)
    assert {:ok, {:atom, false}, context} = quoted_guard([], false)
    assert {:ok, {:atom, :fail}, context} = quoted_guard([], :fail)
    assert {:ok, :boolean, context} = quoted_guard([], is_atom(true or :fail))

    assert {:error, {_, {:unable_unify, :tuple, :boolean, _, _}, _}} =
             quoted_guard([x], is_tuple(x) and is_boolean(x))
  end
end
