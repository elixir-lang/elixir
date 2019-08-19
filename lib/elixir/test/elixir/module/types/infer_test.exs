Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.InferTest do
  use ExUnit.Case, async: true
  alias Module.Types
  alias Module.Types.Infer

  defmacrop quoted_pattern(expr) do
    quote do
      Infer.of_pattern(unquote(Macro.escape(expr)), new_context())
      |> lift_result()
    end
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
  end

  defp lift_result({:ok, type, context}) do
    {:ok, Types.lift_type(type, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
  end

  describe "of_pattern/2" do
    test "error location" do
      assert {:error, {{:unable_unify, :binary, :integer, expr, traces}, location}} =
               quoted_pattern(<<foo::integer, foo::binary>>)

      assert location == [{"types_test.ex", 35, {TypesTest, :test, 0}}]

      assert {:<<>>, _,
              [
                {:"::", _, [{:foo, _, nil}, {:integer, _, nil}]},
                {:"::", _, [{:foo, _, nil}, {:binary, _, nil}]}
              ]} = expr

      assert [
               {{:foo, _, nil},
                {:type, :binary, {:"::", _, [{:foo, _, nil}, {:binary, _, nil}]},
                 {"types_test.ex", 35}}},
               {{:foo, _, nil},
                {:type, :integer, {:"::", _, [{:foo, _, nil}, {:integer, _, nil}]},
                 {"types_test.ex", 35}}}
             ] = traces
    end

    test "literals" do
      assert quoted_pattern(true) == {:ok, {:literal, true}}
      assert quoted_pattern(false) == {:ok, {:literal, false}}
      assert quoted_pattern(:foo) == {:ok, {:literal, :foo}}
      assert quoted_pattern(0) == {:ok, :integer}
      assert quoted_pattern(0.0) == {:ok, :float}
      assert quoted_pattern("foo") == {:ok, :binary}
    end

    test "list" do
      assert quoted_pattern([]) == {:ok, :null}
      assert quoted_pattern([123]) == {:ok, {:cons, :integer, :null}}
      assert quoted_pattern([123 | []]) == {:ok, {:cons, :integer, :null}}
      assert quoted_pattern([123 | 456]) == {:ok, {:cons, :integer, :integer}}

      assert quoted_pattern([123, 456 | 789]) ==
               {:ok, {:cons, :integer, {:cons, :integer, :integer}}}
    end

    test "tuple" do
      assert quoted_pattern({}) == {:ok, {:tuple, []}}
      assert quoted_pattern({:a}) == {:ok, {:tuple, [{:literal, :a}]}}
      assert quoted_pattern({:a, 123}) == {:ok, {:tuple, [{:literal, :a}, :integer]}}
    end

    test "map" do
      assert quoted_pattern(%{}) == {:ok, {:map, []}}
      assert quoted_pattern(%{a: :b}) == {:ok, {:map, [{{:literal, :a}, {:literal, :b}}]}}
      assert quoted_pattern(%{123 => a}) == {:ok, {:map, [{:integer, {:var, 0}}]}}

      assert {:error, {{:unable_unify, {:literal, :foo}, :integer, _, _}, _}} =
               quoted_pattern(%{a: a = 123, b: a = :foo})
    end

    test "binary" do
      assert quoted_pattern(<<"foo"::binary>>) == {:ok, :binary}
      assert quoted_pattern(<<123::integer>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::integer>>) == {:ok, :binary}

      assert quoted_pattern({<<foo::integer>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}
      assert quoted_pattern({<<foo::binary>>, foo}) == {:ok, {:tuple, [:binary, :binary]}}

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               quoted_pattern(<<123::binary>>)

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               quoted_pattern(<<"foo"::integer>>)

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               quoted_pattern(<<foo::binary, foo::integer>>)
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
end
