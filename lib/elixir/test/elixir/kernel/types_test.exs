Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TypesTest do
  use ExUnit.Case, async: true

  import Kernel.Types, only: [context: 0]

  defmacrop quoted_pattern(expr) do
    quote do
      case Kernel.Types.of_pattern(unquote(Macro.escape(expr)), context()) do
        {:ok, type, context} -> {:ok, resolve_types(type, context)}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defmacrop quoted_clause(exprs) do
    quote do
      case Kernel.Types.of_clause(unquote(Macro.escape(exprs))) do
        {:ok, types, context} -> {:ok, Enum.map(types, &resolve_types(&1, context))}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp resolve_types({:var, var}, context) do
    case Map.fetch(context.types, var) do
      {:ok, :unbound} -> {:var, var}
      {:ok, type} -> resolve_types(type, update_in(context.types, &Map.delete(&1, var)))
      :error -> {:var, var}
    end
  end

  defp resolve_types({:tuple, types}, context) do
    {:tuple, Enum.map(types, &resolve_types(&1, context))}
  end

  defp resolve_types({:cons, left, right}, context) do
    {:cons, resolve_types(left, context), resolve_types(right, context)}
  end

  defp resolve_types({:fn, params, return}, context) do
    {:fn, Enum.map(params, &resolve_types(&1, context)), resolve_types(return, context)}
  end

  defp resolve_types(other, _context) do
    other
  end

  describe "of_pattern/1" do
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

    test "binary" do
      assert quoted_pattern(<<"foo"::binary>>) == {:ok, :binary}
      assert quoted_pattern(<<123::integer>>) == {:ok, :binary}
      assert quoted_pattern(<<foo::integer>>) == {:ok, :binary}

      assert quoted_pattern(<<123::binary>>) == {:error, {:unable_unify, :integer, :binary}}
      assert quoted_pattern(<<"foo"::integer>>) == {:error, {:unable_unify, :binary, :integer}}

      assert quoted_pattern(<<foo::binary, foo::integer>>) ==
               {:error, {:unable_unify, :integer, :binary}}
    end

    test "variables" do
      assert quoted_pattern(foo) == {:ok, {:var, 0}}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({foo, bar}) == {:ok, {:tuple, [{:var, 0}, {:var, 1}]}}
    end

    test "assignment" do
      assert quoted_pattern(x = y) == {:ok, {:var, 0}}
      assert quoted_pattern(x = 123) == {:ok, :integer}
      assert quoted_pattern({foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_pattern({x = y}) == {:ok, {:tuple, [{:var, 0}]}}

      assert quoted_pattern(x = y = 123) == {:ok, :integer}
      assert quoted_pattern(x = 123 = y) == {:ok, :integer}
      assert quoted_pattern(123 = x = y) == {:ok, :integer}

      assert quoted_pattern({x} = x) == {:error, {:recursive_type, {:tuple, [var: 0]}}}
    end
  end

  describe "of_clause/1" do
    test "various" do
      assert quoted_clause([true]) == {:ok, [{:literal, true}]}
      assert quoted_clause([foo]) == {:ok, [{:var, 0}]}
    end

    test "assignment" do
      # TODO: Should return [{:var, 0}, {:var, 0}] after they are lifted to quantified types
      assert quoted_clause([x = y, x = y]) == {:ok, [{:var, 0}, {:var, 1}]}
      assert quoted_clause([x = y, y = x]) == {:ok, [{:var, 0}, {:var, 1}]}

      assert quoted_clause([x = :foo, x = y, y = z]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([x = y, y = :foo, y = z]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([x = y, y = z, z = :foo]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([{x} = y, {y} = x]) == {:error, {:recursive_type, {:tuple, [var: 1]}}}
    end
  end
end
