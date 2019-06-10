Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TypesTest do
  use ExUnit.Case, async: true

  import Kernel.Types, only: [context: 0]

  defmacrop quoted_pattern(expr) do
    quote do
      case Kernel.Types.of_pattern(unquote(Macro.escape(expr)), context()) do
        {:ok, type, context} ->
          {type, _context} = Kernel.Types.lift_types(type, context)
          {:ok, type}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defmacrop quoted_clause(exprs) do
    quote do
      case Kernel.Types.of_clause(unquote(Macro.escape(exprs)), context()) do
        {:ok, types, context} ->
          {types, _context} = Enum.map_reduce(types, context, &Kernel.Types.lift_types/2)
          {:ok, types}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  describe "of_pattern/2" do
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

  describe "of_clause/2" do
    test "various" do
      assert quoted_clause([true]) == {:ok, [{:literal, true}]}
      assert quoted_clause([foo]) == {:ok, [{:var, 0}]}
    end

    test "assignment" do
      assert quoted_clause([x = y, x = y]) == {:ok, [{:var, 0}, {:var, 0}]}
      assert quoted_clause([x = y, y = x]) == {:ok, [{:var, 0}, {:var, 0}]}

      assert quoted_clause([x = :foo, x = y, y = z]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([x = y, y = :foo, y = z]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([x = y, y = z, z = :foo]) ==
               {:ok, [{:literal, :foo}, {:literal, :foo}, {:literal, :foo}]}

      assert quoted_clause([{x} = y, {y} = x]) == {:error, {:recursive_type, {:tuple, [var: 1]}}}
    end

    test "guards" do
      assert quoted_clause([x] when is_binary(x)) == {:ok, [:binary]}
      assert quoted_clause([x, y] when is_binary(x) and is_atom(y)) == {:ok, [:binary, :atom]}

      assert quoted_clause([x] when is_binary(x) or is_atom(x)) ==
               {:ok, [{:union, [:binary, :atom]}]}

      assert quoted_clause([x, x] when is_integer(x)) ==
               {:ok, [:integer, :integer]}

      assert quoted_clause([x, x = y, y = z] when is_atom(x)) == {:ok, [:atom, :atom, :atom]}
      assert quoted_clause([x = y, y, y = z] when is_atom(y)) == {:ok, [:atom, :atom, :atom]}
      assert quoted_clause([x = y, y = z, z] when is_atom(z)) == {:ok, [:atom, :atom, :atom]}

      assert quoted_clause([x] when is_binary(x) and is_integer(x)) ==
               {:error, {:unable_unify, :integer, :binary}}
    end
  end
end
