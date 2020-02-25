Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true
  import Module.Types.Expr
  alias Module.Types

  defmacrop quoted_expr(vars \\ [], body) do
    quote do
      unquote(Macro.escape(expand_expr(vars, body)))
      |> of_expr(new_stack(), new_context())
      |> lift_result()
    end
  end

  defp expand_expr(vars, expr) do
    fun =
      quote do
        fn unquote(vars) -> unquote(expr) end
      end

    {ast, _env} = :elixir_expand.expand(fun, __ENV__)
    {:fn, _, [{:->, _, [[_vars], body]}]} = ast
    body
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
  end

  defp new_stack() do
    %{
      Types.stack(:expr)
      | last_expr: {:foo, [], nil}
    }
  end

  defp lift_result({:ok, type, context}) do
    {:ok, Types.lift_type(type, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
  end

  defmodule :"Elixir.Module.Types.ExprTest.Struct" do
    defstruct foo: :atom, bar: 123, baz: %{}
  end

  describe "of_expr/3" do
    test "literal" do
      assert quoted_expr(true) == {:ok, {:atom, true}}
      assert quoted_expr(false) == {:ok, {:atom, false}}
      assert quoted_expr(:foo) == {:ok, {:atom, :foo}}
      assert quoted_expr(0) == {:ok, :integer}
      assert quoted_expr(0.0) == {:ok, :float}
      assert quoted_expr("foo") == {:ok, :binary}
    end

    test "list" do
      assert quoted_expr([]) == {:ok, {:list, :dynamic}}
      assert quoted_expr([123]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, 456]) == {:ok, {:list, :integer}}
      assert quoted_expr([123 | []]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
      assert quoted_expr([123 | ["foo"]]) == {:ok, {:list, {:union, [:integer, :binary]}}}

      # TODO: improper list?
      assert quoted_expr([123 | 456]) == {:ok, {:list, :integer}}
      assert quoted_expr([123, 456 | 789]) == {:ok, {:list, :integer}}
      assert quoted_expr([123 | "foo"]) == {:ok, {:list, {:union, [:integer, :binary]}}}
    end

    test "tuple" do
      assert quoted_expr({}) == {:ok, {:tuple, []}}
      assert quoted_expr({:a}) == {:ok, {:tuple, [{:atom, :a}]}}
      assert quoted_expr({:a, 123}) == {:ok, {:tuple, [{:atom, :a}, :integer]}}
    end

    test "map" do
      assert quoted_expr(%{}) == {:ok, {:map, []}}
      assert quoted_expr(%{a: :b}) == {:ok, {:map, [{{:atom, :a}, {:atom, :b}}]}}
      assert quoted_expr([a], %{123 => a}) == {:ok, {:map, [{:integer, {:var, 0}}]}}

      assert quoted_expr(%{123 => :foo, 456 => :bar}) ==
               {:ok, {:map, [{:integer, {:union, [{:atom, :bar}, {:atom, :foo}]}}]}}
    end

    test "struct" do
      assert quoted_expr(%:"Elixir.Module.Types.ExprTest.Struct"{}) ==
               {:ok,
                {:map,
                 [
                   {{:atom, :__struct__}, {:atom, Module.Types.ExprTest.Struct}},
                   {{:atom, :bar}, :integer},
                   {{:atom, :baz}, {:map, []}},
                   {{:atom, :foo}, {:atom, :atom}}
                 ]}}

      assert quoted_expr(%:"Elixir.Module.Types.ExprTest.Struct"{foo: 123, bar: :atom}) ==
               {:ok,
                {:map,
                 [
                   {{:atom, :__struct__}, {:atom, Module.Types.ExprTest.Struct}},
                   {{:atom, :baz}, {:map, []}},
                   {{:atom, :foo}, :integer},
                   {{:atom, :bar}, {:atom, :atom}}
                 ]}}
    end

    test "binary" do
      assert quoted_expr(<<"foo"::binary>>) == {:ok, :binary}
      assert quoted_expr(<<123::integer>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::little>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer()>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::integer-little>>) == {:ok, :binary}
      assert quoted_expr([foo], <<foo::little-integer>>) == {:ok, :binary}
      assert quoted_expr(<<123::utf8>>) == {:ok, :binary}
      assert quoted_expr(<<"foo"::utf8>>) == {:ok, :binary}

      assert quoted_expr([foo], {<<foo::integer>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}
      assert quoted_expr([foo], {<<foo::binary>>, foo}) == {:ok, {:tuple, [:binary, :binary]}}

      assert quoted_expr([foo], {<<foo::utf8>>, foo}) ==
               {:ok, {:tuple, [:binary, {:union, [:integer, :binary]}]}}

      assert quoted_expr(
               (
                 foo = 0.0
                 <<foo::float>>
               )
             ) == {:ok, :binary}

      assert quoted_expr(
               (
                 foo = 0
                 <<foo::float>>
               )
             ) == {:ok, :binary}

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               quoted_expr(
                 (
                   foo = 0
                   <<foo::binary>>
                 )
               )

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               quoted_expr([foo], <<foo::binary-0, foo::integer>>)
    end

    test "variables" do
      assert quoted_expr([foo], foo) == {:ok, {:var, 0}}
      assert quoted_expr([foo], {foo}) == {:ok, {:tuple, [{:var, 0}]}}
      assert quoted_expr([foo, bar], {foo, bar}) == {:ok, {:tuple, [{:var, 0}, {:var, 1}]}}
    end

    test "pattern match" do
      assert {:error, _} = quoted_expr(:foo = 1)
      assert {:error, _} = quoted_expr(1 = :foo)

      assert quoted_expr(:foo = :foo) == {:ok, {:atom, :foo}}
      assert quoted_expr(1 = 1) == {:ok, :integer}
    end

    test "block" do
      assert quoted_expr(
               (
                 a = 1
                 a
               )
             ) == {:ok, :integer}

      assert quoted_expr(
               (
                 a = :foo
                 a
               )
             ) == {:ok, {:atom, :foo}}

      assert {:error, _} =
               quoted_expr(
                 (
                   a = 1
                   :foo = a
                 )
               )
    end

    test "case" do
      assert {:error, _} = quoted_expr([a], case(a, do: (:foo = b -> :bar = b)))

      assert quoted_expr([a], case(a, do: (:foo = b -> :foo = b))) == {:ok, :dynamic}
    end

    test "fn" do
      assert {:error, _} = quoted_expr(fn :foo = b -> :bar = b end)

      assert quoted_expr(fn :foo = b -> :foo = b end) == {:ok, :dynamic}
    end
  end
end
