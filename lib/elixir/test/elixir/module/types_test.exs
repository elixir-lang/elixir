Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types

  defmacrop quoted_clause(exprs) do
    quote do
      Types.of_clause(unquote(Macro.escape(exprs)), [], new_context())
      |> lift_result()
    end
  end

  defmacrop quoted_clause(exprs, guards) do
    quote do
      Types.of_clause(unquote(Macro.escape(exprs)), unquote(Macro.escape(guards)), new_context())
      |> lift_result()
    end
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
  end

  defp lift_result({:ok, types, context}) when is_list(types) do
    {:ok, Types.lift_types(types, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
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

      assert {:error, {{:unable_unify, {:tuple, [var: 1]}, {:var, 0}, _, _}, _}} =
               quoted_clause([{x} = y, {y} = x])
    end

    test "guards" do
      assert quoted_clause([x], [:erlang.is_binary(x)]) == {:ok, [:binary]}

      assert quoted_clause([x, y], [:erlang.andalso(:erlang.is_binary(x), :erlang.is_atom(y))]) ==
               {:ok, [:binary, :atom]}

      assert quoted_clause([x], [:erlang.orelse(:erlang.is_binary(x), :erlang.is_atom(x))]) ==
               {:ok, [{:union, [:binary, :atom]}]}

      assert quoted_clause([x, x], [:erlang.is_integer(x)]) == {:ok, [:integer, :integer]}

      assert quoted_clause([x = 123], [:erlang.is_integer(x)]) == {:ok, [:integer]}

      assert quoted_clause([x], [:erlang.orelse(:erlang.is_boolean(x), :erlang.is_atom(x))]) ==
               {:ok, [:atom]}

      assert quoted_clause([x], [:erlang.orelse(:erlang.is_atom(x), :erlang.is_boolean(x))]) ==
               {:ok, [:atom]}

      assert quoted_clause([x], [:erlang.andalso(:erlang.is_boolean(x), :erlang.is_atom(x))]) ==
               {:ok, [:boolean]}

      assert quoted_clause([x], [:erlang.andalso(:erlang.is_atom(x), :erlang.is_boolean(x))]) ==
               {:ok, [:boolean]}

      assert quoted_clause([x, x = y, y = z], [:erlang.is_atom(x)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_clause([x = y, y, y = z], [:erlang.is_atom(y)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_clause([x = y, y = z, z], [:erlang.is_atom(z)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               quoted_clause([x], [:erlang.andalso(:erlang.is_binary(x), :erlang.is_integer(x))])
    end

    test "map" do
      assert quoted_clause([%{true: false} = foo, %{} = foo]) ==
               {:ok,
                [
                  {:map, [{{:literal, true}, {:literal, false}}]},
                  {:map, [{{:literal, true}, {:literal, false}}]}
                ]}

      assert quoted_clause([%{true: bool}], [:erlang.is_boolean(bool)]) ==
               {:ok,
                [
                  {:map, [{{:literal, true}, :boolean}]}
                ]}

      assert quoted_clause([%{true: true} = foo, %{false: false} = foo]) ==
               {:ok,
                [
                  {:map,
                   [{{:literal, false}, {:literal, false}}, {{:literal, true}, {:literal, true}}]},
                  {:map,
                   [{{:literal, false}, {:literal, false}}, {{:literal, true}, {:literal, true}}]}
                ]}

      assert {:error, {{:unable_unify, {:literal, true}, {:literal, false}, _, _}, _}} =
               quoted_clause([%{true: false} = foo, %{true: true} = foo])
    end
  end

  test "format_type/1" do
    assert Types.format_type(:binary) == "binary()"
    assert Types.format_type({:literal, true}) == "true"
    assert Types.format_type({:literal, :atom}) == ":atom"
    assert Types.format_type({:cons, :binary, :null}) == "[binary()]"
    assert Types.format_type({:cons, :binary, :binary}) == "[binary() | binary()]"
    assert Types.format_type({:tuple, []}) == "{}"
    assert Types.format_type({:tuple, [:integer]}) == "{integer()}"
    assert Types.format_type({:map, []}) == "%{}"
    assert Types.format_type({:map, [{:integer, :atom}]}) == "%{integer() => atom()}"
    assert Types.format_type({:map, [{:__struct__, Struct}]}) == "%Struct{}"

    assert Types.format_type({:map, [{:__struct__, Struct}, {:integer, :atom}]}) ==
             "%Struct{integer() => atom()}"
  end
end
