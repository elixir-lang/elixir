Code.require_file("../test_helper.exs", __DIR__)

defmodule Module.TypesTest do
  use ExUnit.Case, async: true
  import Bitwise, warn: false
  alias Module.Types

  defmacrop quoted_clause(exprs) do
    quote do
      Types.of_clause(unquote(Macro.escape(exprs)), [], new_stack(), new_context())
      |> lift_result()
    end
  end

  defmacrop quoted_clause(exprs, guards) do
    quote do
      Types.of_clause(
        unquote(Macro.escape(exprs)),
        unquote(Macro.escape(expand_guards(exprs, guards))),
        new_stack(),
        new_context()
      )
      |> lift_result()
    end
  end

  defp expand_guards(exprs, guards) do
    {_, vars} =
      Macro.prewalk(exprs, [], fn
        {name, _, context} = var, vars when is_atom(name) and is_atom(context) ->
          {var, [var | vars]}

        other, vars ->
          {other, vars}
      end)

    fun =
      quote do
        fn unquote(vars) when unquote(guards) -> unquote(vars) end
      end

    {ast, _env} = :elixir_expand.expand(fun, __ENV__)
    {:fn, _, [{:->, _, [[{:when, _, [_, guards]}], _]}]} = ast
    guards
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
  end

  defp new_stack() do
    Types.stack()
  end

  defp lift_result({:ok, types, context}) when is_list(types) do
    {:ok, Types.lift_types(types, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
  end

  describe "of_clause/2" do
    test "various" do
      assert quoted_clause([true]) == {:ok, [{:atom, true}]}
      assert quoted_clause([foo]) == {:ok, [{:var, 0}]}
    end

    test "variable" do
      assert quoted_clause([a]) == {:ok, [{:var, 0}]}
      assert quoted_clause([a, b]) == {:ok, [{:var, 0}, {:var, 1}]}
      assert quoted_clause([a, a]) == {:ok, [{:var, 0}, {:var, 0}]}
      assert quoted_clause([a, a]) == {:ok, [{:var, 0}, {:var, 0}]}

      assert lift_result(
               Types.of_clause([{:a, [], :foo}, {:a, [], :foo}], [], new_stack(), new_context())
             ) == {:ok, [{:var, 0}, {:var, 0}]}

      assert lift_result(
               Types.of_clause([{:a, [], :foo}, {:a, [], :bar}], [], new_stack(), new_context())
             ) == {:ok, [{:var, 0}, {:var, 1}]}

      assert lift_result(
               Types.of_clause(
                 [{:a, [counter: 0], :foo}, {:a, [counter: 1], :foo}],
                 [],
                 new_stack(),
                 new_context()
               )
             ) == {:ok, [{:var, 0}, {:var, 1}]}
    end

    test "assignment" do
      assert quoted_clause([x = y, x = y]) == {:ok, [{:var, 0}, {:var, 0}]}
      assert quoted_clause([x = y, y = x]) == {:ok, [{:var, 0}, {:var, 0}]}

      assert quoted_clause([x = :foo, x = y, y = z]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert quoted_clause([x = y, y = :foo, y = z]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert quoted_clause([x = y, y = z, z = :foo]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert {:error, {{:unable_unify, {:tuple, [var: 1]}, {:var, 0}, _, _}, _}} =
               quoted_clause([{x} = y, {y} = x])
    end

    test "guards" do
      assert quoted_clause([x], [is_binary(x)]) == {:ok, [:binary]}

      assert quoted_clause([x, y], [is_binary(x) and is_atom(y)]) ==
               {:ok, [:binary, :atom]}

      assert quoted_clause([x], [is_binary(x) or is_atom(x)]) ==
               {:ok, [{:union, [:binary, :atom]}]}

      assert quoted_clause([x, x], [is_integer(x)]) == {:ok, [:integer, :integer]}

      assert quoted_clause([x = 123], [is_integer(x)]) == {:ok, [:integer]}

      assert quoted_clause([x], [is_boolean(x) or is_atom(x)]) ==
               {:ok, [:atom]}

      assert quoted_clause([x], [is_atom(x) or is_boolean(x)]) ==
               {:ok, [:atom]}

      assert quoted_clause([x], [is_tuple(x) or is_atom(x)]) ==
               {:ok, [{:union, [:tuple, :atom]}]}

      assert quoted_clause([x], [is_boolean(x) and is_atom(x)]) ==
               {:ok, [:boolean]}

      assert quoted_clause([x], [is_atom(x) and is_boolean(x)]) ==
               {:ok, [:boolean]}

      assert quoted_clause([x], [is_atom(x) > :foo]) == {:ok, [var: 0]}

      assert quoted_clause([x, x = y, y = z], [is_atom(x)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_clause([x = y, y, y = z], [is_atom(y)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_clause([x = y, y = z, z], [is_atom(z)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               quoted_clause([x], [is_binary(x) and is_integer(x)])

      assert {:error, {{:unable_unify, :atom, :tuple, _, _}, _}} =
               quoted_clause([x], [is_tuple(x) and is_atom(x)])

      assert {:error, {{:unable_unify, :tuple, :boolean, _, _}, _}} =
               quoted_clause([x], [is_tuple(is_atom(x))])
    end

    test "failing guard functions" do
      assert quoted_clause([x], [length([])]) == {:ok, [{:var, 0}]}

      assert {:error, {{:unable_unify, {:list, :dynamic}, {:atom, :foo}, _, _}, _}} =
               quoted_clause([x], [length(:foo)])

      assert {:error, {{:unable_unify, {:list, :dynamic}, :boolean, _, _}, _}} =
               quoted_clause([x], [length(is_tuple(x))])

      assert {:error, {{:unable_unify, :tuple, :boolean, _, _}, _}} =
               quoted_clause([x], [elem(is_tuple(x), 0)])

      assert {:error, {{:unable_unify, :number, :boolean, _, _}, _}} =
               quoted_clause([x], [elem({}, is_tuple(x))])

      assert quoted_clause([x], [elem({}, 1)]) == {:ok, [var: 0]}

      assert quoted_clause([x], [elem(x, 1) == :foo]) == {:ok, [:tuple]}

      assert quoted_clause([x], [is_tuple(x) and elem(x, 1)]) == {:ok, [:tuple]}

      assert quoted_clause([x], [length(x) == 0 or elem(x, 1)]) == {:ok, [{:list, :dynamic}]}

      assert quoted_clause([x], [
               (is_list(x) and length(x) == 0) or (is_tuple(x) and elem(x, 1))
             ]) ==
               {:ok, [{:union, [{:list, :dynamic}, :tuple]}]}

      assert quoted_clause([x], [
               (length(x) == 0 and is_list(x)) or (elem(x, 1) and is_tuple(x))
             ]) == {:ok, [{:list, :dynamic}]}

      assert quoted_clause([x, y], [elem(x, 1) and is_atom(y)]) ==
               {:ok, [:tuple, :atom]}

      assert quoted_clause([x], [elem(x, 1) or is_atom(x)]) ==
               {:ok, [:tuple]}

      assert quoted_clause([x, y], [elem(x, 1) or is_atom(y)]) ==
               {:ok, [:tuple, {:var, 0}]}

      assert {:error, {{:unable_unify, :atom, :tuple, _, _}, _}} =
               quoted_clause([x], [elem(x, 1) and is_atom(x)])
    end

    test "inverse guards" do
      assert quoted_clause([x], [not is_tuple(x)]) ==
               {:ok,
                [
                  {:union,
                   [
                     :atom,
                     :binary,
                     :float,
                     :fun,
                     :integer,
                     {:list, :dynamic},
                     {:map, []},
                     :pid,
                     :port,
                     :reference
                   ]}
                ]}

      assert quoted_clause([x], [not (not is_tuple(x))]) ==
               {:ok, [:tuple]}

      assert quoted_clause([x], [not elem(x, 0)]) ==
               {:ok, [:tuple]}

      assert quoted_clause([x], [not (is_tuple(x) and elem(x, 0))]) == {:ok, [{:var, 0}]}

      assert quoted_clause([x], [not (elem(x, 0) and is_tuple(x))]) == {:ok, [:tuple]}

      # TODO: Requires lifting unions to unification
      # assert quoted_clause([x], [not(is_tuple(x)) and not(is_list(x))]) == {
      #          :ok,
      #          [
      #            {:union,
      #             [
      #               :atom,
      #               :binary,
      #               :float,
      #               :fun,
      #               :integer,
      #               {:map, []},
      #               :pid,
      #               :port,
      #               :reference
      #             ]}
      #          ]
      #        }

      assert quoted_clause([x], [not (is_tuple(x) or is_list(x))]) == {
               :ok,
               [
                 {:union,
                  [
                    :atom,
                    :binary,
                    :float,
                    :fun,
                    :integer,
                    {:map, []},
                    :pid,
                    :port,
                    :reference
                  ]}
               ]
             }

      assert quoted_clause([x], [not is_tuple(x) or not is_list(x)]) == {
               :ok,
               [
                 {:union,
                  [
                    :atom,
                    :binary,
                    :float,
                    :fun,
                    :integer,
                    {:list, :dynamic},
                    {:map, []},
                    :pid,
                    :port,
                    :reference,
                    :tuple
                  ]}
               ]
             }

      assert quoted_clause([x], [is_integer(x) and not is_binary(x)]) == {:ok, [:integer]}

      assert quoted_clause([x, y], [is_integer(x) and not is_binary(y)]) ==
               {:ok,
                [
                  :integer,
                  {:union,
                   [
                     :atom,
                     :float,
                     :fun,
                     :integer,
                     {:list, :dynamic},
                     {:map, []},
                     :pid,
                     :port,
                     :reference,
                     :tuple
                   ]}
                ]}

      assert quoted_clause([x], [is_atom(x) and not (is_integer(x) and band(x, 1) == 1)]) ==
               {:ok, [:atom]}

      assert {:error, {{:unable_unify, {:list, :dynamic}, :tuple, _, _}, _}} =
               quoted_clause([x], [
                 not (is_tuple(x) and is_list(x))
               ])
    end

    test "map" do
      assert quoted_clause([%{true: false} = foo, %{} = foo]) ==
               {:ok,
                [
                  {:map, [{{:atom, true}, {:atom, false}}]},
                  {:map, [{{:atom, true}, {:atom, false}}]}
                ]}

      assert quoted_clause([%{true: bool}], [is_boolean(bool)]) ==
               {:ok,
                [
                  {:map, [{{:atom, true}, :boolean}]}
                ]}

      assert quoted_clause([%{true: true} = foo, %{false: false} = foo]) ==
               {:ok,
                [
                  {:map, [{{:atom, false}, {:atom, false}}, {{:atom, true}, {:atom, true}}]},
                  {:map, [{{:atom, false}, {:atom, false}}, {{:atom, true}, {:atom, true}}]}
                ]}

      assert {:error, {{:unable_unify, {:atom, true}, {:atom, false}, _, _}, _}} =
               quoted_clause([%{true: false} = foo, %{true: true} = foo])
    end

    test "struct var guard" do
      assert quoted_clause([%var{}], [is_atom(var)]) ==
               {:ok, [{:map, [{{:atom, :__struct__}, :atom}]}]}

      assert {:error, {{:unable_unify, :integer, :atom, _, _}, _}} =
               quoted_clause([%var{}], [is_integer(var)])
    end
  end

  test "format_type/1" do
    assert Types.format_type(:binary) == "binary()"
    assert Types.format_type({:atom, true}) == "true"
    assert Types.format_type({:atom, :atom}) == ":atom"
    assert Types.format_type({:list, :binary}) == "[binary()]"
    assert Types.format_type({:tuple, []}) == "{}"
    assert Types.format_type({:tuple, [:integer]}) == "{integer()}"
    assert Types.format_type({:map, []}) == "%{}"
    assert Types.format_type({:map, [{:integer, :atom}]}) == "%{integer() => atom()}"
    assert Types.format_type({:map, [{:__struct__, Struct}]}) == "%Struct{}"

    assert Types.format_type({:map, [{:__struct__, Struct}, {:integer, :atom}]}) ==
             "%Struct{integer() => atom()}"
  end
end
