Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.UnifyTest do
  use ExUnit.Case, async: true
  import Module.Types.Unify
  alias Module.Types

  defp unify_lift(left, right, context \\ new_context()) do
    unify(left, right, new_stack(), context)
    |> lift_result()
  end

  defp unify_directed_lift(left, right, context \\ new_context()) do
    stack = %{new_stack() | context: :expr}

    unify(left, right, stack, context)
    |> lift_result()
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0}, [], Module.ParallelChecker.test_cache())
  end

  defp new_stack() do
    %{
      Types.stack()
      | context: :pattern,
        last_expr: {:foo, [], nil}
    }
  end

  defp unify(left, right, context) do
    unify(left, right, new_stack(), context)
  end

  defp lift_result({:ok, type, context}) do
    {:ok, lift_type(type, context)}
  end

  defp lift_result({:error, {type, reason, _context}}) do
    {:error, {type, reason}}
  end

  defp lift_type(type, context), do: [type] |> lift_types(context) |> hd()

  describe "unify/3" do
    test "literal" do
      assert unify_lift({:atom, :foo}, {:atom, :foo}) == {:ok, {:atom, :foo}}

      assert {:error, {:unable_unify, {{:atom, :foo}, {:atom, :bar}, _}}} =
               unify_lift({:atom, :foo}, {:atom, :bar})
    end

    test "type" do
      assert unify_lift(:integer, :integer) == {:ok, :integer}
      assert unify_lift(:binary, :binary) == {:ok, :binary}
      assert unify_lift(:atom, :atom) == {:ok, :atom}

      assert {:error, {:unable_unify, {:integer, :atom, _}}} = unify_lift(:integer, :atom)
    end

    test "subtype undirected" do
      assert unify_lift(:atom, {:atom, true}) == {:ok, {:atom, true}}
      assert unify_lift({:atom, true}, :atom) == {:ok, {:atom, true}}
    end

    test "subtype directed" do
      assert unify_directed_lift({:atom, true}, :atom) == {:ok, {:atom, true}}
      assert {:error, _} = unify_directed_lift(:atom, {:atom, true})
    end

    test "tuple" do
      assert unify_lift({:tuple, 0, []}, {:tuple, 0, []}) == {:ok, {:tuple, 0, []}}

      assert unify_lift({:tuple, 1, [:integer]}, {:tuple, 1, [:integer]}) ==
               {:ok, {:tuple, 1, [:integer]}}

      assert unify_lift({:tuple, 1, [{:atom, :foo}]}, {:tuple, 1, [:atom]}) ==
               {:ok, {:tuple, 1, [{:atom, :foo}]}}

      assert {:error, {:unable_unify, {{:tuple, 1, [:integer]}, {:tuple, 0, []}, _}}} =
               unify_lift({:tuple, 1, [:integer]}, {:tuple, 0, []})

      assert {:error, {:unable_unify, {:integer, :atom, _}}} =
               unify_lift({:tuple, 1, [:integer]}, {:tuple, 1, [:atom]})
    end

    test "list" do
      assert unify_lift({:list, :integer}, {:list, :integer}) == {:ok, {:list, :integer}}

      assert {:error, {:unable_unify, {:atom, :integer, _}}} =
               unify_lift({:list, :atom}, {:list, :integer})
    end

    test "map" do
      assert unify_lift({:map, []}, {:map, []}) == {:ok, {:map, []}}

      assert unify_lift(
               {:map, [{:required, :integer, :atom}]},
               {:map, [{:optional, :dynamic, :dynamic}]}
             ) ==
               {:ok, {:map, [{:required, :integer, :atom}]}}

      assert unify_lift(
               {:map, [{:optional, :dynamic, :dynamic}]},
               {:map, [{:required, :integer, :atom}]}
             ) ==
               {:ok, {:map, [{:required, :integer, :atom}]}}

      assert unify_lift(
               {:map, [{:optional, :dynamic, :dynamic}]},
               {:map, [{:required, :integer, :atom}, {:optional, :dynamic, :dynamic}]}
             ) ==
               {:ok, {:map, [{:required, :integer, :atom}, {:optional, :dynamic, :dynamic}]}}

      assert unify_lift(
               {:map, [{:required, :integer, :atom}, {:optional, :dynamic, :dynamic}]},
               {:map, [{:optional, :dynamic, :dynamic}]}
             ) ==
               {:ok, {:map, [{:required, :integer, :atom}, {:optional, :dynamic, :dynamic}]}}

      assert unify_lift(
               {:map, [{:required, :integer, :atom}]},
               {:map, [{:required, :integer, :atom}]}
             ) ==
               {:ok, {:map, [{:required, :integer, :atom}]}}

      assert {:error,
              {:unable_unify,
               {{:map, [{:required, :integer, :atom}]}, {:map, [{:required, :atom, :integer}]}, _}}} =
               unify_lift(
                 {:map, [{:required, :integer, :atom}]},
                 {:map, [{:required, :atom, :integer}]}
               )

      assert {:error, {:unable_unify, {{:map, [{:required, :integer, :atom}]}, {:map, []}, _}}} =
               unify_lift({:map, [{:required, :integer, :atom}]}, {:map, []})

      assert {:error, {:unable_unify, {{:map, []}, {:map, [{:required, :integer, :atom}]}, _}}} =
               unify_lift({:map, []}, {:map, [{:required, :integer, :atom}]})

      assert {:error,
              {:unable_unify,
               {{:map, [{:required, {:atom, :foo}, :integer}]},
                {:map, [{:required, {:atom, :foo}, :atom}]},
                _}}} =
               unify_lift(
                 {:map, [{:required, {:atom, :foo}, :integer}]},
                 {:map, [{:required, {:atom, :foo}, :atom}]}
               )
    end

    test "map required/optional key" do
      assert unify_lift(
               {:map, [{:required, {:atom, :foo}, {:atom, :bar}}]},
               {:map, [{:required, {:atom, :foo}, :atom}]}
             ) ==
               {:ok, {:map, [{:required, {:atom, :foo}, {:atom, :bar}}]}}

      assert unify_lift(
               {:map, [{:optional, {:atom, :foo}, {:atom, :bar}}]},
               {:map, [{:required, {:atom, :foo}, :atom}]}
             ) ==
               {:ok, {:map, [{:required, {:atom, :foo}, {:atom, :bar}}]}}

      assert unify_lift(
               {:map, [{:required, {:atom, :foo}, {:atom, :bar}}]},
               {:map, [{:optional, {:atom, :foo}, :atom}]}
             ) ==
               {:ok, {:map, [{:required, {:atom, :foo}, {:atom, :bar}}]}}

      assert unify_lift(
               {:map, [{:optional, {:atom, :foo}, {:atom, :bar}}]},
               {:map, [{:optional, {:atom, :foo}, :atom}]}
             ) ==
               {:ok, {:map, [{:optional, {:atom, :foo}, {:atom, :bar}}]}}
    end

    test "map with subtyped keys" do
      assert unify_directed_lift(
               {:map, [{:required, {:atom, :foo}, :integer}]},
               {:map, [{:required, :atom, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:optional, {:atom, :foo}, :integer}]},
               {:map, [{:required, :atom, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:required, {:atom, :foo}, :integer}]},
               {:map, [{:optional, :atom, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:optional, {:atom, :foo}, :integer}]},
               {:map, [{:optional, :atom, :integer}]}
             ) == {:ok, {:map, [{:optional, {:atom, :foo}, :integer}]}}

      assert {:error,
              {:unable_unify,
               {{:map, [{:required, :atom, :integer}]},
                {:map, [{:required, {:atom, :foo}, :integer}]},
                _}}} =
               unify_directed_lift(
                 {:map, [{:required, :atom, :integer}]},
                 {:map, [{:required, {:atom, :foo}, :integer}]}
               )

      assert {:error,
              {:unable_unify,
               {{:map, [{:optional, :atom, :integer}]},
                {:map, [{:required, {:atom, :foo}, :integer}]},
                _}}} =
               unify_directed_lift(
                 {:map, [{:optional, :atom, :integer}]},
                 {:map, [{:required, {:atom, :foo}, :integer}]}
               )

      assert {:error,
              {:unable_unify,
               {{:map, [{:required, :atom, :integer}]},
                {:map, [{:optional, {:atom, :foo}, :integer}]},
                _}}} =
               unify_directed_lift(
                 {:map, [{:required, :atom, :integer}]},
                 {:map, [{:optional, {:atom, :foo}, :integer}]}
               )

      assert unify_directed_lift(
               {:map, [{:optional, :atom, :integer}]},
               {:map, [{:optional, {:atom, :foo}, :integer}]}
             ) == {:ok, {:map, []}}

      assert unify_directed_lift(
               {:map, [{:required, {:atom, :foo}, :integer}]},
               {:map, [{:required, {:atom, :foo}, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:required, {:atom, :foo}, :integer}]},
               {:map, [{:optional, {:atom, :foo}, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:optional, {:atom, :foo}, :integer}]},
               {:map, [{:required, {:atom, :foo}, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:optional, {:atom, :foo}, :integer}]},
               {:map, [{:optional, {:atom, :foo}, :integer}]}
             ) == {:ok, {:map, [{:optional, {:atom, :foo}, :integer}]}}
    end

    test "map with subtyped and multiple matching keys" do
      assert {:error, _} =
               unify_directed_lift(
                 {:map,
                  [
                    {:required, {:atom, :foo}, :integer},
                    {:required, :atom, {:union, [:integer, :boolean]}}
                  ]},
                 {:map, [{:required, :atom, :integer}]}
               )

      assert unify_directed_lift(
               {:map,
                [
                  {:required, {:atom, :foo}, :integer},
                  {:required, :atom, {:union, [:integer, :boolean]}}
                ]},
               {:map, [{:required, :atom, {:union, [:integer, :boolean]}}]}
             ) ==
               {:ok,
                {:map,
                 [
                   {:required, {:atom, :foo}, :integer},
                   {:required, :atom, {:union, [:integer, :boolean]}}
                 ]}}

      assert {:error, _} =
               unify_directed_lift(
                 {:map, [{:required, :atom, :integer}]},
                 {:map,
                  [
                    {:required, {:atom, :foo}, :integer},
                    {:required, :atom, {:union, [:integer, :boolean]}}
                  ]}
               )

      assert {:error, _} =
               unify_directed_lift(
                 {:map, [{:required, :atom, {:union, [:integer, :boolean]}}]},
                 {:map,
                  [
                    {:required, {:atom, :foo}, :integer},
                    {:required, :atom, {:union, [:integer, :boolean]}}
                  ]}
               )

      assert unify_directed_lift(
               {:map, [{:required, :atom, :integer}]},
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]}
             ) == {:ok, {:map, [{:required, :atom, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:required, :atom, {:union, [:integer, :boolean]}}]},
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]}
             ) == {:ok, {:map, [{:required, :atom, {:union, [:integer, :boolean]}}]}}

      assert unify_directed_lift(
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]},
               {:map, [{:required, :atom, :integer}]}
             ) == {:ok, {:map, [{:required, {:atom, :foo}, :integer}]}}

      # TODO: FIX ME
      # assert unify_directed_lift(
      #          {:map,
      #           [
      #             {:optional, {:atom, :foo}, :integer},
      #             {:optional, :atom, {:union, [:integer, :boolean]}}
      #           ]},
      #          {:map, [{:required, :atom, {:union, [:integer, :boolean]}}]}
      #        ) ==
      #          {:ok,
      #           {:map,
      #            [
      #              {:required, {:atom, :foo}, :integer},
      #              {:required, :atom, {:union, [:integer, :boolean]}}
      #            ]}}

      assert {:error, _} =
               unify_directed_lift(
                 {:map,
                  [
                    {:optional, {:atom, :foo}, :integer},
                    {:optional, :atom, {:union, [:integer, :boolean]}}
                  ]},
                 {:map, [{:optional, :atom, :integer}]}
               )

      assert unify_directed_lift(
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]},
               {:map, [{:optional, :atom, {:union, [:integer, :boolean]}}]}
             ) ==
               {:ok,
                {:map,
                 [
                   {:optional, {:atom, :foo}, :integer},
                   {:optional, :atom, {:union, [:integer, :boolean]}}
                 ]}}

      assert unify_directed_lift(
               {:map, [{:optional, :atom, :integer}]},
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]}
             ) == {:ok, {:map, [{:optional, :atom, :integer}]}}

      assert unify_directed_lift(
               {:map, [{:optional, :atom, {:union, [:integer, :boolean]}}]},
               {:map,
                [
                  {:optional, {:atom, :foo}, :integer},
                  {:optional, :atom, {:union, [:integer, :boolean]}}
                ]}
             ) == {:ok, {:map, [{:optional, :atom, {:union, [:integer, :boolean]}}]}}
    end

    test "union" do
      assert unify_lift({:union, []}, {:union, []}) == {:ok, {:union, []}}
      assert unify_lift({:union, [:integer]}, {:union, [:integer]}) == {:ok, {:union, [:integer]}}

      assert unify_lift({:union, [:integer, :atom]}, {:union, [:integer, :atom]}) ==
               {:ok, {:union, [:integer, :atom]}}

      assert unify_lift({:union, [:integer, :atom]}, {:union, [:atom, :integer]}) ==
               {:ok, {:union, [:integer, :atom]}}

      assert unify_lift({:union, [:atom]}, {:union, [{:atom, :bar}]}) ==
               {:ok, {:atom, :bar}}

      assert {:error, {:unable_unify, {:integer, {:union, [:atom]}, _}}} =
               unify_lift({:union, [:integer]}, {:union, [:atom]})
    end

    test "dynamic" do
      assert unify_lift({:atom, :foo}, :dynamic) == {:ok, {:atom, :foo}}
      assert unify_lift(:dynamic, {:atom, :foo}) == {:ok, {:atom, :foo}}
      assert unify_lift(:integer, :dynamic) == {:ok, :integer}
      assert unify_lift(:dynamic, :integer) == {:ok, :integer}
    end

    test "vars" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [version: 1], nil}, var_context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify(:integer, {:var, 0}, var_context)
      assert lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:var, _} = lift_type({:var, 0}, context)
      assert {:var, _} = lift_type({:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 1}, {:var, 0}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {:unable_unify, {:integer, :binary, _}}} =
               unify_lift({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {:unable_unify, {:binary, :integer, _}}} =
               unify_lift({:var, 1}, {:var, 0}, context)
    end

    test "vars inside tuples" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [version: 1], nil}, var_context)

      assert {:ok, {:tuple, 1, [{:var, 0}]}, context} =
               unify({:tuple, 1, [{:var, 0}]}, {:tuple, 1, [:integer]}, var_context)

      assert lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)

      assert {:ok, {:tuple, 1, [{:var, _}]}, _context} =
               unify({:tuple, 1, [{:var, 0}]}, {:tuple, 1, [{:var, 1}]}, context)

      assert {:ok, {:var, 1}, context} = unify({:var, 1}, {:tuple, 1, [{:var, 0}]}, var_context)
      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, context)
      assert lift_type({:var, 1}, context) == {:tuple, 1, [:integer]}

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {:unable_unify, {:integer, :binary, _}}} =
               unify_lift({:tuple, 1, [{:var, 0}]}, {:tuple, 1, [{:var, 1}]}, context)
    end

    # TODO: Vars inside right unions

    test "vars inside left unions" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())

      assert {:ok, {:var, 0}, context} =
               unify({:union, [{:var, 0}, :integer]}, :integer, var_context)

      assert lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} =
               unify({:union, [{:var, 0}, :integer]}, {:union, [:integer, :atom]}, var_context)

      assert lift_type({:var, 0}, context) == {:union, [:integer, :atom]}

      assert {:error, {:unable_unify, {:integer, {:union, [:binary, :atom]}, _}}} =
               unify_directed_lift(
                 {:union, [{:var, 0}, :integer]},
                 {:union, [:binary, :atom]},
                 var_context
               )
    end

    test "recursive type" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [version: 1], nil}, var_context)
      assert {{:var, 2}, var_context} = new_var({:baz, [version: 2], nil}, var_context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 0}, context)
      assert context.types[0] == {:var, 1}
      assert context.types[1] == {:var, 0}

      assert {:ok, {:var, _}, context} = unify({:var, 0}, :tuple, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 0}, context)
      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, context)
      assert context.types[0] == {:var, 1}
      assert context.types[1] == :tuple

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 2}, {:var, 0}, context)
      assert context.types[0] == :unbound
      assert context.types[1] == {:var, 0}
      assert context.types[2] == {:var, 1}

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)

      assert {:error, {:unable_unify, {{:var, 0}, {:tuple, 1, [{:var, 0}]}, _}}} =
               unify_lift({:var, 1}, {:tuple, 1, [{:var, 0}]}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)

      assert {:error, {:unable_unify, {{:var, 0}, {:tuple, 1, [{:var, 0}]}, _}}} =
               unify_lift({:var, 2}, {:tuple, 1, [{:var, 0}]}, context)
    end

    test "error with internal variable" do
      context = new_context()
      {var_integer, context} = add_var(context)
      {var_atom, context} = add_var(context)

      {:ok, _, context} = unify(var_integer, :integer, context)
      {:ok, _, context} = unify(var_atom, :atom, context)

      assert {:error, _} = unify(var_integer, var_atom, context)
    end
  end

  describe "has_unbound_var?/2" do
    setup do
      context = new_context()
      {unbound_var, context} = add_var(context)
      {bound_var, context} = add_var(context)
      {:ok, _, context} = unify(bound_var, :integer, context)
      %{context: context, unbound_var: unbound_var, bound_var: bound_var}
    end

    test "returns true when there are unbound vars",
         %{context: context, unbound_var: unbound_var} do
      assert has_unbound_var?(unbound_var, context)
      assert has_unbound_var?({:union, [unbound_var]}, context)
      assert has_unbound_var?({:tuple, 1, [unbound_var]}, context)
      assert has_unbound_var?({:list, unbound_var}, context)
      assert has_unbound_var?({:map, [{:required, unbound_var, :atom}]}, context)
      assert has_unbound_var?({:map, [{:required, :atom, unbound_var}]}, context)
    end

    test "returns false when there are no unbound vars",
         %{context: context, bound_var: bound_var} do
      refute has_unbound_var?(bound_var, context)
      refute has_unbound_var?({:union, [bound_var]}, context)
      refute has_unbound_var?({:tuple, 1, [bound_var]}, context)
      refute has_unbound_var?(:integer, context)
      refute has_unbound_var?({:list, bound_var}, context)
      refute has_unbound_var?({:map, [{:required, :atom, :atom}]}, context)
      refute has_unbound_var?({:map, [{:required, bound_var, :atom}]}, context)
      refute has_unbound_var?({:map, [{:required, :atom, bound_var}]}, context)
    end
  end

  describe "subtype?/3" do
    test "with simple types" do
      assert subtype?({:atom, :foo}, :atom, new_context())
      assert subtype?({:atom, true}, :atom, new_context())

      refute subtype?(:integer, :binary, new_context())
      refute subtype?(:atom, {:atom, :foo}, new_context())
      refute subtype?(:atom, {:atom, true}, new_context())
    end

    test "with composite types" do
      assert subtype?({:list, {:atom, :foo}}, {:list, :atom}, new_context())
      assert subtype?({:tuple, 1, [{:atom, :foo}]}, {:tuple, 1, [:atom]}, new_context())

      refute subtype?({:list, :atom}, {:list, {:atom, :foo}}, new_context())
      refute subtype?({:tuple, 1, [:atom]}, {:tuple, 1, [{:atom, :foo}]}, new_context())
      refute subtype?({:tuple, 1, [:atom]}, {:tuple, 2, [:atom, :atom]}, new_context())
      refute subtype?({:tuple, 2, [:atom, :atom]}, {:tuple, 1, [:atom]}, new_context())

      refute subtype?(
               {:tuple, 2, [{:atom, :a}, :integer]},
               {:tuple, 2, [{:atom, :b}, :integer]},
               new_context()
             )
    end

    test "with maps" do
      assert subtype?({:map, [{:optional, :atom, :integer}]}, {:map, []}, new_context())

      assert subtype?(
               {:map, [{:required, :atom, :integer}]},
               {:map, [{:required, :atom, :integer}]},
               new_context()
             )

      assert subtype?(
               {:map, [{:required, {:atom, :foo}, :integer}]},
               {:map, [{:required, :atom, :integer}]},
               new_context()
             )

      assert subtype?(
               {:map, [{:required, :integer, {:atom, :foo}}]},
               {:map, [{:required, :integer, :atom}]},
               new_context()
             )

      refute subtype?({:map, [{:required, :atom, :integer}]}, {:map, []}, new_context())

      refute subtype?(
               {:map, [{:required, :atom, :integer}]},
               {:map, [{:required, {:atom, :foo}, :integer}]},
               new_context()
             )

      refute subtype?(
               {:map, [{:required, :integer, :atom}]},
               {:map, [{:required, :integer, {:atom, :foo}}]},
               new_context()
             )
    end

    test "with unions" do
      assert subtype?({:union, [{:atom, :foo}]}, {:union, [:atom]}, new_context())
      assert subtype?({:union, [{:atom, :foo}, {:atom, :bar}]}, {:union, [:atom]}, new_context())
      assert subtype?({:union, [{:atom, :foo}]}, {:union, [:integer, :atom]}, new_context())

      assert subtype?({:atom, :foo}, {:union, [:atom]}, new_context())
      assert subtype?({:atom, :foo}, {:union, [:integer, :atom]}, new_context())

      assert subtype?({:union, [{:atom, :foo}]}, :atom, new_context())
      assert subtype?({:union, [{:atom, :foo}, {:atom, :bar}]}, :atom, new_context())

      refute subtype?({:union, [:atom]}, {:union, [{:atom, :foo}]}, new_context())
      refute subtype?({:union, [:atom]}, {:union, [{:atom, :foo}, :integer]}, new_context())
      refute subtype?(:atom, {:union, [{:atom, :foo}, :integer]}, new_context())
      refute subtype?({:union, [:atom]}, {:atom, :foo}, new_context())
    end
  end

  test "to_union/2" do
    assert to_union([:atom], new_context()) == :atom
    assert to_union([:integer, :integer], new_context()) == :integer
    assert to_union([{:atom, :foo}, {:atom, :bar}, :atom], new_context()) == :atom

    assert to_union([:binary, :atom], new_context()) == {:union, [:binary, :atom]}
    assert to_union([:atom, :binary, :atom], new_context()) == {:union, [:atom, :binary]}

    assert to_union([{:atom, :foo}, :binary, :atom], new_context()) ==
             {:union, [:binary, :atom]}

    assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
    assert to_union([{:var, 0}], var_context) == {:var, 0}

    assert to_union([{:tuple, 1, [:integer]}, {:tuple, 1, [:integer]}], new_context()) ==
             {:tuple, 1, [:integer]}
  end

  test "format_type/1" do
    assert format_type(:binary, false) == "binary()"
    assert format_type({:atom, true}, false) == "true"
    assert format_type({:atom, :atom}, false) == ":atom"
    assert format_type({:list, :binary}, false) == "[binary()]"
    assert format_type({:tuple, 0, []}, false) == "{}"
    assert format_type({:tuple, 1, [:integer]}, false) == "{integer()}"

    assert format_type({:map, []}, true) == "map()"
    assert format_type({:map, [{:required, {:atom, :foo}, :atom}]}, true) == "map()"

    assert format_type({:map, []}, false) ==
             "%{}"

    assert format_type({:map, [{:required, {:atom, :foo}, :atom}]}, false) ==
             "%{foo: atom()}"

    assert format_type({:map, [{:required, :integer, :atom}]}, false) ==
             "%{integer() => atom()}"

    assert format_type({:map, [{:optional, :integer, :atom}]}, false) ==
             "%{optional(integer()) => atom()}"

    assert format_type({:map, [{:optional, {:atom, :foo}, :atom}]}, false) ==
             "%{optional(:foo) => atom()}"

    assert format_type({:map, [{:required, {:atom, :__struct__}, {:atom, Struct}}]}, false) ==
             "%Struct{}"

    assert format_type(
             {:map,
              [{:required, {:atom, :__struct__}, {:atom, Struct}}, {:required, :integer, :atom}]},
             false
           ) ==
             "%Struct{integer() => atom()}"
  end
end
