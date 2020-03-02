Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.InferTest do
  use ExUnit.Case, async: true
  import Module.Types.Infer
  alias Module.Types

  defp unify_lift(left, right, context \\ new_context()) do
    unify(left, right, new_stack(), context)
    |> lift_result()
  end

  defp unify_directed_lift(left, right) do
    stack = %{new_stack() | context: :expr}

    unify(left, right, stack, new_context())
    |> lift_result()
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
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
    {:ok, Types.lift_type(type, context)}
  end

  defp lift_result({:error, {Types, reason, location}}) do
    {:error, {reason, location}}
  end

  describe "unify/3" do
    test "literal" do
      assert unify_lift({:atom, :foo}, {:atom, :foo}) == {:ok, {:atom, :foo}}

      assert {:error, {{:unable_unify, {:atom, :foo}, {:atom, :bar}, _, _}, _}} =
               unify_lift({:atom, :foo}, {:atom, :bar})
    end

    test "type" do
      assert unify_lift(:integer, :integer) == {:ok, :integer}
      assert unify_lift(:binary, :binary) == {:ok, :binary}
      assert unify_lift(:atom, :atom) == {:ok, :atom}
      assert unify_lift(:boolean, :boolean) == {:ok, :boolean}

      assert {:error, {{:unable_unify, :integer, :boolean, _, _}, _}} =
               unify_lift(:integer, :boolean)
    end

    test "subtype undirected" do
      assert unify_lift(:boolean, :atom) == {:ok, :boolean}
      assert unify_lift(:atom, :boolean) == {:ok, :boolean}
      assert unify_lift(:boolean, {:atom, true}) == {:ok, {:atom, true}}
      assert unify_lift({:atom, true}, :boolean) == {:ok, {:atom, true}}
      assert unify_lift(:atom, {:atom, true}) == {:ok, {:atom, true}}
      assert unify_lift({:atom, true}, :atom) == {:ok, {:atom, true}}
    end

    test "subtype directed" do
      assert unify_directed_lift(:boolean, :atom) == {:ok, :boolean}
      assert unify_directed_lift({:atom, true}, :boolean) == {:ok, {:atom, true}}
      assert unify_directed_lift({:atom, true}, :atom) == {:ok, {:atom, true}}

      assert {:error, _} = unify_directed_lift(:atom, :boolean)
      assert {:error, _} = unify_directed_lift(:boolean, {:atom, true})
      assert {:error, _} = unify_directed_lift(:atom, {:atom, true})
    end

    test "tuple" do
      assert unify_lift({:tuple, []}, {:tuple, []}) == {:ok, {:tuple, []}}
      assert unify_lift({:tuple, [:integer]}, {:tuple, [:integer]}) == {:ok, {:tuple, [:integer]}}
      assert unify_lift({:tuple, [:boolean]}, {:tuple, [:atom]}) == {:ok, {:tuple, [:boolean]}}

      assert {:error, {{:unable_unify, {:tuple, [:integer]}, {:tuple, []}, _, _}, _}} =
               unify_lift({:tuple, [:integer]}, {:tuple, []})

      assert {:error, {{:unable_unify, :integer, :atom, _, _}, _}} =
               unify_lift({:tuple, [:integer]}, {:tuple, [:atom]})
    end

    test "list" do
      assert unify_lift({:list, :integer}, {:list, :integer}) == {:ok, {:list, :integer}}

      assert {:error, {{:unable_unify, :atom, :integer, _, _}, _}} =
               unify_lift({:list, :atom}, {:list, :integer})
    end

    test "map" do
      assert unify_lift({:map, []}, {:map, []}) == {:ok, {:map, []}}

      assert unify_lift({:map, [{:integer, :atom}]}, {:map, []}) ==
               {:ok, {:map, [{:integer, :atom}]}}

      assert unify_lift({:map, []}, {:map, [{:integer, :atom}]}) ==
               {:ok, {:map, [{:integer, :atom}]}}

      assert unify_lift({:map, [{:integer, :atom}]}, {:map, [{:integer, :atom}]}) ==
               {:ok, {:map, [{:integer, :atom}]}}

      assert unify_lift({:map, [{:integer, :atom}]}, {:map, [{:atom, :integer}]}) ==
               {:ok, {:map, [{:integer, :atom}, {:atom, :integer}]}}

      assert unify_lift(
               {:map, [{{:atom, :foo}, :boolean}]},
               {:map, [{{:atom, :foo}, :atom}]}
             ) ==
               {:ok, {:map, [{{:atom, :foo}, :boolean}]}}

      assert {:error, {{:unable_unify, :integer, :atom, _, _}, _}} =
               unify_lift(
                 {:map, [{{:atom, :foo}, :integer}]},
                 {:map, [{{:atom, :foo}, :atom}]}
               )
    end

    test "union" do
      assert unify_lift({:union, []}, {:union, []}) == {:ok, {:union, []}}
      assert unify_lift({:union, [:integer]}, {:union, [:integer]}) == {:ok, {:union, [:integer]}}

      assert unify_lift({:union, [:integer, :atom]}, {:union, [:integer, :atom]}) ==
               {:ok, {:union, [:integer, :atom]}}

      assert unify_lift({:union, [:integer, :atom]}, {:union, [:atom, :integer]}) ==
               {:ok, {:union, [:integer, :atom]}}

      assert unify_lift({:union, [:atom]}, {:union, [:boolean]}) == {:ok, {:union, [:boolean]}}
      assert unify_lift({:union, [:boolean]}, {:union, [:atom]}) == {:ok, {:union, [:boolean]}}

      assert {:error, {{:unable_unify, {:union, [:integer]}, {:union, [:atom]}, _, _}, _}} =
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
      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify(:integer, {:var, 0}, var_context)
      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:var, _} = Types.lift_type({:var, 0}, context)
      assert {:var, _} = Types.lift_type({:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 1}, {:var, 0}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               unify_lift({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               unify_lift({:var, 1}, {:var, 0}, context)
    end

    test "vars inside tuples" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [version: 1], nil}, var_context)

      assert {:ok, {:tuple, [{:var, 0}]}, context} =
               unify({:tuple, [{:var, 0}]}, {:tuple, [:integer]}, var_context)

      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)

      assert {:ok, {:tuple, [{:var, _}]}, _context} =
               unify({:tuple, [{:var, 0}]}, {:tuple, [{:var, 1}]}, context)

      assert {:ok, {:var, 1}, context} = unify({:var, 1}, {:tuple, [{:var, 0}]}, var_context)
      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, context)
      assert Types.lift_type({:var, 1}, context) == {:tuple, [:integer]}

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               unify_lift({:tuple, [{:var, 0}]}, {:tuple, [{:var, 1}]}, context)
    end

    # TODO: Vars inside unions

    test "recursive type" do
      assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [version: 1], nil}, var_context)
      assert {{:var, 2}, var_context} = new_var({:baz, [version: 2], nil}, var_context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, _context} = unify({:var, 1}, {:var, 0}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)
      assert {:ok, {:var, _}, _context} = unify({:var, 2}, {:var, 0}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)

      assert {:error, {{:unable_unify, {:var, 0}, {:tuple, [{:var, 0}]}, _, _}, _}} =
               unify_lift({:var, 1}, {:tuple, [{:var, 0}]}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)

      assert {:error, {{:unable_unify, {:var, 0}, {:tuple, [{:var, 0}]}, _, _}, _}} =
               unify_lift({:var, 2}, {:tuple, [{:var, 0}]}, context)
    end
  end

  test "subtype?/3" do
    assert subtype?({:atom, :foo}, :atom, new_context())
    assert subtype?({:atom, true}, :boolean, new_context())
    assert subtype?({:atom, true}, :atom, new_context())
    assert subtype?(:boolean, :atom, new_context())

    refute subtype?(:integer, :binary, new_context())
    refute subtype?(:atom, {:atom, :foo}, new_context())
    refute subtype?(:boolean, {:atom, true}, new_context())
    refute subtype?(:atom, {:atom, true}, new_context())
    refute subtype?(:atom, :boolean, new_context())
  end

  test "to_union/2" do
    assert to_union([:atom], new_context()) == :atom
    assert to_union([:integer, :integer], new_context()) == :integer
    assert to_union([:boolean, :atom], new_context()) == :atom
    assert to_union([{:atom, :foo}, :boolean, :atom], new_context()) == :atom

    assert to_union([:binary, :atom], new_context()) == {:union, [:binary, :atom]}
    assert to_union([:atom, :binary, :atom], new_context()) == {:union, [:atom, :binary]}

    assert to_union([{:atom, :foo}, :binary, :atom], new_context()) ==
             {:union, [:binary, :atom]}

    assert {{:var, 0}, var_context} = new_var({:foo, [version: 0], nil}, new_context())
    assert to_union([{:var, 0}], var_context) == {:var, 0}

    assert to_union([{:tuple, [:integer]}, {:tuple, [:integer]}], new_context()) ==
             {:tuple, [:integer]}
  end
end
