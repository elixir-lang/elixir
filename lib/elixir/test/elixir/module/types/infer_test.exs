Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.InferTest do
  use ExUnit.Case, async: true
  import Module.Types.Infer
  alias Module.Types

  defmacrop quoted_pattern(expr) do
    quote do
      of_pattern(unquote(Macro.escape(expr)), new_stack(), new_context())
      |> lift_result()
    end
  end

  defmacrop quoted_guard(guards, context) do
    quote do
      of_guard(unquote(Macro.escape(expand_guards(guards))), new_stack(), unquote(context))
    end
  end

  defp expand_guards(guards) do
    fun =
      quote do
        fn var!(x) when unquote(guards) -> var!(x) end
      end

    {ast, _env} = :elixir_expand.expand(fun, __ENV__)
    {:fn, _, [{:->, _, [[{:when, _, [_, guards]}], _]}]} = ast
    guards
  end

  defp unify_lift(left, right, context \\ new_context()) do
    unify(left, right, new_stack(), context)
    |> lift_result()
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0})
  end

  defp new_stack() do
    %{
      Types.stack()
      | expr_stack: [{:foo, [], nil}]
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

  describe "of_pattern/2" do
    test "error location" do
      assert {:error, {{:unable_unify, :binary, :integer, expr, traces}, location}} =
               quoted_pattern(<<foo::integer, foo::binary>>)

      assert location == [{"types_test.ex", 63, {TypesTest, :test, 0}}]

      assert {:<<>>, _,
              [
                {:"::", _, [{:foo, _, nil}, {:integer, _, nil}]},
                {:"::", _, [{:foo, _, nil}, {:binary, _, nil}]}
              ]} = expr

      assert [
               {{:foo, _, nil},
                {:type, :binary, {:"::", _, [{:foo, _, nil}, {:binary, _, nil}]},
                 {"types_test.ex", 63}}},
               {{:foo, _, nil},
                {:type, :integer, {:"::", _, [{:foo, _, nil}, {:integer, _, nil}]},
                 {"types_test.ex", 63}}}
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
      assert quoted_pattern([] ++ _) == {:ok, {:list, :dynamic}}
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

      assert {:error, {{:unable_unify, {:atom, :foo}, :integer, _, _}, _}} =
               quoted_pattern(%{a: a = 123, b: a = :foo})
    end

    test "struct" do
      defmodule :"Elixir.Module.Types.InferTest.Struct" do
        defstruct foo: :atom, bar: 123, baz: %{}
      end

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
      assert quoted_pattern(<<foo::integer>>) == {:ok, :binary}
      assert quoted_pattern(<<123::utf8>>) == {:ok, :binary}
      assert quoted_pattern(<<"foo"::utf8>>) == {:ok, :binary}

      assert quoted_pattern({<<foo::integer>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}
      assert quoted_pattern({<<foo::binary>>, foo}) == {:ok, {:tuple, [:binary, :binary]}}
      assert quoted_pattern({<<foo::utf8>>, foo}) == {:ok, {:tuple, [:binary, :integer]}}

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

    test "subtype" do
      assert unify_lift(:boolean, :atom) == {:ok, :boolean}
      assert unify_lift(:atom, :boolean) == {:ok, :boolean}
      assert unify_lift(:boolean, {:atom, true}) == {:ok, {:atom, true}}
      assert unify_lift({:atom, true}, :boolean) == {:ok, {:atom, true}}
      assert unify_lift(:atom, {:atom, true}) == {:ok, {:atom, true}}
      assert unify_lift({:atom, true}, :atom) == {:ok, {:atom, true}}
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
      assert {{:var, 0}, var_context} = new_var({:foo, [], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [], nil}, var_context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify(:integer, {:var, 0}, var_context)
      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:var, _} = Types.lift_type({:var, 0}, context)
      assert {:var, _} = Types.lift_type({:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 0}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               unify_lift({:var, 0}, {:var, 1}, context)

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :integer, :binary, _, _}, _}} =
               unify_lift({:var, 1}, {:var, 0}, context)
    end

    test "vars inside tuples" do
      assert {{:var, 0}, var_context} = new_var({:foo, [], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [], nil}, var_context)

      assert {:ok, {:tuple, [{:var, 0}]}, context} =
               unify({:tuple, [{:var, 0}]}, {:tuple, [:integer]}, var_context)

      assert Types.lift_type({:var, 0}, context) == :integer

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :integer, context)

      assert {:ok, {:tuple, [{:var, _}]}, context} =
               unify({:tuple, [{:var, 0}]}, {:tuple, [{:var, 1}]}, context)

      assert {:ok, {:var, 1}, context} = unify({:var, 1}, {:tuple, [{:var, 0}]}, var_context)
      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, context)
      assert Types.lift_type({:var, 1}, context) == {:tuple, [:integer]}

      assert {:ok, {:var, 0}, context} = unify({:var, 0}, :integer, var_context)
      assert {:ok, {:var, 1}, context} = unify({:var, 1}, :binary, context)

      assert {:error, {{:unable_unify, :binary, :integer, _, _}, _}} =
               unify_lift({:tuple, [{:var, 0}]}, {:tuple, [{:var, 1}]}, context)
    end

    # TODO: Vars inside unions

    test "recursive type" do
      assert {{:var, 0}, var_context} = new_var({:foo, [], nil}, new_context())
      assert {{:var, 1}, var_context} = new_var({:bar, [], nil}, var_context)
      assert {{:var, 2}, var_context} = new_var({:baz, [], nil}, var_context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 0}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)
      assert {:ok, {:var, _}, context} = unify({:var, 2}, {:var, 0}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)

      assert {:error, {{:unable_unify, {:tuple, [var: 0]}, {:var, 0}, _, _}, _}} =
               unify_lift({:var, 1}, {:tuple, [{:var, 0}]}, context)

      assert {:ok, {:var, _}, context} = unify({:var, 0}, {:var, 1}, var_context)
      assert {:ok, {:var, _}, context} = unify({:var, 1}, {:var, 2}, context)

      assert {:error, {{:unable_unify, {:tuple, [var: 0]}, {:var, 0}, _, _}, _}} =
               unify_lift({:var, 2}, {:tuple, [{:var, 0}]}, context)
    end
  end

  test "of_guard/2" do
    assert {{:var, 0}, var_context} = new_var({:x, [], nil}, new_context())

    assert {:ok, :boolean, context} = quoted_guard(is_tuple(x), var_context)
    assert Types.lift_type({:var, 0}, context) == :tuple

    assert {:ok, :dynamic, context} = quoted_guard(elem(x, 0), var_context)
    assert Types.lift_type({:var, 0}, context) == :tuple

    assert {:error, {_, {:unable_unify, :boolean, :tuple, _, _}, _}} =
             quoted_guard(is_tuple(x) and is_boolean(x), var_context)
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

    assert {{:var, 0}, var_context} = new_var({:foo, [], nil}, new_context())
    assert to_union([{:var, 0}], var_context) == {:var, 0}

    assert to_union([{:tuple, [:integer]}, {:tuple, [:integer]}], new_context()) ==
             {:tuple, [:integer]}
  end
end
