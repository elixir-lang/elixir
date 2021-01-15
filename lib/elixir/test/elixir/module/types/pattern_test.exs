Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.PatternTest do
  use ExUnit.Case, async: true

  alias Module.Types
  alias Module.Types.{Unify, Pattern}

  defmacrop quoted_pattern(patterns) do
    quote do
      {patterns, true} = unquote(Macro.escape(expand_head(patterns, true)))

      Pattern.of_pattern(patterns, new_stack(), new_context())
      |> lift_result()
    end
  end

  defmacrop quoted_head(patterns, guards \\ []) do
    quote do
      {patterns, guards} = unquote(Macro.escape(expand_head(patterns, guards)))

      Pattern.of_head(patterns, guards, new_stack(), new_context())
      |> lift_result()
    end
  end

  defp expand_head(patterns, guards) do
    fun =
      quote do
        fn unquote(patterns) when unquote(guards) -> :ok end
      end

    fun =
      Macro.prewalk(fun, fn
        {var, meta, nil} -> {var, meta, __MODULE__}
        other -> other
      end)

    {ast, _env} = :elixir_expand.expand(fun, __ENV__)
    {:fn, _, [{:->, _, [[{:when, _, [patterns, guards]}], _]}]} = ast
    {patterns, guards}
  end

  defp new_context() do
    Types.context("types_test.ex", TypesTest, {:test, 0}, [], Module.ParallelChecker.test_cache())
  end

  defp new_stack() do
    %{
      Types.stack()
      | last_expr: {:foo, [], nil}
    }
  end

  defp lift_result({:ok, types, context}) when is_list(types) do
    {:ok, Unify.lift_types(types, context)}
  end

  defp lift_result({:ok, type, context}) do
    {:ok, [type] |> Unify.lift_types(context) |> hd()}
  end

  defp lift_result({:error, {type, reason, _context}}) do
    {:error, {type, reason}}
  end

  defmodule :"Elixir.Module.Types.PatternTest.Struct" do
    defstruct foo: :atom, bar: 123, baz: %{}
  end

  describe "patterns" do
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
      assert quoted_pattern({}) == {:ok, {:tuple, 0, []}}
      assert quoted_pattern({:a}) == {:ok, {:tuple, 1, [{:atom, :a}]}}
      assert quoted_pattern({:a, 123}) == {:ok, {:tuple, 2, [{:atom, :a}, :integer]}}
    end

    test "map" do
      assert quoted_pattern(%{}) == {:ok, {:map, [{:optional, :dynamic, :dynamic}]}}

      assert quoted_pattern(%{a: :b}) ==
               {:ok,
                {:map, [{:required, {:atom, :a}, {:atom, :b}}, {:optional, :dynamic, :dynamic}]}}

      assert quoted_pattern(%{123 => a}) ==
               {:ok,
                {:map,
                 [
                   {:required, :integer, {:var, 0}},
                   {:optional, :dynamic, :dynamic}
                 ]}}

      assert quoted_pattern(%{123 => :foo, 456 => :bar}) ==
               {:ok,
                {:map,
                 [
                   {:required, :integer, :dynamic},
                   {:optional, :dynamic, :dynamic}
                 ]}}

      assert {:error, {:unable_unify, {:integer, {:atom, :foo}, _}}} =
               quoted_pattern(%{a: a = 123, b: a = :foo})
    end

    test "struct" do
      assert quoted_pattern(%:"Elixir.Module.Types.PatternTest.Struct"{}) ==
               {:ok,
                {:map,
                 [
                   {:required, {:atom, :__struct__}, {:atom, Module.Types.PatternTest.Struct}},
                   {:required, {:atom, :bar}, :dynamic},
                   {:required, {:atom, :baz}, :dynamic},
                   {:required, {:atom, :foo}, :dynamic}
                 ]}}

      assert quoted_pattern(%:"Elixir.Module.Types.PatternTest.Struct"{foo: 123, bar: :atom}) ==
               {:ok,
                {:map,
                 [
                   {:required, {:atom, :foo}, :integer},
                   {:required, {:atom, :bar}, {:atom, :atom}},
                   {:required, {:atom, :__struct__}, {:atom, Module.Types.PatternTest.Struct}},
                   {:required, {:atom, :baz}, :dynamic}
                 ]}}
    end

    test "struct var" do
      assert quoted_pattern(%var{}) ==
               {:ok,
                {:map,
                 [{:required, {:atom, :__struct__}, :atom}, {:optional, :dynamic, :dynamic}]}}

      assert quoted_pattern(%var{foo: 123}) ==
               {:ok,
                {:map,
                 [
                   {:required, {:atom, :__struct__}, :atom},
                   {:required, {:atom, :foo}, :integer},
                   {:optional, :dynamic, :dynamic}
                 ]}}

      assert quoted_pattern(%var{foo: var}) ==
               {:ok,
                {:map,
                 [
                   {:required, {:atom, :__struct__}, :atom},
                   {:required, {:atom, :foo}, :atom},
                   {:optional, :dynamic, :dynamic}
                 ]}}
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

      assert quoted_pattern({<<foo::integer>>, foo}) == {:ok, {:tuple, 2, [:binary, :integer]}}
      assert quoted_pattern({<<foo::binary>>, foo}) == {:ok, {:tuple, 2, [:binary, :binary]}}
      assert quoted_pattern({<<foo::utf8>>, foo}) == {:ok, {:tuple, 2, [:binary, :integer]}}

      assert {:error, {:unable_unify, {:binary, :integer, _}}} =
               quoted_pattern(<<foo::binary-0, foo::integer>>)
    end

    test "variables" do
      assert quoted_pattern(foo) == {:ok, {:var, 0}}
      assert quoted_pattern({foo}) == {:ok, {:tuple, 1, [{:var, 0}]}}
      assert quoted_pattern({foo, bar}) == {:ok, {:tuple, 2, [{:var, 0}, {:var, 1}]}}

      assert quoted_pattern(_) == {:ok, :dynamic}
      assert quoted_pattern({_ = 123, _}) == {:ok, {:tuple, 2, [:integer, :dynamic]}}
    end

    test "assignment" do
      assert quoted_pattern(x = y) == {:ok, {:var, 0}}
      assert quoted_pattern(x = 123) == {:ok, :integer}
      assert quoted_pattern({foo}) == {:ok, {:tuple, 1, [{:var, 0}]}}
      assert quoted_pattern({x = y}) == {:ok, {:tuple, 1, [{:var, 0}]}}

      assert quoted_pattern(x = y = 123) == {:ok, :integer}
      assert quoted_pattern(x = 123 = y) == {:ok, :integer}
      assert quoted_pattern(123 = x = y) == {:ok, :integer}

      assert {:error, {:unable_unify, {{:tuple, 1, [var: 0]}, {:var, 0}, _}}} =
               quoted_pattern({x} = x)
    end
  end

  describe "heads" do
    test "variable" do
      assert quoted_head([a]) == {:ok, [{:var, 0}]}
      assert quoted_head([a, b]) == {:ok, [{:var, 0}, {:var, 1}]}
      assert quoted_head([a, a]) == {:ok, [{:var, 0}, {:var, 0}]}

      assert {:ok, [{:var, 0}, {:var, 0}], _} =
               Pattern.of_head(
                 [{:a, [version: 0], :foo}, {:a, [version: 0], :foo}],
                 [],
                 new_stack(),
                 new_context()
               )

      assert {:ok, [{:var, 0}, {:var, 1}], _} =
               Pattern.of_head(
                 [{:a, [version: 0], :foo}, {:a, [version: 1], :foo}],
                 [],
                 new_stack(),
                 new_context()
               )
    end

    test "assignment" do
      assert quoted_head([x = y, x = y]) == {:ok, [{:var, 0}, {:var, 0}]}
      assert quoted_head([x = y, y = x]) == {:ok, [{:var, 0}, {:var, 0}]}

      assert quoted_head([x = :foo, x = y, y = z]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert quoted_head([x = y, y = :foo, y = z]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert quoted_head([x = y, y = z, z = :foo]) ==
               {:ok, [{:atom, :foo}, {:atom, :foo}, {:atom, :foo}]}

      assert {:error, {:unable_unify, {{:tuple, 1, [var: 1]}, {:var, 0}, _}}} =
               quoted_head([{x} = y, {y} = x])
    end

    test "guards" do
      assert quoted_head([x], [is_binary(x)]) == {:ok, [:binary]}

      assert quoted_head([x, y], [is_binary(x) and is_atom(y)]) ==
               {:ok, [:binary, :atom]}

      assert quoted_head([x], [is_binary(x) or is_atom(x)]) ==
               {:ok, [{:union, [:binary, :atom]}]}

      assert quoted_head([x, x], [is_integer(x)]) == {:ok, [:integer, :integer]}

      assert quoted_head([x = 123], [is_integer(x)]) == {:ok, [:integer]}

      assert quoted_head([x], [is_boolean(x) or is_atom(x)]) ==
               {:ok, [:atom]}

      assert quoted_head([x], [is_atom(x) or is_boolean(x)]) ==
               {:ok, [:atom]}

      assert quoted_head([x], [is_tuple(x) or is_atom(x)]) ==
               {:ok, [{:union, [:tuple, :atom]}]}

      assert quoted_head([x], [is_boolean(x) and is_atom(x)]) ==
               {:ok, [{:union, [atom: true, atom: false]}]}

      assert quoted_head([x], [is_atom(x) and is_boolean(x)]) ==
               {:ok, [{:union, [atom: true, atom: false]}]}

      assert quoted_head([x], [is_atom(x) > :foo]) == {:ok, [var: 0]}

      assert quoted_head([x, x = y, y = z], [is_atom(x)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_head([x = y, y, y = z], [is_atom(y)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_head([x = y, y = z, z], [is_atom(z)]) ==
               {:ok, [:atom, :atom, :atom]}

      assert quoted_head([x, y], [is_atom(x) or is_integer(y)]) ==
               {:ok, [{:var, 0}, {:var, 1}]}

      assert quoted_head([x], [is_atom(x) or is_atom(x)]) ==
               {:ok, [:atom]}

      assert quoted_head([x, y], [(is_atom(x) and is_atom(y)) or (is_atom(x) and is_integer(y))]) ==
               {:ok, [:atom, union: [:atom, :integer]]}

      assert quoted_head([x, y], [is_atom(x) or is_integer(x)]) ==
               {:ok, [union: [:atom, :integer], var: 0]}

      assert quoted_head([x, y], [is_atom(y) or is_integer(y)]) ==
               {:ok, [{:var, 0}, {:union, [:atom, :integer]}]}

      assert quoted_head([x = y], [is_atom(y) or is_integer(y)]) ==
               {:ok, [{:union, [:atom, :integer]}]}

      assert quoted_head([x = y], [is_atom(x) or is_integer(x)]) ==
               {:ok, [{:union, [:atom, :integer]}]}

      assert quoted_head([x = y], [is_atom(x) or is_integer(x)]) ==
               {:ok, [{:union, [:atom, :integer]}]}

      assert quoted_head([x], [true == false or is_integer(x)]) ==
               {:ok, [var: 0]}

      assert {:error, {:unable_unify, {:binary, :integer, _}}} =
               quoted_head([x], [is_binary(x) and is_integer(x)])

      assert {:error, {:unable_unify, {:tuple, :atom, _}}} =
               quoted_head([x], [is_tuple(x) and is_atom(x)])

      assert {:error, {:unable_unify, {{:atom, true}, :tuple, _}}} =
               quoted_head([x], [is_tuple(is_atom(x))])
    end

    test "guard and" do
      assert quoted_head([], [(true and 1) > 0]) == {:ok, []}

      assert quoted_head(
               [struct],
               [is_map_key(struct, :map) and map_size(:erlang.map_get(:map, struct))]
             ) == {:ok, [{:map, [{:optional, :dynamic, :dynamic}]}]}
    end

    test "nested calls with interesections in guards" do
      assert quoted_head([x], [:erlang.rem(x, 2)]) == {:ok, [:integer]}
      assert quoted_head([x], [:erlang.rem(x + x, 2)]) == {:ok, [{:union, [:integer, :float]}]}
    end

    test "erlang-only guards" do
      assert quoted_head([x], [:erlang.size(x)]) ==
               {:ok, [{:union, [:binary, :tuple]}]}
    end

    test "failing guard functions" do
      assert quoted_head([x], [length([])]) == {:ok, [{:var, 0}]}

      assert {:error, {:unable_unify, {{:atom, :foo}, {:list, :dynamic}, _}}} =
               quoted_head([x], [length(:foo)])

      assert {:error, {:unable_unify, {{:atom, true}, {:list, :dynamic}, _}}} =
               quoted_head([x], [length(is_tuple(x))])

      assert {:error, {:unable_unify, {{:atom, true}, :tuple, _}}} =
               quoted_head([x], [elem(is_tuple(x), 0)])

      assert {:error, {:unable_unify, {{:atom, true}, {:union, [:integer, :float]}, _}}} =
               quoted_head([x], [elem({}, is_tuple(x))])

      assert quoted_head([x], [elem({}, 1)]) == {:ok, [var: 0]}

      assert quoted_head([x], [elem(x, 1) == :foo]) == {:ok, [:tuple]}

      assert quoted_head([x], [is_tuple(x) and elem(x, 1)]) == {:ok, [:tuple]}

      assert quoted_head([x], [length(x) == 0 or elem(x, 1)]) == {:ok, [{:list, :dynamic}]}

      assert quoted_head([x], [
               (is_list(x) and length(x) == 0) or (is_tuple(x) and elem(x, 1))
             ]) ==
               {:ok, [{:union, [{:list, :dynamic}, :tuple]}]}

      assert quoted_head([x], [
               (length(x) == 0 and is_list(x)) or (elem(x, 1) and is_tuple(x))
             ]) == {:ok, [{:list, :dynamic}]}

      assert quoted_head([x], [elem(x, 1) or is_atom(x)]) == {:ok, [:tuple]}

      assert quoted_head([x], [is_atom(x) or elem(x, 1)]) == {:ok, [{:union, [:atom, :tuple]}]}

      assert quoted_head([x, y], [elem(x, 1) and is_atom(y)]) == {:ok, [:tuple, :atom]}

      assert quoted_head([x, y], [elem(x, 1) or is_atom(y)]) == {:ok, [:tuple, {:var, 0}]}

      assert {:error, {:unable_unify, {:tuple, :atom, _}}} =
               quoted_head([x], [elem(x, 1) and is_atom(x)])
    end

    test "map" do
      assert quoted_head([%{true: false} = foo, %{} = foo]) ==
               {:ok,
                [
                  {:map,
                   [{:required, {:atom, true}, {:atom, false}}, {:optional, :dynamic, :dynamic}]},
                  {:map,
                   [{:required, {:atom, true}, {:atom, false}}, {:optional, :dynamic, :dynamic}]}
                ]}

      assert quoted_head([%{true: bool}], [is_boolean(bool)]) ==
               {:ok,
                [
                  {:map,
                   [
                     {:required, {:atom, true}, {:union, [atom: true, atom: false]}},
                     {:optional, :dynamic, :dynamic}
                   ]}
                ]}

      assert quoted_head([%{true: true} = foo, %{false: false} = foo]) ==
               {:ok,
                [
                  {:map,
                   [
                     {:required, {:atom, false}, {:atom, false}},
                     {:required, {:atom, true}, {:atom, true}},
                     {:optional, :dynamic, :dynamic}
                   ]},
                  {:map,
                   [
                     {:required, {:atom, false}, {:atom, false}},
                     {:required, {:atom, true}, {:atom, true}},
                     {:optional, :dynamic, :dynamic}
                   ]}
                ]}

      assert {:error,
              {:unable_unify,
               {{:map, [{:required, {:atom, true}, {:atom, true}}]},
                {:map, [{:required, {:atom, true}, {:atom, false}}]},
                _}}} = quoted_head([%{true: false} = foo, %{true: true} = foo])
    end

    test "binary in guards" do
      assert quoted_head([a, b], [byte_size(a <> b) > 0]) ==
               {:ok, [:binary, :binary]}

      assert quoted_head([map], [byte_size(map.a <> map.b) > 0]) ==
               {:ok, [map: [{:optional, :dynamic, :dynamic}]]}
    end

    test "struct var guard" do
      assert quoted_head([%var{}], [is_atom(var)]) ==
               {:ok,
                [
                  {:map,
                   [{:required, {:atom, :__struct__}, :atom}, {:optional, :dynamic, :dynamic}]}
                ]}

      assert {:error, {:unable_unify, {:atom, :integer, _}}} =
               quoted_head([%var{}], [is_integer(var)])
    end
  end
end
