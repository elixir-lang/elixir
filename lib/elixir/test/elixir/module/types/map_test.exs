Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.MapTest do
  # This file holds cases for maps and structs.
  use ExUnit.Case, async: true

  import TypeHelper

  defmodule :"Elixir.Module.Types.MapTest.Struct" do
    defstruct foo: :atom, bar: 123, baz: %{}
  end

  test "map" do
    assert quoted_expr(%{}) == {:ok, {:map, []}}
    assert quoted_expr(%{a: :b}) == {:ok, {:map, [{:required, {:atom, :a}, {:atom, :b}}]}}
    assert quoted_expr([a], %{123 => a}) == {:ok, {:map, [{:required, :integer, {:var, 0}}]}}

    assert quoted_expr(%{123 => :foo, 456 => :bar}) ==
             {:ok, {:map, [{:required, :integer, {:union, [{:atom, :foo}, {:atom, :bar}]}}]}}
  end

  test "struct" do
    assert quoted_expr(%:"Elixir.Module.Types.MapTest.Struct"{}) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :bar}, :integer},
                 {:required, {:atom, :baz}, {:map, []}},
                 {:required, {:atom, :foo}, {:atom, :atom}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct}}
               ]}}

    assert quoted_expr(%:"Elixir.Module.Types.MapTest.Struct"{foo: 123, bar: :atom}) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :baz}, {:map, []}},
                 {:required, {:atom, :foo}, :integer},
                 {:required, {:atom, :bar}, {:atom, :atom}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct}}
               ]}}
  end

  test "map field" do
    assert quoted_expr(%{foo: :bar}.foo) == {:ok, {:atom, :bar}}

    assert quoted_expr(
             (
               map = %{foo: :bar}
               map.foo
             )
           ) == {:ok, {:atom, :bar}}

    assert quoted_expr(
             [map],
             (
               map.foo
               map.bar
               map
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :bar}, {:var, 0}},
                 {:required, {:atom, :foo}, {:var, 1}},
                 {:optional, :dynamic, :dynamic}
               ]}}

    assert quoted_expr(
             [map],
             (
               :foo = map.foo
               :bar = map.bar
               map
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :bar}, {:atom, :bar}},
                 {:required, {:atom, :foo}, {:atom, :foo}},
                 {:optional, :dynamic, :dynamic}
               ]}}

    assert {:error,
            {:unable_unify,
             {{:map, [{:required, {:atom, :bar}, {:var, 1}}, {:optional, :dynamic, :dynamic}]},
              {:map, [{:required, {:atom, :foo}, {:atom, :foo}}]},
              _}}} =
             quoted_expr(
               (
                 map = %{foo: :foo}
                 map.bar
               )
             )
  end

  defmodule :"Elixir.Module.Types.MapTest.Struct2" do
    defstruct [:field]
  end

  test "map and struct fields" do
    assert quoted_expr(
             [map],
             (
               %Module.Types.MapTest.Struct2{} = map
               map.field
               map
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :field}, {:var, 0}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}}
               ]}}

    assert quoted_expr(
             [map],
             (
               _ = map.field
               %Module.Types.MapTest.Struct2{} = map
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :field}, {:var, 0}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}}
               ]}}

    assert {:error, {:unable_unify, {_, _, _}}} =
             quoted_expr(
               [map],
               (
                 %Module.Types.MapTest.Struct2{} = map
                 map.no_field
               )
             )

    assert {:error, {:unable_unify, {_, _, _}}} =
             quoted_expr(
               [map],
               (
                 _ = map.no_field
                 %Module.Types.MapTest.Struct2{} = map
               )
             )
  end

  test "map pattern" do
    assert quoted_expr(%{a: :b} = %{a: :b}) ==
             {:ok, {:map, [{:required, {:atom, :a}, {:atom, :b}}]}}

    assert quoted_expr(
             (
               a = :a
               %{^a => :b} = %{:a => :b}
             )
           ) == {:ok, {:map, [{:required, {:atom, :a}, {:atom, :b}}]}}

    assert {:error,
            {:unable_unify,
             {{:map, [{:required, {:atom, :c}, {:atom, :d}}]},
              {:map, [{:required, {:atom, :a}, {:atom, :b}}, {:optional, :dynamic, :dynamic}]},
              _}}} = quoted_expr(%{a: :b} = %{c: :d})

    assert {:error,
            {:unable_unify,
             {{:map, [{:required, {:atom, :b}, {:atom, :error}}]},
              {:map, [{:required, {:var, 0}, {:atom, :ok}}, {:optional, :dynamic, :dynamic}]},
              _}}} =
             quoted_expr(
               (
                 a = :a
                 %{^a => :ok} = %{:b => :error}
               )
             )
  end

  test "map update" do
    assert quoted_expr(
             (
               map = %{foo: :a}
               %{map | foo: :b}
             )
           ) ==
             {:ok, {:map, [{:required, {:atom, :foo}, {:atom, :b}}]}}

    assert quoted_expr([map], %{map | foo: :b}) ==
             {:ok,
              {:map, [{:required, {:atom, :foo}, {:atom, :b}}, {:optional, :dynamic, :dynamic}]}}

    assert {:error,
            {:unable_unify,
             {{:map, [{:required, {:atom, :bar}, :dynamic}, {:optional, :dynamic, :dynamic}]},
              {:map, [{:required, {:atom, :foo}, {:atom, :a}}]},
              _}}} =
             quoted_expr(
               (
                 map = %{foo: :a}
                 %{map | bar: :b}
               )
             )
  end

  test "struct update" do
    assert quoted_expr(
             (
               map = %Module.Types.MapTest.Struct2{field: :a}
               %Module.Types.MapTest.Struct2{map | field: :b}
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :field}, {:atom, :b}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}}
               ]}}

    # TODO: improve error message to translate to MULTIPLE missing fields
    assert {:error,
            {:unable_unify,
             {{:map,
               [
                 {:required, {:atom, :field}, {:atom, :b}},
                 {:required, {:atom, :foo}, {:var, 1}},
                 {:optional, :dynamic, :dynamic}
               ]},
              {:map,
               [
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}},
                 {:required, {:atom, :field}, :dynamic}
               ]},
              _}}} =
             quoted_expr(
               [map],
               (
                 _ = map.foo
                 %Module.Types.MapTest.Struct2{map | field: :b}
               )
             )

    assert {:error,
            {:unable_unify,
             {{:map, [{:required, {:atom, :field}, {:atom, :b}}]},
              {:map,
               [
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}},
                 {:required, {:atom, :field}, :dynamic}
               ]},
              _}}} =
             quoted_expr(
               (
                 map = %{field: :a}
                 %Module.Types.MapTest.Struct2{map | field: :b}
               )
             )

    assert quoted_expr([map], %Module.Types.MapTest.Struct2{map | field: :b}) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :field}, {:atom, :b}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}}
               ]}}

    assert {:error,
            {:unable_unify,
             {{:map,
               [{:required, {:atom, :not_field}, :dynamic}, {:optional, :dynamic, :dynamic}]},
              {:map,
               [
                 {:required, {:atom, :field}, {:atom, nil}},
                 {:required, {:atom, :__struct__}, {:atom, Module.Types.MapTest.Struct2}}
               ]},
              _}}} =
             quoted_expr(
               (
                 map = %Module.Types.MapTest.Struct2{}
                 %{map | not_field: :b}
               )
             )
  end

  describe "in guards" do
    test "not is_struct/2" do
      assert quoted_expr([var], [not is_struct(var, URI)], var.name) == {:ok, {:var, 0}}
    end

    test "map guards" do
      assert quoted_expr([var], [is_map(var)], var.foo) == {:ok, {:var, 0}}
      assert quoted_expr([var], [is_map_key(var, :bar)], var.foo) == {:ok, {:var, 0}}
      assert quoted_expr([var], [:erlang.map_get(:bar, var)], var.foo) == {:ok, {:var, 0}}
      assert quoted_expr([var], [map_size(var) == 1], var.foo) == {:ok, {:var, 0}}
    end
  end

  test "map creation with bound var keys" do
    assert quoted_expr(
             [atom, bool, true = var],
             [is_atom(atom) and is_boolean(bool)],
             %{atom => :atom, bool => :bool, var => true}
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, true}, {:atom, true}},
                 {:required, {:union, [atom: true, atom: false]},
                  {:union, [{:atom, :bool}, {:atom, true}]}},
                 {:required, :atom, {:union, [{:atom, :atom}, {:atom, :bool}, {:atom, true}]}}
               ]}}

    assert quoted_expr(
             [atom, bool, true = var],
             [is_atom(atom) and is_boolean(bool)],
             %{var => true, bool => :bool, atom => :atom}
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, true}, {:atom, true}},
                 {:required, {:union, [atom: true, atom: false]},
                  {:union, [{:atom, :bool}, {:atom, true}]}},
                 {:required, :atom, {:union, [{:atom, :atom}, {:atom, :bool}, {:atom, true}]}}
               ]}}

    assert quoted_expr(
             [atom, bool, true = var],
             [is_atom(atom) and is_boolean(bool)],
             %{var => true, atom => :atom, bool => :bool}
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, true}, {:atom, true}},
                 {:required, {:union, [atom: true, atom: false]},
                  {:union, [{:atom, :bool}, {:atom, true}]}},
                 {:required, :atom, {:union, [{:atom, :atom}, {:atom, :bool}, {:atom, true}]}}
               ]}}
  end

  test "map creation with unbound var keys" do
    assert quoted_expr(
             [var, struct],
             (
               map = %{var => :foo}
               %^var{} = struct
               map
             )
           ) == {:ok, {:map, [{:required, :atom, {:atom, :foo}}]}}

    # If we have multiple keys, the unbound key must become required(dynamic) => dynamic
    assert quoted_expr(
             [var, struct],
             (
               map = %{var => :foo, :foo => :bar}
               %^var{} = struct
               map
             )
           ) ==
             {:ok,
              {:map,
               [
                 {:required, {:atom, :foo}, {:atom, :bar}},
                 {:required, :dynamic, :dynamic}
               ]}}
  end
end
