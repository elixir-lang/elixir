# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.MapTest do
  # Tests for the Map module
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr
  defmacro domain_key(arg) when is_atom(arg), do: [arg]

  describe "inferred" do
    test "Map.new/0" do
      assert typecheck!(Map.new()) == dynamic(empty_map())
    end

    test "Map.equal?/2" do
      assert typecheck!([x, y], {Map.equal?(x, y), x, y}) ==
               dynamic(tuple([boolean(), open_map(), open_map()]))
    end
  end

  describe ":maps.take/2" do
    test "checking" do
      assert typecheck!(:maps.take(:key, %{key: 123})) |> equal?(tuple([integer(), empty_map()]))

      assert typecheck!([x], :maps.take(:key, x))
             |> equal?(
               union(
                 dynamic(tuple([term(), open_map(key: not_set())])),
                 atom([:error])
               )
             )

      assert typecheck!([condition?, x], :maps.take(if(condition?, do: :foo, else: :bar), x))
             |> equal?(
               union(
                 dynamic(
                   tuple([
                     term(),
                     union(
                       open_map(foo: not_set()),
                       open_map(bar: not_set())
                     )
                   ])
                 ),
                 atom([:error])
               )
             )

      assert typecheck!([x], :maps.take(123, x))
             |> equal?(
               union(
                 dynamic(tuple([term(), open_map()])),
                 atom([:error])
               )
             )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = :maps.take(:key, x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], :maps.take(:foo, x)) =~
               "incompatible types given to :maps.take/2"

      assert typeerror!(:maps.take(:key, %{})) =~ """
             incompatible types given to :maps.take/2:

                 :maps.take(:key, %{})

             the map:

                 empty_map()

             does not have all required keys:

                 :key

             therefore this function will always return :error
             """
    end
  end

  describe "Map.delete/2" do
    test "checking" do
      assert typecheck!(Map.delete(%{}, :key)) ==
               closed_map(key: not_set())

      assert typecheck!(Map.delete(%{key: 123}, :key)) ==
               closed_map(key: not_set())

      assert typecheck!([x], Map.delete(x, :key)) ==
               dynamic(open_map(key: not_set()))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.delete(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) ==
               union(
                 closed_map(foo: not_set()),
                 closed_map(foo: integer(), bar: not_set())
               )

      assert typecheck!([x], Map.delete(x, 123)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.delete(x, :key)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.delete(x, :key)) =~
               "incompatible types given to Map.delete/2"
    end

    test "combined with put" do
      assert typecheck!([x], x |> Map.delete(:key) |> Map.put(:key, "123")) ==
               dynamic(open_map(key: binary()))

      assert typecheck!([x, y], x |> Map.delete(:key) |> Map.put(String.to_atom(y), "123")) ==
               dynamic(open_map(key: if_set(binary())))
    end
  end

  describe "Map.fetch/2" do
    test "checking" do
      assert typecheck!(Map.fetch(%{key: 123}, :key)) ==
               tuple([atom([:ok]), integer()]) |> union(atom([:error]))

      assert typecheck!([x], Map.fetch(x, :key))
             |> equal?(dynamic(tuple([atom([:ok]), term()])) |> union(atom([:error])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.fetch(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) == tuple([atom([:ok]), integer()]) |> union(atom([:error]))

      assert typecheck!([x], Map.fetch(x, 123))
             |> equal?(dynamic(tuple([atom([:ok]), term()])) |> union(atom([:error])))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.fetch(x, :key)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.fetch(%{}, :foo)) =~
               """
               incompatible types given to Map.fetch/2:

                   Map.fetch(%{}, :foo)

               the map:

                   empty_map()

               does not have all required keys:

                   :foo

               therefore this function will always return :error
               """
    end
  end

  describe "Map.fetch!/2" do
    test "checking" do
      assert typecheck!(Map.fetch!(%{key: 123}, :key)) == integer()

      assert typecheck!([x], Map.fetch!(x, :key)) == dynamic()

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.fetch!(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) == integer()

      assert typecheck!([x], Map.fetch!(x, 123)) |> equal?(dynamic())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 y = Integer.to_string(Map.fetch!(x, :key))
                 {x, y}
               )
             ) == dynamic(tuple([open_map(key: integer()), binary()]))
    end

    test "errors" do
      assert typeerror!(Map.fetch!(%{}, :foo)) =~
               """
               incompatible types given to Map.fetch!/2:

                   Map.fetch!(%{}, :foo)

               the map:

                   empty_map()

               does not have all required keys:

                   :foo

               therefore this function will always raise
               """

      assert typeerror!(Map.fetch!(%{}, 123)) =~
               """
               incompatible types given to Map.fetch!/2:

                   Map.fetch!(%{}, 123)

               the map:

                   empty_map()

               does not have all required keys:

                   integer()

               therefore this function will always raise
               """
    end
  end

  describe "Map.from_struct/1" do
    test "checking" do
      assert typecheck!(Map.from_struct(%{})) ==
               closed_map(__struct__: not_set())

      assert typecheck!(Map.from_struct(%{key: 123})) ==
               closed_map(key: integer(), __struct__: not_set())

      assert typecheck!(Map.from_struct(%URI{})) ==
               closed_map(
                 __struct__: not_set(),
                 authority: atom([nil]),
                 fragment: atom([nil]),
                 host: atom([nil]),
                 path: atom([nil]),
                 port: atom([nil]),
                 query: atom([nil]),
                 scheme: atom([nil]),
                 userinfo: atom([nil])
               )

      assert typecheck!([x], Map.from_struct(x)) ==
               dynamic(open_map(__struct__: not_set()))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.from_struct(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.from_struct(x)) =~
               "incompatible types given to Map.from_struct/1"
    end
  end

  describe "Map.put/3" do
    test "checking" do
      assert typecheck!(Map.put(%{}, :key, :value)) ==
               closed_map(key: atom([:value]))

      assert typecheck!(Map.put(%{key: 123}, :key, :value)) ==
               closed_map(key: atom([:value]))

      assert typecheck!([x], Map.put(x, :key, :value)) ==
               dynamic(open_map(key: atom([:value])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.put(%{foo: 123}, if(condition?, do: :foo, else: :bar), "123")
             ) ==
               union(
                 closed_map(foo: binary()),
                 closed_map(foo: integer(), bar: binary())
               )

      assert typecheck!([x], Map.put(x, 123, 456)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.put(x, :key, :value)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.put(x, :key, :value)) =~
               "incompatible types given to Map.put/3"
    end
  end

  describe "Map.replace!/3" do
    test "checking" do
      assert typecheck!(Map.replace!(%{key: 123}, :key, :value)) ==
               closed_map(key: atom([:value]))

      assert typecheck!([x], Map.replace!(x, :key, :value)) ==
               dynamic(open_map(key: atom([:value])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.replace!(%{foo: 123}, if(condition?, do: :foo, else: :bar), "123")
             ) == closed_map(foo: binary())

      assert typecheck!([x], Map.replace!(x, 123, 456)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.replace!(x, :key, :value)
                 x
               )
             ) == dynamic(open_map(key: term()))
    end

    test "errors" do
      assert typeerror!(Map.replace!(%{}, :key, :value)) =~
               """
               incompatible types given to Map.replace!/3:

                   Map.replace!(%{}, :key, :value)

               the map:

                   empty_map()

               does not have all required keys:

                   :key

               therefore this function will always raise
               """
    end
  end

  describe "Map.to_list/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.to_list(x)) == dynamic(list(tuple([term(), term()])))

      assert typecheck!(
               (
                 x = %{}
                 Map.to_list(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.to_list(x)
               )
             ) ==
               list(tuple([binary(), atom([:three])]))

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.to_list(x)
               )
             ) ==
               non_empty_list(
                 union(tuple([atom([:a]), integer()]), tuple([atom([:b]), binary()]))
               )

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.to_list(x)
               )
             ) ==
               non_empty_list(
                 tuple([atom([:a]), integer()])
                 |> union(tuple([atom([:b]), binary()]))
                 |> union(tuple([binary(), atom([:three])]))
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.to_list(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.to_list(x)) =~ "incompatible types given to Map.to_list/1"
    end
  end

  describe "Map.from_keys/2" do
    test "checking" do
      assert typecheck!([], Map.from_keys([], :value)) ==
               empty_map()

      assert typecheck!([x], Map.from_keys(x, :value)) ==
               open_map()

      assert typecheck!(
               (
                 x = [:key1, :key2]
                 Map.from_keys(x, 123)
               )
             ) ==
               closed_map(key1: if_set(integer()), key2: if_set(integer()))
               |> difference(empty_map())

      assert typecheck!(
               [condition?],
               (
                 x = if condition?, do: [123, "123"], else: []
                 Map.from_keys(x, 123)
               )
             ) ==
               closed_map([
                 {domain_key(:integer), if_set(integer())},
                 {domain_key(:binary), if_set(integer())}
               ])
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.from_keys(x, :value)
                 x
               )
             ) == dynamic(list(term()))
    end

    test "errors" do
      assert typeerror!([x = %{}], Map.from_keys(x, :value)) =~
               "incompatible types given to Map.from_keys/2"
    end
  end

  describe "Map.keys/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.keys(x)) == dynamic(list(term()))

      assert typecheck!(
               (
                 x = %{}
                 Map.keys(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.keys(x)
               )
             ) ==
               list(binary())

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.keys(x)
               )
             ) ==
               non_empty_list(union(atom([:a]), atom([:b])))

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.keys(x)
               )
             ) ==
               non_empty_list(
                 atom([:a])
                 |> union(atom([:b]))
                 |> union(binary())
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.keys(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.keys(x)) =~ "incompatible types given to Map.keys/1"
    end
  end

  describe "Map.values/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.values(x)) == dynamic(list(term()))

      assert typecheck!(
               (
                 x = %{}
                 Map.values(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.values(x)
               )
             ) ==
               list(atom([:three]))

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.values(x)
               )
             ) ==
               non_empty_list(union(integer(), binary()))

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.values(x)
               )
             ) ==
               non_empty_list(
                 integer()
                 |> union(binary())
                 |> union(atom([:three]))
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.values(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.values(x)) =~ "incompatible types given to Map.values/1"
    end
  end
end
