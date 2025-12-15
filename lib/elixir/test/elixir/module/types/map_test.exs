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
      assert typecheck!(:maps.take(:key, %{key: 123})) ==
               tuple([integer(), empty_map()]) |> union(atom([:error]))

      assert typecheck!([x], :maps.take(:key, x)) ==
               union(
                 dynamic(tuple([term(), open_map(key: not_set())])),
                 atom([:error])
               )

      assert typecheck!([condition?, x], :maps.take(if(condition?, do: :foo, else: :bar), x)) ==
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

      assert typecheck!([x], :maps.take(123, x)) ==
               union(
                 dynamic(tuple([term(), open_map()])),
                 atom([:error])
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
               empty_map()

      assert typecheck!(Map.delete(%{key: 123}, :key)) ==
               empty_map()

      assert typecheck!([x], Map.delete(x, :key)) ==
               dynamic(open_map(key: not_set()))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.delete(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) ==
               union(
                 empty_map(),
                 closed_map(foo: integer())
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

      assert typecheck!([x], Map.fetch(x, :key)) ==
               dynamic(tuple([atom([:ok]), term()])) |> union(atom([:error]))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.fetch(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) == tuple([atom([:ok]), integer()]) |> union(atom([:error]))

      assert typecheck!([x], Map.fetch(x, 123)) ==
               dynamic(tuple([atom([:ok]), term()])) |> union(atom([:error]))
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

      assert typecheck!([x], Map.fetch!(x, 123)) == dynamic()
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

  describe "Map.from_struct/1" do
    test "checking" do
      assert typecheck!(Map.from_struct(%{})) ==
               empty_map()

      assert typecheck!(Map.from_struct(%{key: 123})) ==
               closed_map(key: integer())

      assert typecheck!(Map.from_struct(%URI{})) ==
               closed_map(
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

  describe "Map.get/2" do
    test "checking" do
      assert typecheck!(Map.get(%{key: 123}, :key)) == integer() |> union(atom([nil]))

      assert typecheck!([x], Map.get(x, :key)) == dynamic(term()) |> union(atom([nil]))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.get(%{foo: 123}, if(condition?, do: :foo, else: :bar))
             ) == integer() |> union(atom([nil]))

      assert typecheck!([x], Map.get(x, 123)) == dynamic(term()) |> union(atom([nil]))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.get(x, :key)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.get(%{}, :foo)) =~
               """
               incompatible types given to Map.get/2:

                   Map.get(%{}, :foo)

               the map:

                   empty_map()

               does not have all required keys:

                   :foo

               therefore this function will always return nil
               """
    end
  end

  describe "Map.get/3" do
    test "checking" do
      assert typecheck!(Map.get(%{key: 123}, :key, 123)) == integer()

      assert typecheck!([x], Map.get(x, :key, 123)) == dynamic(term()) |> union(integer())

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.get(%{foo: 123}, if(condition?, do: :foo, else: :bar), 123)
             ) == integer()

      assert typecheck!([x], Map.get(x, 123, 123)) == dynamic(term()) |> union(integer())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.get(x, :key, 123)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.get(%{}, :foo, 123)) =~
               """
               incompatible types given to Map.get/3:

                   Map.get(%{}, :foo, 123)

               the map:

                   empty_map()

               does not have all required keys:

                   :foo

               therefore this function will always return integer()
               """
    end
  end

  describe "Map.get_lazy/3" do
    test "checking" do
      assert typecheck!(Map.get_lazy(%{key: 123}, :key, fn -> 123 end))
             |> equal?(integer())

      assert typecheck!([x], Map.get_lazy(x, :key, fn -> 123 end)) ==
               dynamic(term())

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.get_lazy(%{foo: 123}, if(condition?, do: :foo, else: :bar), fn -> 123 end)
             )
             |> equal?(integer())

      assert typecheck!([x], Map.get_lazy(x, 123, fn -> 123 end)) ==
               dynamic(term())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.get_lazy(x, :key, fn -> 123 end)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.get_lazy(%{}, :foo, fn -> 123 end)) =~
               """
               incompatible types given to Map.get_lazy/3:

                   Map.get_lazy(%{}, :foo, fn -> 123 end)

               the map:

                   empty_map()

               does not have all required keys:

                   :foo

               therefore this function will always return integer()
               """

      assert typeerror!(Map.get_lazy(%{}, :foo, 123)) =~
               """
               expected a 0-arity function on function call within Map.get_lazy/3:

                   Map.get_lazy(%{}, :foo, 123)

               but got type:

                   integer()
               """
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

  describe "Map.pop/2" do
    test "checking" do
      assert typecheck!(Map.pop(%{key: 123}, :key)) ==
               tuple([union(integer(), atom([nil])), empty_map()])

      assert typecheck!([x], Map.pop(x, :key)) ==
               dynamic(tuple([term(), open_map(key: not_set())]))

      assert typecheck!([condition?, x], Map.pop(x, if(condition?, do: :foo, else: :bar))) ==
               dynamic(
                 tuple([
                   term(),
                   union(
                     open_map(foo: not_set()),
                     open_map(bar: not_set())
                   )
                 ])
               )

      assert typecheck!(
               [x],
               (
                 x = %{String.to_integer(x) => :before}
                 Map.pop(x, 123)
               )
             ) ==
               tuple([
                 atom([:before, nil]),
                 closed_map([{domain_key(:integer), atom([:before])}])
               ])
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.pop(x, :key)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.pop(x, :foo)) =~
               "incompatible types given to Map.pop/2"

      assert typeerror!(Map.pop(%{}, :key)) =~ """
             incompatible types given to Map.pop/2:

                 Map.pop(%{}, :key)

             the map:

                 empty_map()

             does not have all required keys:

                 :key

             """
    end
  end

  describe "Map.pop_lazy/3" do
    test "checking" do
      assert typecheck!(Map.pop_lazy(%{key: 123}, :key, fn -> :error end)) ==
               dynamic(tuple([union(integer(), atom([:error])), empty_map()]))

      assert typecheck!([x], Map.pop_lazy(x, :key, fn -> :error end)) ==
               dynamic(tuple([term(), open_map(key: not_set())]))

      assert typecheck!(
               [condition?, x],
               Map.pop_lazy(x, if(condition?, do: :foo, else: :bar), fn -> :error end)
             ) ==
               dynamic(
                 tuple([
                   term(),
                   union(
                     open_map(foo: not_set()),
                     open_map(bar: not_set())
                   )
                 ])
               )

      assert typecheck!(
               [x],
               (
                 x = %{String.to_integer(x) => :before}
                 Map.pop_lazy(x, 123, fn -> :after end)
               )
             ) ==
               dynamic(
                 tuple([
                   atom([:before, :after]),
                   closed_map([{domain_key(:integer), atom([:before])}])
                 ])
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.pop_lazy(x, :key, fn -> :error end)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.pop_lazy(x, :foo, fn -> :error end)) =~
               "incompatible types given to Map.pop_lazy/3"

      assert typeerror!(Map.pop_lazy(%{}, :key, fn -> :error end)) =~ """
             incompatible types given to Map.pop_lazy/3:

                 Map.pop_lazy(%{}, :key, fn -> :error end)

             the map:

                 empty_map()

             does not have all required keys:

                 :key

             """

      assert typeerror!(Map.pop_lazy(%{}, :foo, 123)) =~
               """
               expected a 0-arity function on function call within Map.pop_lazy/3:

                   Map.pop_lazy(%{}, :foo, 123)

               but got type:

                   integer()
               """
    end
  end

  describe "Map.pop/3" do
    test "checking" do
      assert typecheck!(Map.pop(%{key: 123}, :key, :error)) ==
               tuple([union(integer(), atom([:error])), empty_map()])

      assert typecheck!([x], Map.pop(x, :key, :error)) ==
               dynamic(tuple([term(), open_map(key: not_set())]))

      assert typecheck!([condition?, x], Map.pop(x, if(condition?, do: :foo, else: :bar), :error)) ==
               dynamic(
                 tuple([
                   term(),
                   union(
                     open_map(foo: not_set()),
                     open_map(bar: not_set())
                   )
                 ])
               )

      assert typecheck!(
               [x],
               (
                 x = %{String.to_integer(x) => :before}
                 Map.pop(x, 123, :after)
               )
             ) ==
               tuple([
                 atom([:before, :after]),
                 closed_map([{domain_key(:integer), atom([:before])}])
               ])
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.pop(x, :key, :error)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.pop(x, :foo, :error)) =~
               "incompatible types given to Map.pop/3"

      assert typeerror!(Map.pop(%{}, :key, :error)) =~ """
             incompatible types given to Map.pop/3:

                 Map.pop(%{}, :key, :error)

             the map:

                 empty_map()

             does not have all required keys:

                 :key

             """
    end
  end

  describe "Map.pop!/2" do
    test "checking" do
      assert typecheck!(Map.pop!(%{key: 123}, :key)) ==
               tuple([integer(), empty_map()])

      assert typecheck!([x], Map.pop!(x, :key)) ==
               dynamic(tuple([term(), open_map(key: not_set())]))

      assert typecheck!([condition?, x], Map.pop!(x, if(condition?, do: :foo, else: :bar))) ==
               dynamic(
                 tuple([
                   term(),
                   union(
                     open_map(foo: not_set()),
                     open_map(bar: not_set())
                   )
                 ])
               )

      assert typecheck!([x], Map.pop!(x, 123)) ==
               dynamic(tuple([term(), open_map()]))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.pop!(x, :key)
                 x
               )
             ) == dynamic(open_map(key: term()))
    end

    test "errors" do
      assert typeerror!([x = []], Map.pop!(x, :foo)) =~
               "incompatible types given to Map.pop!/2"

      assert typeerror!(Map.pop!(%{}, :key)) =~ """
             incompatible types given to Map.pop!/2:

                 Map.pop!(%{}, :key)

             the map:

                 empty_map()

             does not have all required keys:

                 :key

             therefore this function will always raise
             """
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

  describe "Map.put_new_lazy/3" do
    test "checking" do
      assert typecheck!(Map.put_new_lazy(%{}, :key, fn -> :value end)) ==
               closed_map(key: atom([:value]))

      assert typecheck!(Map.put_new_lazy(%{key: 123}, :key, fn -> :value end)) ==
               closed_map(key: integer())

      assert typecheck!([x], Map.put_new_lazy(x, :key, fn -> :value end)) ==
               dynamic(open_map(key: term()))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.put_new_lazy(%{foo: 123}, if(condition?, do: :foo, else: :bar), fn -> "123" end)
             ) == union(closed_map(foo: integer()), closed_map(foo: integer(), bar: binary()))

      assert typecheck!([], Map.put_new_lazy(%{789 => "binary"}, 123, fn -> 456 end)) ==
               closed_map([{domain_key(:integer), union(binary(), integer())}])

      assert typecheck!([x], Map.put_new_lazy(x, 123, fn -> 456 end)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.put_new_lazy(x, :key, fn -> :value end)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.put_new_lazy(x, :key, fn -> :value end)) |> strip_ansi() =~
               """
               incompatible types given to Map.put_new_lazy/3:

                   Map.put_new_lazy(x, :key, fn -> :value end)

               given types:

                   empty_list(), :key, (-> dynamic(:value))

               but expected one of:

                   map(), term(), (-> term())
               """

      assert typeerror!(Map.put_new_lazy(%{}, :foo, 123)) =~
               """
               expected a 0-arity function on function call within Map.put_new_lazy/3:

                   Map.put_new_lazy(%{}, :foo, 123)

               but got type:

                   integer()
               """
    end
  end

  describe "Map.put_new/3" do
    test "checking" do
      assert typecheck!(Map.put_new(%{}, :key, :value)) ==
               closed_map(key: atom([:value]))

      assert typecheck!(Map.put_new(%{key: 123}, :key, :value)) ==
               closed_map(key: integer())

      assert typecheck!([x], Map.put_new(x, :key, :value)) ==
               dynamic(open_map(key: term()))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.put_new(%{foo: 123}, if(condition?, do: :foo, else: :bar), "123")
             ) == union(closed_map(foo: integer()), closed_map(foo: integer(), bar: binary()))

      assert typecheck!([], Map.put_new(%{789 => "binary"}, 123, 456)) ==
               closed_map([{domain_key(:integer), union(binary(), integer())}])

      assert typecheck!([x], Map.put_new(x, 123, 456)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.put_new(x, :key, :value)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.put_new(x, :key, :value)) |> strip_ansi() =~
               """
               incompatible types given to Map.put_new/3:

                   Map.put_new(x, :key, :value)

               given types:

                   empty_list(), :key, :value

               but expected one of:

                   map(), term(), term()
               """
    end
  end

  describe "Map.replace/3" do
    test "checking" do
      assert typecheck!(Map.replace(%{key: 123}, :key, :value)) ==
               closed_map(key: atom([:value]))

      assert typecheck!([x], Map.replace(x, :key, :value)) ==
               dynamic(open_map(key: atom([:value])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.replace(%{foo: 123}, if(condition?, do: :foo, else: :bar), "123")
             ) == closed_map(foo: binary())

      assert typecheck!([x], Map.replace(x, 123, 456)) == dynamic(open_map())
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.replace(x, :key, :value)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.replace(%{}, :key, :value)) =~
               """
               incompatible types given to Map.replace/3:

                   Map.replace(%{}, :key, :value)

               the map:

                   empty_map()

               does not have all required keys:

                   :key

               therefore this function will always do nothing
               """
    end
  end

  describe "Map.replace_lazy/3" do
    test "checking" do
      assert typecheck!(Map.replace_lazy(%{key: 123}, :key, fn _ -> :value end)) ==
               dynamic(closed_map(key: atom([:value])))

      assert typecheck!([x], Map.replace_lazy(x, :key, fn _ -> :value end)) ==
               dynamic(open_map(key: atom([:value])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.replace_lazy(%{foo: 123}, if(condition?, do: :foo, else: :bar), fn _ ->
                 "123"
               end)
             ) == dynamic(closed_map(foo: binary()))

      # Both succeed but different clauses
      assert typecheck!(
               [condition?],
               Map.replace_lazy(
                 %{key1: :foo, key2: :bar},
                 if(condition?, do: :key1, else: :key2),
                 fn
                   :foo -> 123
                   :bar -> 123.0
                 end
               )
             ) ==
               dynamic(
                 union(
                   closed_map(key1: atom([:foo]), key2: float()),
                   closed_map(key1: integer(), key2: atom([:bar]))
                 )
               )

      assert typecheck!([x], Map.replace_lazy(x, 123, fn _ -> 456 end)) == dynamic(open_map())

      assert typecheck!([], Map.replace_lazy(%{123 => 456}, 123, fn x -> x * 1.0 end)) ==
               dynamic(closed_map([{domain_key(:integer), union(integer(), float())}]))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.replace_lazy(x, :key, fn _ -> :value end)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.replace_lazy(%{}, :key, fn _ -> :value end)) =~
               """
               incompatible types given to Map.replace_lazy/3:

                   Map.replace_lazy(%{}, :key, fn _ -> :value end)

               the map:

                   empty_map()

               does not have all required keys:

                   :key

               therefore this function will always do nothing
               """

      assert typeerror!(Map.replace_lazy(%{key: :foo}, :key, fn :bar -> :value end))
             |> strip_ansi() =~
               """
               incompatible types given on function call within Map.replace_lazy/3:

                   Map.replace_lazy(%{key: :foo}, :key, fn :bar -> :value end)

               given types:

                   dynamic(:foo)

               but function has type:

                   (:bar -> dynamic(:value))
               """
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

  describe "Map.update/4" do
    test "checking" do
      assert typecheck!(Map.update(%{}, :key, :default, fn _ -> :value end)) ==
               dynamic(closed_map(key: atom([:default])))

      assert typecheck!(Map.update(%{key: 123}, :key, :default, fn _ -> :value end)) ==
               dynamic(closed_map(key: atom([:value])))

      assert typecheck!([x], Map.update(x, :key, :default, fn _ -> :value end)) ==
               dynamic(open_map(key: atom([:value, :default])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.update(%{foo: 123}, if(condition?, do: :foo, else: :bar), :default, fn _ ->
                 "123"
               end)
             ) ==
               dynamic(
                 union(
                   closed_map(foo: binary()),
                   closed_map(foo: integer(), bar: atom([:default]))
                 )
               )

      # Both succeed but different clauses
      assert typecheck!(
               [condition?],
               Map.update(
                 %{key1: :foo, key2: :bar},
                 if(condition?, do: :key1, else: :key2),
                 :default,
                 fn
                   :foo -> 123
                   :bar -> 123.0
                 end
               )
             ) ==
               dynamic(
                 union(
                   closed_map(key1: atom([:foo]), key2: float()),
                   closed_map(key1: integer(), key2: atom([:bar]))
                 )
               )

      assert typecheck!([x], Map.update(x, 123, :default, fn _ -> 456 end)) == dynamic(open_map())

      integer_to_integer_float_atom =
        dynamic(
          closed_map([
            {domain_key(:integer), integer() |> union(float()) |> union(atom([:default]))}
          ])
        )

      assert typecheck!([], Map.update(%{123 => 456}, 123, :default, fn x -> x * 1.0 end)) ==
               integer_to_integer_float_atom

      assert typecheck!([], Map.update(%{123 => 456}, 456, :default, fn x -> x * 1.0 end)) ==
               integer_to_integer_float_atom
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.update(x, :key, :default, fn _ -> :value end)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!(Map.update(%{key: :foo}, :key, :default, fn :bar -> :value end))
             |> strip_ansi() =~
               """
               incompatible types given on function call within Map.update/4:

                   Map.update(%{key: :foo}, :key, :default, fn :bar -> :value end)

               given types:

                   dynamic(:foo)

               but function has type:

                   (:bar -> dynamic(:value))
               """
    end
  end

  describe "Map.update!/3" do
    test "checking" do
      assert typecheck!(Map.update!(%{key: 123}, :key, fn _ -> :value end)) ==
               dynamic(closed_map(key: atom([:value])))

      assert typecheck!([x], Map.update!(x, :key, fn _ -> :value end)) ==
               dynamic(open_map(key: atom([:value])))

      # If one of them succeeds, we are still fine!
      assert typecheck!(
               [condition?],
               Map.update!(%{foo: 123}, if(condition?, do: :foo, else: :bar), fn _ -> "123" end)
             ) == dynamic(closed_map(foo: binary()))

      # Both succeed but different clauses
      assert typecheck!(
               [condition?],
               Map.update!(%{key1: :foo, key2: :bar}, if(condition?, do: :key1, else: :key2), fn
                 :foo -> 123
                 :bar -> 123.0
               end)
             ) ==
               dynamic(
                 union(
                   closed_map(key1: atom([:foo]), key2: float()),
                   closed_map(key1: integer(), key2: atom([:bar]))
                 )
               )

      assert typecheck!([x], Map.update!(x, 123, fn _ -> 456 end)) == dynamic(open_map())

      assert typecheck!([], Map.update!(%{123 => 456}, 123, fn x -> x * 1.0 end)) ==
               dynamic(closed_map([{domain_key(:integer), union(integer(), float())}]))

      assert typecheck!([], Map.update!(%{123 => 456}, 456, fn x -> x * 1.0 end)) ==
               dynamic(closed_map([{domain_key(:integer), union(integer(), float())}]))
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.update!(x, :key, fn _ -> :value end)
                 x
               )
             ) == dynamic(open_map(key: term()))
    end

    test "errors" do
      assert typeerror!(Map.update!(%{}, :key, fn _ -> :value end)) =~
               """
               incompatible types given to Map.update!/3:

                   Map.update!(%{}, :key, fn _ -> :value end)

               the map:

                   empty_map()

               does not have all required keys:

                   :key

               therefore this function will always raise
               """

      assert typeerror!(Map.update!(%{key: :foo}, :key, fn :bar -> :value end)) |> strip_ansi() =~
               """
               incompatible types given on function call within Map.update!/3:

                   Map.update!(%{key: :foo}, :key, fn :bar -> :value end)

               given types:

                   dynamic(:foo)

               but function has type:

                   (:bar -> dynamic(:value))
               """
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
