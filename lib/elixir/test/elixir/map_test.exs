Code.require_file("test_helper.exs", __DIR__)

defmodule MapTest do
  use ExUnit.Case, async: true

  doctest Map

  @sample %{a: 1, b: 2}

  test "maps in attributes" do
    assert @sample == %{a: 1, b: 2}
  end

  test "maps when quoted" do
    assert quote(do: %{foo: 1}) == {:%{}, [], [{:foo, 1}]}
  end

  test "maps keywords and atoms" do
    assert [%{}: :%] == [{:%{}, :%}]
    assert [%: :%{}] == [{:%, :%{}}]
  end

  test "maps with variables" do
    a = 0
    assert %{a: a = 1, b: a} == %{a: 1, b: 0}
    assert a == 1
  end

  test "maps with generated variables in key" do
    assert %{"#{1}" => 1} == %{"1" => 1}
    assert %{for(x <- 1..3, do: x) => 1} == %{[1, 2, 3] => 1}
    assert %{with(x = 1, do: x) => 1} == %{1 => 1}
    assert %{with({:ok, x} <- {:ok, 1}, do: x) => 1} == %{1 => 1}

    assert %{
             try do
               raise "error"
             rescue
               _ -> 1
             end => 1
           } == %{1 => 1}

    assert %{
             try do
               throw(1)
             catch
               x -> x
             end => 1
           } == %{1 => 1}

    assert %{
             try do
               a = 1
               a
             rescue
               _ -> 2
             end => 1
           } == %{1 => 1}

    assert %{
             try do
               1
             else
               a -> a
             end => 1
           } == %{1 => 1}
  end

  test "matching with map as a key" do
    assert %{%{1 => 2} => x} = %{%{1 => 2} => 3}
    assert x == 3
  end

  test "is_map/1" do
    assert is_map(Map.new())
    refute is_map(Enum.to_list(%{}))
  end

  test "map_size/1" do
    assert map_size(%{}) == 0
    assert map_size(@sample) == 2
  end

  test "new/1" do
    assert Map.new(%{a: 1, b: 2}) == %{a: 1, b: 2}
    assert Map.new(MapSet.new(a: 1, b: 2, a: 3)) == %{b: 2, a: 3}
  end

  test "new/2" do
    transformer = fn {key, value} -> {key, value * 2} end
    assert Map.new(%{a: 1, b: 2}, transformer) == %{a: 2, b: 4}
    assert Map.new(MapSet.new(a: 1, b: 2, a: 3), transformer) == %{b: 4, a: 6}
  end

  test "take/2" do
    assert Map.take(%{a: 1, b: 2, c: 3}, [:b, :c]) == %{b: 2, c: 3}
    assert Map.take(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == %{b: 2, c: 3}
    assert Map.take(%{a: 1, b: 2, c: 3}, []) == %{}
    assert_raise BadMapError, fn -> Map.take(:foo, []) end
  end

  test "drop/2" do
    assert Map.drop(%{a: 1, b: 2, c: 3}, [:b, :c]) == %{a: 1}
    assert Map.drop(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == %{a: 1}
    assert_raise BadMapError, fn -> Map.drop(:foo, []) end
  end

  test "split/2" do
    assert Map.split(%{a: 1, b: 2, c: 3}, [:b, :c]) == {%{b: 2, c: 3}, %{a: 1}}
    assert Map.split(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == {%{b: 2, c: 3}, %{a: 1}}
    assert_raise BadMapError, fn -> Map.split(:foo, []) end
  end

  test "get_and_update/3" do
    message = "the given function must return a two-element tuple or :pop, got: 1"

    assert_raise RuntimeError, message, fn ->
      Map.get_and_update(%{a: 1}, :a, fn value -> value end)
    end
  end

  test "get_and_update!/3" do
    message = "the given function must return a two-element tuple or :pop, got: 1"

    assert_raise RuntimeError, message, fn ->
      Map.get_and_update!(%{a: 1}, :a, fn value -> value end)
    end
  end

  test "maps with optional comma" do
    assert Code.eval_string("%{a: :b,}") == {%{a: :b}, []}
    assert Code.eval_string("%{1 => 2,}") == {%{1 => 2}, []}
    assert Code.eval_string("%{1 => 2, a: :b,}") == {%{1 => 2, a: :b}, []}
  end

  test "update maps" do
    assert %{@sample | a: 3} == %{a: 3, b: 2}

    assert_raise KeyError, fn ->
      %{@sample | c: 3}
    end
  end

  test "map dot access" do
    assert @sample.a == 1

    assert_raise KeyError, fn ->
      @sample.c
    end
  end

  test "put/3 optimized by the compiler" do
    map = %{a: 1, b: 2}

    assert Map.put(map, :a, 2) == %{a: 2, b: 2}
    assert Map.put(map, :c, 3) == %{a: 1, b: 2, c: 3}

    assert Map.put(%{map | a: 2}, :a, 3) == %{a: 3, b: 2}
    assert Map.put(%{map | a: 2}, :b, 3) == %{a: 2, b: 3}

    assert Map.put(map, :a, 2) |> Map.put(:a, 3) == %{a: 3, b: 2}
    assert Map.put(map, :a, 2) |> Map.put(:c, 3) == %{a: 2, b: 2, c: 3}
    assert Map.put(map, :c, 3) |> Map.put(:a, 2) == %{a: 2, b: 2, c: 3}
    assert Map.put(map, :c, 3) |> Map.put(:c, 4) == %{a: 1, b: 2, c: 4}
  end

  test "merge/2 with map literals optimized by the compiler" do
    map = %{a: 1, b: 2}

    assert Map.merge(map, %{a: 2}) == %{a: 2, b: 2}
    assert Map.merge(map, %{c: 3}) == %{a: 1, b: 2, c: 3}
    assert Map.merge(%{a: 2}, map) == %{a: 1, b: 2}
    assert Map.merge(%{c: 3}, map) == %{a: 1, b: 2, c: 3}

    assert Map.merge(%{map | a: 2}, %{a: 3}) == %{a: 3, b: 2}
    assert Map.merge(%{map | a: 2}, %{b: 3}) == %{a: 2, b: 3}
    assert Map.merge(%{a: 2}, %{map | a: 3}) == %{a: 3, b: 2}
    assert Map.merge(%{a: 2}, %{map | b: 3}) == %{a: 1, b: 3}

    assert Map.merge(map, %{a: 2}) |> Map.merge(%{a: 3, c: 3}) == %{a: 3, b: 2, c: 3}
    assert Map.merge(map, %{c: 3}) |> Map.merge(%{c: 4}) == %{a: 1, b: 2, c: 4}
    assert Map.merge(map, %{a: 3, c: 3}) |> Map.merge(%{a: 2}) == %{a: 2, b: 2, c: 3}
  end

  test "merge/3" do
    # When first map is bigger
    assert Map.merge(%{a: 1, b: 2, c: 3}, %{c: 4, d: 5}, fn :c, 3, 4 -> :x end) ==
             %{a: 1, b: 2, c: :x, d: 5}

    # When second map is bigger
    assert Map.merge(%{b: 2, c: 3}, %{a: 1, c: 4, d: 5}, fn :c, 3, 4 -> :x end) ==
             %{a: 1, b: 2, c: :x, d: 5}
  end

  test "implements (almost) all functions in Keyword" do
    assert Keyword.__info__(:functions) -- Map.__info__(:functions) ==
             [delete: 3, delete_first: 2, get_values: 2, keyword?: 1, pop_first: 2, pop_first: 3]
  end

  test "variable keys" do
    x = :key
    %{^x => :value} = %{x => :value}
    assert %{x => :value} == %{key: :value}
    assert (fn %{^x => :value} -> true end).(%{key: :value})

    map = %{x => :value}
    assert %{map | x => :new_value} == %{x => :new_value}
  end

  defmodule ExternalUser do
    def __struct__ do
      %{__struct__: ThisDoesNotLeak, name: "john", age: 27}
    end

    def __struct__(kv) do
      Enum.reduce(kv, __struct__(), fn {k, v}, acc -> :maps.update(k, v, acc) end)
    end
  end

  test "structs" do
    assert %ExternalUser{} == %{__struct__: ExternalUser, name: "john", age: 27}

    assert %ExternalUser{name: "meg"} == %{__struct__: ExternalUser, name: "meg", age: 27}

    user = %ExternalUser{}
    assert %ExternalUser{user | name: "meg"} == %{__struct__: ExternalUser, name: "meg", age: 27}

    %ExternalUser{name: name} = %ExternalUser{}
    assert name == "john"

    map = %{}

    assert_raise BadStructError, "expected a struct named MapTest.ExternalUser, got: %{}", fn ->
      %ExternalUser{map | name: "meg"}
    end
  end

  describe "structs with variable name" do
    test "extracts the struct module" do
      %module{name: "john"} = %ExternalUser{name: "john", age: 27}
      assert module == ExternalUser
    end

    test "returns the struct on match" do
      assert Code.eval_string("%struct{} = %ExternalUser{}", [], __ENV__) ==
               {%ExternalUser{}, [struct: ExternalUser]}
    end

    test "supports the pin operator" do
      module = ExternalUser
      user = %ExternalUser{name: "john", age: 27}
      %^module{name: "john"} = user
    end

    test "is supported in case" do
      user = %ExternalUser{name: "john", age: 27}

      case user do
        %module{} = %{age: 27} -> assert module == ExternalUser
      end
    end

    defp foo(), do: "foo"
    defp destruct1(%module{}), do: module
    defp destruct2(%_{}), do: :ok

    test "does not match" do
      invalid_struct = %{__struct__: foo()}

      assert_raise CaseClauseError, fn ->
        case invalid_struct do
          %module{} -> module
        end
      end

      assert_raise CaseClauseError, fn ->
        case invalid_struct do
          %_{} -> :ok
        end
      end

      assert_raise CaseClauseError, fn ->
        foo = foo()

        case invalid_struct do
          %^foo{} -> :ok
        end
      end

      assert_raise FunctionClauseError, fn ->
        destruct1(invalid_struct)
      end

      assert_raise FunctionClauseError, fn ->
        destruct2(invalid_struct)
      end

      assert_raise MatchError, fn ->
        %module{} = invalid_struct
        _ = module
      end

      assert_raise MatchError, fn ->
        %_{} = invalid_struct
      end

      assert_raise MatchError, fn ->
        foo = foo()
        %^foo{} = invalid_struct
      end
    end
  end

  test "structs when using dynamic modules" do
    defmodule Module.concat(MapTest, DynamicUser) do
      defstruct [:name, :age]

      def sample do
        %__MODULE__{}
      end
    end
  end

  test "structs when quoted" do
    quoted =
      quote do
        %User{foo: 1}
      end

    assert {:%, [], [aliases, {:%{}, [], [{:foo, 1}]}]} = quoted
    assert aliases == {:__aliases__, [alias: false], [:User]}

    quoted =
      quote do
        %unquote(User){foo: 1}
      end

    assert quoted == {:%, [], [User, {:%{}, [], [{:foo, 1}]}]}
  end

  test "defstruct can only be used once in a module" do
    message =
      "defstruct has already been called for TestMod, " <>
        "defstruct can only be called once per module"

    assert_raise ArgumentError, message, fn ->
      Code.eval_string("""
      defmodule TestMod do
        defstruct [:foo]
        defstruct [:foo]
      end
      """)
    end
  end

  test "defstruct allows keys to be enforced" do
    message = "the following keys must also be given when building struct TestMod: [:foo]"

    assert_raise ArgumentError, message, fn ->
      Code.eval_string("""
      defmodule TestMod do
        @enforce_keys :foo
        defstruct [:foo]
        def foo do
          %TestMod{}
        end
      end
      """)
    end
  end

  test "defstruct raises on invalid enforce_keys" do
    message = "keys given to @enforce_keys must be atoms, got: \"foo\""

    assert_raise ArgumentError, message, fn ->
      Code.eval_string("""
      defmodule TestMod do
        @enforce_keys "foo"
        defstruct [:foo]
      end
      """)
    end
  end

  test "struct always expands context module" do
    Code.compiler_options(ignore_module_conflict: true)

    defmodule LocalPoint do
      defstruct x: 0
      def new, do: %LocalPoint{}
    end

    assert LocalPoint.new() == %{__struct__: LocalPoint, x: 0}

    defmodule LocalPoint do
      defstruct x: 0, y: 0
      def new, do: %LocalPoint{}
    end

    assert LocalPoint.new() == %{__struct__: LocalPoint, x: 0, y: 0}
  after
    Code.compiler_options(ignore_module_conflict: false)
  end

  defmodule LocalUser do
    defmodule NestedUser do
      defstruct []
    end

    defstruct name: "john", nested: struct(NestedUser), context: %{}

    def new do
      %LocalUser{}
    end

    defmodule Context do
      def new do
        %LocalUser{}
      end
    end
  end

  test "local and nested structs" do
    assert LocalUser.new() == %LocalUser{name: "john", nested: %LocalUser.NestedUser{}}
    assert LocalUser.Context.new() == %LocalUser{name: "john", nested: %LocalUser.NestedUser{}}
  end

  defmodule :elixir_struct_from_erlang_module do
    defstruct [:hello]
    def world(%:elixir_struct_from_erlang_module{} = struct), do: struct
  end

  test "struct from erlang module" do
    struct = %:elixir_struct_from_erlang_module{}
    assert :elixir_struct_from_erlang_module.world(struct) == struct
  end
end
