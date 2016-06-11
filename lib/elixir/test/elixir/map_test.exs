Code.require_file "test_helper.exs", __DIR__

defmodule MapTest do
  use ExUnit.Case, async: true

  doctest Map

  @sample %{a: 1, b: 2}

  test "maps in attributes" do
    assert @sample == %{a: 1, b: 2}
  end

  test "maps when quoted" do
    assert (quote do
      %{foo: 1}
    end) == {:%{}, [], [{:foo, 1}]}
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
    assert %{(for x <- 1..3, do: x) => 1} == %{[1, 2, 3] => 1}
    assert %{(with x = 1, do: x) => 1} == %{1 => 1}
    assert %{(with {:ok, x} <- {:ok, 1}, do: x) => 1} == %{1 => 1}
    assert %{(try do raise "error" rescue _ -> 1 end) => 1} == %{1 => 1}
    assert %{(try do throw 1 catch x -> x end) => 1} == %{1 => 1}
    assert %{(try do a = 1; a rescue _ -> 2 end) => 1} == %{1 => 1}
    assert %{(try do 1 else a -> a end) => 1} == %{1 => 1}
  end

  test "is_map/1" do
    assert is_map(Map.new)
    refute is_map(Enum.to_list(%{}))
  end

  test "map_size/1" do
    assert map_size(%{}) == 0
    assert map_size(@sample) == 2
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

  test "maps with optional comma" do
    assert %{a: :b,} == %{a: :b}
    assert %{1 => 2,} == %{1 => 2}
    assert %{1 => 2, a: :b,} == %{1 => 2, a: :b}
  end

  test "maps with duplicate keys" do
    assert %{a: :b, a: :c} == %{a: :c}
    assert %{1 => 2, 1 => 3} == %{1 => 3}
    assert %{:a => :b, a: :c} == %{a: :c}
  end

  test "update maps" do
    assert %{@sample | a: 3} == %{a: 3, b: 2}

    assert_raise KeyError, fn ->
      %{@sample | c: 3}
    end
  end

  test "map access" do
    assert @sample.a == 1

    assert_raise KeyError, fn ->
      @sample.c
    end
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
      Enum.reduce kv, __struct__(), fn {k, v}, acc -> :maps.update(k, v, acc) end
    end
  end

  test "structs" do
    assert %ExternalUser{} ==
           %{__struct__: ExternalUser, name: "john", age: 27}

    assert %ExternalUser{name: "meg"} ==
           %{__struct__: ExternalUser, name: "meg", age: 27}

    assert %ExternalUser{__struct__: ThisWillBeIgnored} ==
           %{__struct__: ExternalUser, name: "john", age: 27}

    user = %ExternalUser{}
    assert %ExternalUser{user | name: "meg"} ==
           %{__struct__: ExternalUser, name: "meg", age: 27}

    %ExternalUser{name: name} = %ExternalUser{}
    assert name == "john"

    map = %{}
    assert_raise BadStructError, "expected a struct named MapTest.ExternalUser, got: %{}", fn ->
      %ExternalUser{map | name: "meg"}
    end
  end

  test "structs when matching" do
    %struct{name: "john"} = %ExternalUser{name: "john", age: 27}
    assert struct == ExternalUser
    user = %ExternalUser{name: "john", age: 27}
    %^struct{name: "john"} = user
  end

  test "structs when quoted" do
    assert (quote do
      %User{foo: 1}
    end) == {:%, [], [
      {:__aliases__, [alias: false], [:User]},
      {:%{}, [], [{:foo, 1}]}
    ]}

    assert (quote do
      %unquote(User){foo: 1}
    end) == {:%, [], [User, {:%{}, [], [{:foo, 1}]}]}
  end

  test "defstruct can only be used once in a module" do
    message = "defstruct has already been called for TestMod, " <>
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

  test "defstruct allow keys to be enforced" do
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
    assert LocalUser.new == %LocalUser{name: "john", nested: %LocalUser.NestedUser{}}
    assert LocalUser.Context.new == %LocalUser{name: "john", nested: %LocalUser.NestedUser{}}
  end
end
