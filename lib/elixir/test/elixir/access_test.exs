Code.require_file("test_helper.exs", __DIR__)

defmodule AccessTest do
  use ExUnit.Case, async: true

  doctest Access

  # Test nil at compilation time does not fail
  # and that @config[:foo] has proper precedence.
  @config nil
  nil = @config[:foo]

  @config [foo: :bar]
  :bar = @config[:foo]

  @mod :lists
  [1, 2, 3] = @mod.flatten([1, [2], 3])

  @mod -13
  -13 = @mod

  test "for nil" do
    assert nil[:foo] == nil
    assert Access.fetch(nil, :foo) == :error
    assert Access.get(nil, :foo) == nil

    assert_raise ArgumentError, "could not put/update key :foo on a nil value", fn ->
      Access.get_and_update(nil, :foo, fn nil -> {:ok, :bar} end)
    end
  end

  test "for keywords" do
    assert [foo: :bar][:foo] == :bar
    assert [foo: [bar: :baz]][:foo][:bar] == :baz
    assert [foo: [bar: :baz]][:fuu][:bar] == nil

    assert Access.fetch([foo: :bar], :foo) == {:ok, :bar}
    assert Access.fetch([foo: :bar], :bar) == :error

    msg = ~r/the Access calls for keywords expect the key to be an atom/

    assert_raise ArgumentError, msg, fn ->
      Access.fetch([], "foo")
    end

    assert Access.get([foo: :bar], :foo) == :bar
    assert Access.get_and_update([], :foo, fn nil -> {:ok, :baz} end) == {:ok, [foo: :baz]}

    assert Access.get_and_update([foo: :bar], :foo, fn :bar -> {:ok, :baz} end) ==
             {:ok, [foo: :baz]}

    assert Access.pop([foo: :bar], :foo) == {:bar, []}
    assert Access.pop([], :foo) == {nil, []}
  end

  test "for maps" do
    assert %{foo: :bar}[:foo] == :bar
    assert %{1 => 1}[1] == 1
    assert %{1.0 => 1.0}[1.0] == 1.0
    assert %{1 => 1}[1.0] == nil

    assert Access.fetch(%{foo: :bar}, :foo) == {:ok, :bar}
    assert Access.fetch(%{foo: :bar}, :bar) == :error

    assert Access.get(%{foo: :bar}, :foo) == :bar
    assert Access.get_and_update(%{}, :foo, fn nil -> {:ok, :baz} end) == {:ok, %{foo: :baz}}

    assert Access.get_and_update(%{foo: :bar}, :foo, fn :bar -> {:ok, :baz} end) ==
             {:ok, %{foo: :baz}}

    assert Access.pop(%{foo: :bar}, :foo) == {:bar, %{}}
    assert Access.pop(%{}, :foo) == {nil, %{}}
  end

  test "for struct" do
    defmodule Sample do
      defstruct [:name]
    end

    message =
      "function AccessTest.Sample.fetch/2 is undefined (AccessTest.Sample does not implement the Access behaviour)"

    assert_raise UndefinedFunctionError, message, fn ->
      Access.fetch(struct(Sample, []), :name)
    end

    message =
      "function AccessTest.Sample.get_and_update/3 is undefined (AccessTest.Sample does not implement the Access behaviour)"

    assert_raise UndefinedFunctionError, message, fn ->
      Access.get_and_update(struct(Sample, []), :name, fn nil -> {:ok, :baz} end)
    end

    message =
      "function AccessTest.Sample.pop/2 is undefined (AccessTest.Sample does not implement the Access behaviour)"

    assert_raise UndefinedFunctionError, message, fn ->
      Access.pop(struct(Sample, []), :name)
    end
  end

  describe "fetch!/2" do
    assert Access.fetch!(%{foo: :bar}, :foo) == :bar

    assert_raise ArgumentError,
                 ~r/the Access calls for keywords expect the key to be an atom/,
                 fn ->
                   Access.fetch!([], "foo")
                 end

    assert_raise ArgumentError,
                 ~r/the Access call for: nil was not able to find: \"foo\"/,
                 fn ->
                   Access.fetch!(nil, "foo")
                 end
  end

  describe "filter/1" do
    @test_list [1, 2, 3, 4, 5, 6]

    test "filters in get_in" do
      assert get_in(@test_list, [Access.filter(&(&1 > 3))]) == [4, 5, 6]
    end

    test "retains order in get_and_update_in/1" do
      assert get_and_update_in(@test_list, [Access.filter(&(&1 == 3 || &1 == 2))], &{&1 * 2, &1}) ==
               {[4, 6], [1, 2, 3, 4, 5, 6]}
    end

    test "retains order in pop_in/1" do
      assert pop_in(@test_list, [Access.filter(&(&1 == 3 || &1 == 2))]) == {[2, 3], [1, 4, 5, 6]}
    end

    test "chains with other access functions" do
      mixed_map_and_list = %{foo: Enum.map(@test_list, &%{value: &1})}

      assert get_in(mixed_map_and_list, [:foo, Access.filter(&(&1.value <= 3)), :value]) ==
               [1, 2, 3]
    end
  end

  describe "at/1" do
    @test_list [1, 2, 3, 4, 5, 6]

    test "returns element from the end if index is negative" do
      assert get_in(@test_list, [Access.at(-2)]) == 5
    end

    test "returns nil if index is out of bounds counting from the end" do
      assert get_in(@test_list, [Access.at(-10)]) == nil
    end

    test "updates the element counting from the end if index is negative" do
      assert get_and_update_in(@test_list, [Access.at(-2)], fn prev ->
               {prev, :foo}
             end) == {5, [1, 2, 3, 4, :foo, 6]}
    end

    test "returns nil and does not update if index is out of bounds" do
      assert get_and_update_in(@test_list, [Access.at(-10)], fn prev ->
               {prev, :foo}
             end) == {nil, [1, 2, 3, 4, 5, 6]}
    end
  end
end
