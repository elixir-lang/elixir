ExUnit.start [seed: 0]

defmodule Difference do
  @moduledoc """
  This module contains failing tests to see
  difference highlighting in action.
  """
  use ExUnit.Case

  defmodule User do
    defstruct [:age]
  end

  test "1. integers" do
    assert 491512235 == 490512035
  end

  test "2. floats" do
    assert 42.0 == 43.0
  end

  test "3. strings" do
    string1 = "fox hops over the dog"
    string2 = "fox jumps over the lazy cat"
    assert string1 == string2
  end

  test "4. lists" do
    list1 = [{"Content-type", "text/plain"}, {"etag", "One"}]
    list2 = [{"content-type", "text/html"}, {"Etag", "Two"}]
    assert list1 == list2
  end

  test "5. tuples" do
    tuple1 = {:yes, 'ject', []}
    tuple2 = {:yes, 'lter', []}
    assert tuple1 == tuple2
  end

  test "6. maps; mixed diff" do
    map1 = Enum.into(1..40, %{}, &{&1, &1}) |> Map.delete(33)
    map2 = Enum.reduce(5..10, map1, &Map.delete(&2, &1)) |> Map.put(33, 33) |> Map.put(23, 32)
    assert map1 == map2
  end

  test "7. maps; missing pair and match" do
    map1 = %{baz: 12}
    map2 = %{foo: 12, bar: 12, baz: 12}
    assert map1 == map2
  end

  test "8. maps; surplus pair and match" do
    map1 = %{foo: 12, bar: 12, baz: 12}
    map2 = %{baz: 12}
    assert map1 == map2
  end

  test "9. maps; missing pair" do
    assert %{} == %{baz: 12}
  end

  test "10. maps; surplus pair" do
    assert %{baz: 12} == %{}
  end

  test "11. structs" do
    assert %User{age: 16} == %User{age: 21}
  end
end
