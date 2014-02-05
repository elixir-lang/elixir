Code.require_file "test_helper.exs", __DIR__

defmodule MapTest do
  use ExUnit.Case, async: true

  defp empty_map do
    %{}
  end

  defp two_items_map do
    %{ a: 1, b: 2 }
  end

  test "maps when quoted" do
    assert (quote do
      %{ foo: 1 }
    end) == { :%{}, [], [{ :foo, 1 }] }

    assert (quote do
      %
        { foo: 1 }
    end) == { :%{}, [], [{ :foo, 1 }] }
  end

  test "structs when quoted" do
    assert (quote do
      %User{ foo: 1 }
    end) == { :%, [], [
      { :__aliases__, [alias: false], [:User] },
      { :%{}, [], [{ :foo, 1 }] }
    ] }

    assert (quote do
      %
        User{ foo: 1 }
    end) == { :%, [], [
      { :__aliases__, [alias: false], [:User] },
      { :%{}, [], [{ :foo, 1 }] }
    ] }

    assert (quote do
      %unquote(User){ foo: 1 }
    end) == { :%, [], [User, { :%{}, [], [{ :foo, 1 }] }] }
  end

  test "maps keywords and atoms" do
    assert [%{}: :%] == [{ :%{}, :% }]
    assert [%: :%{}] == [{ :%, :%{} }]
  end

  test "maps with variables" do
    a = 0
    assert %{ a: a = 1, b: a } == %{ a: 1, b: 0 }
    assert a == 1
  end

  test "is_map/1" do
    assert is_map empty_map
    refute is_map(Enum.to_list(empty_map))
  end

  test "map_size/1" do
    assert map_size(empty_map) == 0
    assert map_size(two_items_map) == 2
  end

  test "maps with optional comma" do
    assert %{ a: :b, } == %{ a: :b }
    assert %{ 1 => 2, } == %{ 1 => 2 }
    assert %{ 1 => 2, a: :b, } == %{ 1 => 2, a: :b }
  end

  test "maps with duplicate keys" do
    assert %{ a: :b, a: :c } == %{ a: :c }
    assert %{ 1 => 2, 1 => 3 } == %{ 1 => 3 }
    assert %{ :a => :b, a: :c } == %{ a: :c }
  end
end
