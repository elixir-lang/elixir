Code.require_file "test_helper.exs", __DIR__

defmodule MapTest do
  use ExUnit.Case, async: true

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
end
