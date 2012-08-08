Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case, async: true

  test :list do
    assert [foo: :bar][:foo] == :bar
    assert [foo: [bar: :baz]][:foo][:bar] == :baz
  end

  test :function do
    function = fn x -> x == :foo end
    assert function[:foo] == true
    assert function[:bar] == false
  end

  test :atom do
    exception = assert_raise RuntimeError, fn ->
      foo = :foo
      foo[:atom]
    end
    assert exception.message == "The access protocol can only be invoked for atoms at compilation time, tried to invoke it for :foo"
  end
end

defmodule Access.RecordTest do
  use ExUnit.Case, async: true

  defrecord Config, integer: 0

  test :keywords do
    assert Config[] == { Config, 0 }
    assert Config[integer: 1] == { Config, 1 }
  end

  test :in_match_with_variable do
    assert get_var(Config.new) == 0
    assert get_var(Config.new(integer: 1)) == 1
  end

  test :in_match_with_record_match do
    assert is_config(Config.new) == true
    assert is_config({ Access.AtomTest, 1 }) == false
    assert is_config({ Config, 1, 2 }) == false
  end

  test :in_match_with_field_match do
    assert is_zero(Config.new) == true
    assert is_zero(Config.new(integer: 1)) == false
  end

  test :match do
    assert Config[integer: 1] = Config.new(integer: 1)
    refute Config[integer: 1] = Config.new(integer: 0)
  end

  defp get_var(Config[integer: integer]) do
    integer
  end

  defp is_zero(Config[integer: 0]), do: true
  defp is_zero(Config[integer: _]),  do: false

  defp is_config(Config[]), do: true
  defp is_config(_), do: false
end