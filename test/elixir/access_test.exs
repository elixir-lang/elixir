Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case

  defrecord Config, other: { :a, :b, :c }

  test :literal do
    assert { :a, :b, :c }[1] == :a
    assert Config.new.other[1] == :a
  end

  test :positive_integer do
    tuple = { :a, :b, :c }
    assert tuple[0] == nil
    assert tuple[1] == :a
    assert tuple[2] == :b
    assert tuple[3] == :c
    assert tuple[4] == nil
  end

  test :negative_integer do
    tuple = { :a, :b, :c }
    assert tuple[-4] == nil
    assert tuple[-3] == :a
    assert tuple[-2] == :b
    assert tuple[-1] == :c
  end

  test :access do
    assert Tuple.access({ :a, :b, :c }, -1) == :c
  end
end

defmodule Access.ListTest do
  use ExUnit.Case

  test :literal do
    assert 'abc'[%r(a)] == 'a'
  end

  test :regex do
    list = 'abc'
    assert list[%r(b)] == 'b'
    assert list[%r(d)] == nil
  end

  test :atom do
    list = [foo: "bar"]
    assert list[:foo] == "bar"
    assert list[:bar] == nil
  end

  test :access do
    assert List.access([foo: :bar ], :foo) == :bar
  end
end

defmodule Access.BinaryTest do
  use ExUnit.Case

  test :literal do
    assert "abc"[%r(a)] == "a"
  end

  test :regex do
    binary = "abc"
    assert binary[%r(b)] == "b"
    assert binary[%r(d)] == nil
  end

  test :access do
    assert Binary.access("abc", %r"a") == "a"
  end
end

defmodule Access.AtomTest do
  use ExUnit.Case

  defrecord Config, integer: 0

  test :keywords do
    assert Config[] == { Config, 0 }
    assert Config[integer: 1] == { Config, 1 }
  end

  test :in_guard_with_variable do
    assert get_var(Config.new) == 0
    assert get_var(Config.new(integer: 1)) == 1
  end

  test :in_guard_with_record_match do
    assert is_config(Config.new) == true
    assert is_config({ Access.AtomTest, 1 }) == false
    assert is_config({ Config, 1, 2 }) == false
  end

  test :in_guard_with_field_match do
    assert is_zero(Config.new) == true
    assert is_zero(Config.new(integer: 1)) == false
  end

  test :match do
    assert_match Config[integer: 1], Config.new(integer: 1)
    refute_match Config[integer: 1], Config.new(integer: 0)
  end

  defp get_var(Config[integer: integer]) do
    integer
  end

  defp is_zero(Config[integer: 0]), do: true
  defp is_zero(Config[integer: _]),  do: false

  defp is_config(Config[]), do: true
  defp is_config(_), do: false
end

defmodule Access.FunctionTest do
  use ExUnit.Case

  test :any do
    function = fn(x) -> x == :foo end
    assert function[:foo] == true
    assert function[:bar] == false
  end
end
