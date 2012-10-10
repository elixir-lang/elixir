Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.QuoteTest.Hygiene do
  defmacro no_interference do
    quote do: a = 1
  end

  defmacro no_hygiene do
    quote [hygiene: false] do
      a = 1
    end
  end

  defmacro write_interference do
    quote do: var!(a) = 1
  end

  defmacro read_interference do
    quote do: 10 = var!(a)
  end
end

defmodule Kernel.QuoteTest do
  use ExUnit.Case, async: true
  import __MODULE__.Hygiene

  test :no_interference do
    a = 10
    no_interference
    assert a == 10
  end

  test :no_hygiene do
    no_hygiene
    assert a == 1
  end

  test :write_interference do
    write_interference
    assert a == 1
  end

  test :read_interference do
    a = 10
    read_interference
  end

  test :list do
    assert quote(do: [1,2,3]) == [1,2,3]
  end

  test :tuple do
    assert quote(do: { :a, 1 }) == {:a,1}
  end

  test :keep_line do
    ## DO NOT MOVE THIS LINE
    assert quote(line: :keep, do: bar(1,2,3)) == { :bar, 58, [1,2,3] }
  end

  test :fixed_line do
    assert quote(line: 3, do: bar(1,2,3)) == { :bar, 3, [1,2,3] }
  end

  test :keep_location do
    ## DO NOT MOVE THIS LINE
    assert quote(location: :keep, do: bar(1,2,3)) == {
      :__scope__,
      67,
      [
        [file: __FILE__],
        { :bar, 67, [1,2,3] }
      ]
    }
  end

  test :unquote_call do
    assert quote(do: foo(bar)[:baz])
    assert quote(do: unquote(:bar)()) == quote(do: bar())
    assert quote(do: unquote(:bar)(1) do 2 + 3 end) == quote(do: bar(1) do 2 + 3 end)
    assert quote(do: foo.unquote(:bar)) == quote(do: foo.bar)
    assert quote(do: foo.unquote(:bar)(1)) == quote(do: foo.bar(1))
    assert quote(do: foo.unquote(:bar)(1) do 2 + 3 end) == quote(do: foo.bar(1) do 2 + 3 end)
  end

  test :splice_on_root do
    contents = [1, 2, 3]
    assert quote(do: unquote_splicing(contents)) == quote do: (1; 2; 3)
  end
end
