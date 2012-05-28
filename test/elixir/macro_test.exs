Code.require_file "../test_helper", __FILE__

defmodule Macro.ExternalTest do
  defmacro external do
    17 = __CALLER__.line
    __FILE__ = __CALLER__.file
    [line: 17, file: __FILE__] = __CALLER__.location
  end
end

defmodule MacroTest do
  use ExUnit.Case

  # Changing the lines above will make compilation
  # fail since we are assertnig on the caller lines
  require Macro.ExternalTest
  Macro.ExternalTest.external()

  ## Escape

  test :escape_handle_tuples_with_size_different_than_two do
    assert { :{}, 0, [:a] } == Macro.escape({ :a })
    assert { :{}, 0, [:a, :b, :c] } == Macro.escape({ :a, :b, :c })
  end

  test :escape_simply_returns_tuples_with_size_equal_to_two do
    assert { :a, :b } == Macro.escape({ :a, :b })
  end

  test :escape_simply_returns_any_other_structure do
    assert [1,2,3] == Macro.escape([1,2,3])
  end

  test :escape_works_recursively do
    assert [1,{:{}, 0, [:a,:b,:c]},3] == Macro.escape([1, { :a, :b, :c },3])
  end

  ## Expand aliases

  test :expand_aliases_with_raw_atom do
    assert Macro.expand_aliases(quote(do: :foo), __ENV__) == :foo
  end

  test :expand_aliases_with_current_module do
    assert Macro.expand_aliases(quote(do: __MODULE__), __ENV__) == __MODULE__
  end

  test :expand_aliases_with_main do
    assert Macro.expand_aliases(quote(do: __MAIN__), __ENV__) == __MAIN__
  end

  test :expand_aliases_with_simple_alias do
    assert Macro.expand_aliases(quote(do: Foo), __ENV__) == Foo
  end

  test :expand_aliases_with_current_module_plus_alias do
    assert Macro.expand_aliases(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
  end

  test :expand_aliases_with_main_plus_alias do
    assert Macro.expand_aliases(quote(do: __MAIN__.Foo), __ENV__) == Foo
  end

  test :expand_aliases_with_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand_aliases(quote(do: Bar.Baz), __ENV__) == Foo.Baz
  end

  test :expand_aliases_with_main_plus_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand_aliases(quote(do: __MAIN__.Bar.Baz), __ENV__) == __MAIN__.Bar.Baz
  end

  test :expand_aliases_with_erlang do
    assert Macro.expand_aliases(quote(do: Erlang.foo), __ENV__) == :foo
  end

  test :expand_aliases_with_nested_erlang do
    assert Macro.expand_aliases(quote(do: Erlang.foo.bar), __ENV__) == nil
  end
end