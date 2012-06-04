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

  test :expand_with_raw_atom do
    assert Macro.expand(quote(do: :foo), __ENV__) == :foo
  end

  test :expand_with_current_module do
    assert Macro.expand(quote(do: __MODULE__), __ENV__) == __MODULE__
  end

  test :expand_with_main do
    assert Macro.expand(quote(do: __MAIN__), __ENV__) == __MAIN__
  end

  test :expand_with_simple_alias do
    assert Macro.expand(quote(do: Foo), __ENV__) == Foo
  end

  test :expand_with_current_module_plus_alias do
    assert Macro.expand(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
  end

  test :expand_with_main_plus_alias do
    assert Macro.expand(quote(do: __MAIN__.Foo), __ENV__) == Foo
  end

  test :expand_with_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand(quote(do: Bar.Baz), __ENV__) == Foo.Baz
  end

  test :expand_with_main_plus_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand(quote(do: __MAIN__.Bar.Baz), __ENV__) == __MAIN__.Bar.Baz
  end

  test :expand_with_erlang do
    assert Macro.expand(quote(do: Erlang.foo), __ENV__) == :foo
  end

  test :expand_with_imported_macro do
    assert Macro.expand(quote(do: 1 || false), __ENV__) == (quote do
      case 1 do
        oror in [false, nil] -> false
        oror -> oror
      end
    end)
  end

  test :expand_with_require_macro do
    assert Macro.expand(quote(do: Elixir.Builtin.||(1, false)), __ENV__) == (quote do
      case 1 do
        oror in [false, nil] -> false
        oror -> oror
      end
    end)
  end

  test :expand_with_not_expandable_expression do
    expr = quote(do: other(1,2,3))
    assert Macro.expand(expr, __ENV__) == expr
  end
end