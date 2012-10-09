Code.require_file "../test_helper.exs", __FILE__

defmodule Macro.ExternalTest do
  defmacro external do
    17 = __CALLER__.line
    __FILE__ = __CALLER__.file
    [line: 17, file: __FILE__] = __CALLER__.location
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true

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
    assert Macro.expand(quote(do: Elixir), __ENV__) == Elixir
  end

  test :expand_with_simple_alias do
    assert Macro.expand(quote(do: Foo), __ENV__) == Foo
  end

  test :expand_with_current_module_plus_alias do
    assert Macro.expand(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
  end

  test :expand_with_main_plus_alias do
    assert Macro.expand(quote(do: Elixir.Foo), __ENV__) == Foo
  end

  test :expand_with_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand(quote(do: Bar.Baz), __ENV__) == Foo.Baz
  end

  test :expand_with_main_plus_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
  end

  test :expand_with_op do
    assert Macro.expand(quote(do: Foo.bar.Baz), __ENV__) == (quote do
      Foo.bar.Baz
    end)
  end

  test :expand_with_erlang do
    assert Macro.expand(quote(do: :foo), __ENV__) == :foo
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
    assert Macro.expand(quote(do: Kernel.||(1, false)), __ENV__) == (quote do
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

  @foo 1
  @bar Macro.expand(quote(do: @foo), __ENV__)

  test :expand_with_module_at do
    assert @bar == 1
  end

  ## to_binary

  test :var_to_binary do
    assert Macro.to_binary(quote do: foo) == "foo"
  end

  test :local_call_to_binary do
    assert Macro.to_binary(quote do: foo(1, 2, 3)) == "foo(1, 2, 3)"
    assert Macro.to_binary(quote do: foo([1, 2, 3])) == "foo([1, 2, 3])"
  end

  test :remote_call_to_binary do
    assert Macro.to_binary(quote do: foo.bar(1, 2, 3)) == "foo.bar(1, 2, 3)"
    assert Macro.to_binary(quote do: foo.bar([1, 2, 3])) == "foo.bar([1, 2, 3])"
  end

  test :remote_and_fun_call_to_binary do
    assert Macro.to_binary(quote do: foo.bar.(1, 2, 3)) == "foo.bar().(1, 2, 3)"
    assert Macro.to_binary(quote do: foo.bar.([1, 2, 3])) == "foo.bar().([1, 2, 3])"
  end

  test :aliases_call_to_binary do
    assert Macro.to_binary(quote do: Foo.Bar.baz(1, 2, 3)) == "Foo.Bar.baz(1, 2, 3)"
    assert Macro.to_binary(quote do: Foo.Bar.baz([1, 2, 3])) == "Foo.Bar.baz([1, 2, 3])"
  end

  test :blocks_to_binary do
    assert Macro.to_binary(quote do: (1; 2; (:foo; :bar); 3)) <> "\n" == """
    (
      1
      2
      (
        :foo
        :bar
      )
      3
    )
    """
  end

  test :if_else_to_binary do
    assert Macro.to_binary(quote do: (if foo, do: bar, else: baz)) <> "\n" == """
    if(foo) do
      bar
    else
      baz
    end
    """
  end

  test :case_to_binary do
    assert Macro.to_binary(quote do: (case foo do true -> 0; false -> (1; 2) end)) <> "\n" == """
    case(foo) do
      true ->
        0
      false ->
        1
        2
    end
    """
  end

  test :fn_to_binary do
    assert Macro.to_binary(quote do: (fn(x) -> x + 1 end)) <> "\n" == """
    fn x ->
      x + 1
    end
    """
  end

  test :partial_to_binary do
    assert Macro.to_binary(quote do: identity(&1)) == "identity(&1)"
  end

  test :nested_to_binary do
    assert Macro.to_binary(quote do: (defmodule Foo do def foo do 1 + 1 end end)) <> "\n" == """
    defmodule(Foo) do
      def(foo) do
        1 + 1
      end
    end
    """
  end

  test :op_precedence_to_binary do
    assert Macro.to_binary(quote do: (1 + 2) * (3 - 4)) == "(1 + 2) * (3 - 4)"
    assert Macro.to_binary(quote do: ((1 + 2) * 3) - 4) == "((1 + 2) * 3) - 4"
  end

  test :containers_to_binary do
    assert Macro.to_binary(quote do: { 1, 2, 3 })   == "{1, 2, 3}"
    assert Macro.to_binary(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
    assert Macro.to_binary(quote do: << 1, 2, 3 >>) == "<<1, 2, 3>>"
  end

  test :binary_ops_to_binary do
    assert Macro.to_binary(quote do: 1 + 2)   == "1 + 2"
    assert Macro.to_binary(quote do: [ 1, 2 | 3 ]) == "[1, 2 | 3]"
    assert Macro.to_binary(quote do: [h|t] = [1,2,3]) == "[h | t] = [1, 2, 3]"
  end

  test :unary_ops_to_binary do
    assert Macro.to_binary(quote do: -1) == "-1"
    assert Macro.to_binary(quote do: @foo(bar)) == "@foo(bar)"
  end

  ## is_term

  test :is_term_on_term do
   assert Macro.term?((quote do: 1)) == true
   assert Macro.term?((quote do: 1.1)) == true
   assert Macro.term?((quote do: -1)) == true
   assert Macro.term?((quote do: +1)) == true
   assert Macro.term?((quote do: [])) == true
   assert Macro.term?((quote do: [1,2,3])) == true
   assert Macro.term?((quote do: "")) == true
   assert Macro.term?((quote do: {})) == true
   assert Macro.term?((quote do: {1,2})) == true
   assert Macro.term?((quote do: {1,2,3})) == true
   assert Macro.term?((quote do: {1,2,3,4})) == true
   assert Macro.term?((quote do: Alias)) == true   
  end

  test :is_term_on_expr do
   assert Macro.term?((quote do: 1+1)) == false
   assert Macro.term?((quote do: [1+1])) == false
   assert Macro.term?((quote do: {1+1})) == false
  end
end