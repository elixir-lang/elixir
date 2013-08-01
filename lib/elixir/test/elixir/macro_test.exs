Code.require_file "test_helper.exs", __DIR__

defmodule Macro.ExternalTest do
  defmacro external do
    18 = __CALLER__.line
    __FILE__ = __CALLER__.file
    18 = __CALLER__.location[:line]
    __FILE__ = __CALLER__.location[:file]
  end

  defmacro oror(left, right) do
    quote do: unquote(left) || unquote(right)
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true

  # Changing the lines above will make compilation
  # fail since we are assertnig on the caller lines
  import Macro.ExternalTest

  ## Escape

  test :escape_handle_tuples_with_size_different_than_two do
    assert { :{}, [], [:a] } == Macro.escape({ :a })
    assert { :{}, [], [:a, :b, :c] } == Macro.escape({ :a, :b, :c })
    assert { :{}, [], [:a, { :{}, [], [1,2,3] }, :c] } == Macro.escape({ :a, { 1, 2, 3 }, :c })
  end

  test :escape_simply_returns_tuples_with_size_equal_to_two do
    assert { :a, :b } == Macro.escape({ :a, :b })
  end

  test :escape_simply_returns_any_other_structure do
    assert [1, 2, 3] == Macro.escape([1, 2, 3])
  end

  test :escape_works_recursively do
    assert [1,{:{}, [], [:a,:b,:c]}, 3] == Macro.escape([1, { :a, :b, :c }, 3])
  end

  test :escape_with_unquote do
    contents = quote unquote: false, do: unquote(1)
    assert Macro.escape(contents, unquote: true) == 1

    contents = quote unquote: false, do: unquote(x)
    assert Macro.escape(contents, unquote: true) == { :x, [], MacroTest }
  end

  defp eval_escaped(contents) do
    { eval, [] } = Code.eval_quoted(Macro.escape(contents, unquote: true))
    eval
  end

  test :escape_with_remote_unquote do
    contents = quote unquote: false, do: Kernel.unquote(:is_atom)(:ok)
    assert eval_escaped(contents) == quote(do: Kernel.is_atom(:ok))
  end

  test :escape_with_nested_unquote do
    contents = quote do
      quote do: unquote(x)
    end
    assert eval_escaped(contents) == quote do: (quote do: unquote(x))
  end

  test :escape_with_alias_or_no_args_remote_unquote do
    contents = quote unquote: false, do: Kernel.unquote(:self)
    assert eval_escaped(contents) == quote(do: Kernel.self())

    contents = quote unquote: false, do: x.unquote(Foo)
    assert eval_escaped(contents) == quote(do: x.unquote(Foo))
  end

  test :escape_with_splicing do
    contents = quote unquote: false, do: [1, 2, 3, 4, 5]
    assert Macro.escape(contents, unquote: true) == [1, 2, 3, 4, 5]

    contents = quote unquote: false, do: [1, 2, unquote_splicing([3, 4, 5])]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]

    contents = quote unquote: false, do: [unquote_splicing([1, 2, 3]), 4, 5]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]

    contents = quote unquote: false, do: [unquote_splicing([1, 2, 3]), unquote_splicing([4, 5])]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]

    contents = quote unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]), 5]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]

    contents = quote unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4])|[5]]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]
  end

  ## Expand aliases

  test :expand_once do
    assert { :||, _, _ } = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
  end

  test :expand_once_with_raw_atom do
    assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
  end

  test :expand_once_with_current_module do
    assert Macro.expand_once(quote(do: __MODULE__), __ENV__) == __MODULE__
  end

  test :expand_once_with_main do
    assert Macro.expand_once(quote(do: Elixir), __ENV__) == Elixir
  end

  test :expand_once_with_simple_alias do
    assert Macro.expand_once(quote(do: Foo), __ENV__) == Foo
  end

  test :expand_once_with_current_module_plus_alias do
    assert Macro.expand_once(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
  end

  test :expand_once_with_main_plus_alias do
    assert Macro.expand_once(quote(do: Elixir.Foo), __ENV__) == Foo
  end

  test :expand_once_with_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand_once(quote(do: Bar.Baz), __ENV__) == Foo.Baz
  end

  test :expand_once_with_main_plus_custom_alias do
    alias Foo, as: Bar
    assert Macro.expand_once(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
  end

  test :expand_once_with_op do
    assert Macro.expand_once(quote(do: Foo.bar.Baz), __ENV__) == (quote do
      Foo.bar.Baz
    end)
  end

  test :expand_once_with_erlang do
    assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
  end

  defmacro local_macro do
    :local_macro
  end

  test :expand_once_local_macro do
    assert Macro.expand_once(quote(do: local_macro), __ENV__) == :local_macro
  end

  test :expand_once_with_imported_macro do
    assert Macro.expand_once(quote(do: 1 || false), __ENV__) == (quote context: Kernel do
      case 1 do
        var!(oror, false) in [false, nil] -> false
        var!(oror, false) -> var!(oror, false)
      end
    end)
  end

  test :expand_once_with_require_macro do
    assert Macro.expand_once(quote(do: Kernel.||(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        var!(oror, false) in [false, nil] -> false
        var!(oror, false) -> var!(oror, false)
      end
    end)
  end

  test :expand_once_with_not_expandable_expression do
    expr = quote(do: other(1, 2, 3))
    assert Macro.expand_once(expr, __ENV__) == expr
  end

  @foo 1
  @bar Macro.expand_once(quote(do: @foo), __ENV__)

  test :expand_once_with_module_at do
    assert @bar == 1
  end

  test :expand do
    assert Macro.expand(quote(do: oror(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        var!(oror, false) in [false, nil] -> false
        var!(oror, false) -> var!(oror, false)
      end
    end)
  end

  test :expand_all do
    assert Macro.expand_all(quote(do: oror(1, local_macro)), __ENV__) == (quote context: Kernel do
      case 1 do
        var!(oror, false) in [false, nil] -> :local_macro
        var!(oror, false) -> var!(oror, false)
      end
    end)
  end

  ## to_string

  test :var_to_string do
    assert Macro.to_string(quote do: foo) == "foo"
  end

  test :local_call_to_string do
    assert Macro.to_string(quote do: foo(1, 2, 3)) == "foo(1, 2, 3)"
    assert Macro.to_string(quote do: foo([1, 2, 3])) == "foo([1, 2, 3])"
  end

  test :remote_call_to_string do
    assert Macro.to_string(quote do: foo.bar(1, 2, 3)) == "foo.bar(1, 2, 3)"
    assert Macro.to_string(quote do: foo.bar([1, 2, 3])) == "foo.bar([1, 2, 3])"
  end

  test :low_atom_remote_call_to_string do
    assert Macro.to_string(quote do: :foo.bar(1, 2, 3)) == ":foo.bar(1, 2, 3)"
  end

  test :big_atom_remote_call_to_string do
    assert Macro.to_string(quote do: Foo.Bar.bar(1, 2, 3)) == "Foo.Bar.bar(1, 2, 3)"
  end

  test :remote_and_fun_call_to_string do
    assert Macro.to_string(quote do: foo.bar.(1, 2, 3)) == "foo.bar().(1, 2, 3)"
    assert Macro.to_string(quote do: foo.bar.([1, 2, 3])) == "foo.bar().([1, 2, 3])"
  end

  test :atom_call_to_string do
    assert Macro.to_string(quote do: :foo.(1, 2, 3)) == ":foo.(1, 2, 3)"
  end

  test :aliases_call_to_string do
    assert Macro.to_string(quote do: Foo.Bar.baz(1, 2, 3)) == "Foo.Bar.baz(1, 2, 3)"
    assert Macro.to_string(quote do: Foo.Bar.baz([1, 2, 3])) == "Foo.Bar.baz([1, 2, 3])"
  end

  test :arrow_to_string do
    assert Macro.to_string(quote do: foo(1, (2 -> 3))) == "foo(1, (2 -> 3))"
  end

  test :blocks_to_string do
    assert Macro.to_string(quote do: (1; 2; (:foo; :bar); 3)) <> "\n" == """
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

  test :if_else_to_string do
    assert Macro.to_string(quote do: (if foo, do: bar, else: baz)) <> "\n" == """
    if(foo) do
      bar
    else
      baz
    end
    """
  end

  test :case_to_string do
    assert Macro.to_string(quote do: (case foo do true -> 0; false -> (1; 2) end)) <> "\n" == """
    case(foo) do
      true ->
        0
      false ->
        1
        2
    end
    """
  end

  test :fn_to_string do
    assert Macro.to_string(quote do: (fn -> 1 + 2 end)) == "fn -> 1 + 2 end"
    assert Macro.to_string(quote do: (fn(x) -> x + 1 end)) == "fn x -> x + 1 end"

    assert Macro.to_string(quote do: (fn(x) -> y = x + 1; y end)) <> "\n" == """
    fn x ->
      y = (x + 1)
      y
    end
    """

    assert Macro.to_string(quote do: (fn(x) -> y = x + 1; y; (z) -> z end)) <> "\n" == """
    fn
      x ->
        y = (x + 1)
        y
      z ->
        z
    end
    """
  end

  test :when do
    assert Macro.to_string(quote do: (() -> x)) == "(() -> x)"
    assert Macro.to_string(quote do: (x when y -> z)) == "(x when y -> z)"
    assert Macro.to_string(quote do: (x, y when z -> w)) == "((x, y) when z -> w)"
    assert Macro.to_string(quote do: ((x, y) when z -> w)) == "((x, y) when z -> w)"
  end

  test :nested_to_string do
    assert Macro.to_string(quote do: (defmodule Foo do def foo do 1 + 1 end end)) <> "\n" == """
    defmodule(Foo) do
      def(foo) do
        1 + 1
      end
    end
    """
  end

  test :op_precedence_to_string do
    assert Macro.to_string(quote do: (1 + 2) * (3 - 4)) == "(1 + 2) * (3 - 4)"
    assert Macro.to_string(quote do: ((1 + 2) * 3) - 4) == "((1 + 2) * 3) - 4"
  end

  test :containers_to_string do
    assert Macro.to_string(quote do: { 1, 2, 3 })   == "{1, 2, 3}"
    assert Macro.to_string(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
    assert Macro.to_string(quote do: << 1, 2, 3 >>) == "<<1, 2, 3>>"
    assert Macro.to_string(quote do: << <<1>> >>) == "<< <<1>> >>"
  end

  test :binary_ops_to_string do
    assert Macro.to_string(quote do: 1 + 2)   == "1 + 2"
    assert Macro.to_string(quote do: [ 1, 2 | 3 ]) == "[1, 2 | 3]"
    assert Macro.to_string(quote do: [h|t] = [1, 2, 3]) == "[h | t] = [1, 2, 3]"
  end

  test :unary_ops_to_string do
    assert Macro.to_string(quote do: not 1) == "not 1"
    assert Macro.to_string(quote do: not foo) == "not foo"
    assert Macro.to_string(quote do: -1) == "-1"
    assert Macro.to_string(quote do: @foo(bar)) == "@foo(bar)"
    assert Macro.to_string(quote do: identity(&1)) == "identity(&1)"
    assert Macro.to_string(quote do: identity(&foo)) == "identity(&foo)"
  end

  ## safe_term

  test :safe_terms do
   assert Macro.safe_term(quote do: 1) == :ok
   assert Macro.safe_term(quote do: 1.1) == :ok
   assert Macro.safe_term(quote do: -1) == :ok
   assert Macro.safe_term(quote do: +1) == :ok
   assert Macro.safe_term(quote do: []) == :ok
   assert Macro.safe_term(quote do: [1, 2, 3]) == :ok
   assert Macro.safe_term(quote do: "") == :ok
   assert Macro.safe_term(quote do: {}) == :ok
   assert Macro.safe_term(quote do: {1, 2}) == :ok
   assert Macro.safe_term(quote do: {1, 2, 3}) == :ok
   assert Macro.safe_term(quote do: {1, 2, 3, 4}) == :ok
   assert Macro.safe_term(quote do: Alias) == :ok
  end

  test :unsafe_terms do
   assert Macro.safe_term(quote do: 1+1)   == { :unsafe, quote do: 1 + 1 }
   assert Macro.safe_term(quote do: [1+1]) == { :unsafe, quote do: 1 + 1 }
   assert Macro.safe_term(quote do: {1+1}) == { :unsafe, quote do: 1 + 1 }
  end

  ## extract_args

  test :extract_args do
    assert Macro.extract_args(quote do: foo)        == { :foo, [] }
    assert Macro.extract_args(quote do: foo())      == { :foo, [] }
    assert Macro.extract_args(quote do: :foo.())    == { :foo, [] }
    assert Macro.extract_args(quote do: foo(1, 2, 3)) == { :foo, [1, 2, 3] }
    assert Macro.extract_args(quote do: 1.(1, 2, 3))  == :error
  end

  ## env

  test :env_stacktrace do
    env = __ENV__.file("foo").line(12)
    assert env.stacktrace == [{ __MODULE__, :test_env_stacktrace, 1, [file: "foo", line: 12] }]
    env = env.function(nil)
    assert env.stacktrace == [{ __MODULE__, :__MODULE__, 0, [file: "foo", line: 12] }]
    env = env.module(nil)
    assert env.stacktrace == [{ :elixir_compiler, :__FILE__, 2, [file: "foo", line: 12] }]
  end
end
