Code.require_file "test_helper.exs", __DIR__

defmodule Macro.ExternalTest do
  defmacro external do
    line = 18
    file = __ENV__.file
    ^line = __CALLER__.line
    ^file = __CALLER__.file
    ^line = Macro.Env.location(__CALLER__)[:line]
    ^file = Macro.Env.location(__CALLER__)[:file]
  end

  defmacro oror(left, right) do
    quote do: unquote(left) || unquote(right)
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true

  # Changing the lines above will make compilation
  # fail since we are asserting on the caller lines
  import Macro.ExternalTest

  ## Escape

  test :escape_handle_tuples_with_size_different_than_two do
    assert {:{}, [], [:a]} == Macro.escape({:a})
    assert {:{}, [], [:a, :b, :c]} == Macro.escape({:a, :b, :c})
    assert {:{}, [], [:a, {:{}, [], [1, 2, 3]}, :c]} == Macro.escape({:a, {1, 2, 3}, :c})
  end

  test :escape_simply_returns_tuples_with_size_equal_to_two do
    assert {:a, :b} == Macro.escape({:a, :b})
  end

  test :escape_simply_returns_any_other_structure do
    assert [1, 2, 3] == Macro.escape([1, 2, 3])
  end

  test :escape_handles_maps do
    assert {:%{}, [], [a: 1]} = Macro.escape(%{a: 1})
  end

  test :escape_handles_bitstring do
    assert {:<<>>, [], [{:::, [], [1, 4]}, ","]} == Macro.escape(<<300::12>>)
  end

  test :escape_works_recursively do
    assert [1, {:{}, [], [:a, :b, :c]}, 3] == Macro.escape([1, {:a, :b, :c}, 3])
  end

  test :escape_improper do
    assert [{:|, [], [1, 2]}] == Macro.escape([1|2])
    assert [1, {:|, [], [2, 3]}] == Macro.escape([1, 2|3])
  end

  test :escape_with_unquote do
    contents = quote unquote: false, do: unquote(1)
    assert Macro.escape(contents, unquote: true) == 1

    contents = quote unquote: false, do: unquote(x)
    assert Macro.escape(contents, unquote: true) == {:x, [], MacroTest}
  end

  defp eval_escaped(contents) do
    {eval, []} = Code.eval_quoted(Macro.escape(contents, unquote: true))
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

  ## Expansion

  test :expand_once do
    assert {:||, _, _} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
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
    alias Foo, as: Bar, warn: false
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

  test :expand_once_env do
    env = %{__ENV__ | line: 0}
    assert Macro.expand_once(quote(do: __ENV__), env) == {:%{}, [], Map.to_list(env)}
    assert Macro.expand_once(quote(do: __ENV__.file), env) == env.file
    assert Macro.expand_once(quote(do: __ENV__.unknown), env) == quote(do: __ENV__.unknown)
  end

  defmacro local_macro do
    :local_macro
  end

  test :expand_once_local_macro do
    assert Macro.expand_once(quote(do: local_macro), __ENV__) == :local_macro
  end

  test :expand_once_checks_vars do
    local_macro = 1
    assert local_macro == 1
    quote = {:local_macro, [], nil}
    assert Macro.expand_once(quote, __ENV__) == quote
  end

  defp expand_once_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])
    quoted
    |> Macro.expand_once(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test :expand_once_with_imported_macro do
    temp_var = {:x, [], Kernel}
    assert expand_once_and_clean(quote(do: 1 || false), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
      end
    end)
  end

  test :expand_once_with_require_macro do
    temp_var = {:x, [], Kernel}
    assert expand_once_and_clean(quote(do: Kernel.||(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
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

  defp expand_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])
    quoted
    |> Macro.expand(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test :expand do
    temp_var = {:x, [], Kernel}
    assert expand_and_clean(quote(do: oror(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
      end
    end)
  end

  test :var do
    assert Macro.var(:foo, nil) == {:foo, [], nil}
    assert Macro.var(:foo, Other) == {:foo, [], Other}
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

  test :sigil_call_to_string do
    assert Macro.to_string(quote do: ~r"123") == ~s/~r"123"/
    assert Macro.to_string(quote do: ~r"123"u) == ~s/~r"123"u/
    assert Macro.to_string(quote do: ~r"\n123") == ~s/~r"\\\\n123"/

    assert Macro.to_string(quote do: ~r"1#{two}3") == ~S/~r"1#{two}3"/
    assert Macro.to_string(quote do: ~r"1#{two}3"u) == ~S/~r"1#{two}3"u/

    assert Macro.to_string(quote do: ~R"123") == ~s/~R"123"/
    assert Macro.to_string(quote do: ~R"123"u) == ~s/~R"123"u/
    assert Macro.to_string(quote do: ~R"\n123") == ~s/~R"\\\\n123"/
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
      y = x + 1
      y
    end
    """

    assert Macro.to_string(quote do: (fn(x) -> y = x + 1; y; (z) -> z end)) <> "\n" == """
    fn
      x ->
        y = x + 1
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
    assert Macro.to_string(quote do: ((1 + 2) * 3) - 4) == "(1 + 2) * 3 - 4"
    assert Macro.to_string(quote do: (1 + 2 + 3) == "(1 + 2 + 3)")
    assert Macro.to_string(quote do: (1 + 2 - 3) == "(1 + 2 - 3)")
  end

  test :containers_to_string do
    assert Macro.to_string(quote do: {})   == "{}"
    assert Macro.to_string(quote do: [])   == "[]"
    assert Macro.to_string(quote do: {1, 2, 3})   == "{1, 2, 3}"
    assert Macro.to_string(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
    assert Macro.to_string(quote do: %{})  == "%{}"
    assert Macro.to_string(quote do: %{:foo => :bar})  == "%{foo: :bar}"
    assert Macro.to_string(quote do: %{{1, 2} => [1, 2, 3]})  == "%{{1, 2} => [1, 2, 3]}"
    assert Macro.to_string(quote do: %{map | "a" => "b"})  == "%{map | \"a\" => \"b\"}"
    assert Macro.to_string(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
    assert Macro.to_string(quote do: << 1, 2, 3 >>) == "<<1, 2, 3>>"
    assert Macro.to_string(quote do: << <<1>> >>) == "<< <<1>> >>"
  end

  test :struct_to_string do
    assert Macro.to_string(quote do: %Test{})  == "%Test{}"
    assert Macro.to_string(quote do: %Test{foo: 1, bar: 1})  == "%Test{foo: 1, bar: 1}"
    assert Macro.to_string(quote do: %Test{struct | foo: 2})  == "%Test{struct | foo: 2}"
    assert Macro.to_string(quote do: %Test{} + 1)  == "%Test{} + 1"
  end

  test :binary_ops_to_string do
    assert Macro.to_string(quote do: 1 + 2)   == "1 + 2"
    assert Macro.to_string(quote do: [ 1, 2 | 3 ]) == "[1, 2 | 3]"
    assert Macro.to_string(quote do: [h|t] = [1, 2, 3]) == "[h | t] = [1, 2, 3]"
    assert Macro.to_string(quote do: (x ++ y) ++ z) == "(x ++ y) ++ z"
  end

  test :unary_ops_to_string do
    assert Macro.to_string(quote do: not 1) == "not 1"
    assert Macro.to_string(quote do: not foo) == "not foo"
    assert Macro.to_string(quote do: -1) == "-1"
    assert Macro.to_string(quote do: !(foo > bar)) == "!(foo > bar)"
    assert Macro.to_string(quote do: @foo(bar)) == "@foo(bar)"
    assert Macro.to_string(quote do: identity(&1)) == "identity(&1)"
    assert Macro.to_string(quote do: identity(&foo)) == "identity(&foo)"
  end

  test :access_to_string do
    assert Macro.to_string(quote do: a[b]) == "a[b]"
    assert Macro.to_string(quote do: a[1 + 2]) == "a[1 + 2]"
  end

  test :kw_list_to_string do
    assert Macro.to_string(quote do: [a: a, b: b]) == "[a: a, b: b]"
    assert Macro.to_string(quote do: [a: 1, b: 1 + 2]) == "[a: 1, b: 1 + 2]"
    assert Macro.to_string(quote do: ["a.b": 1, c: 1 + 2]) == "[\"a.b\": 1, c: 1 + 2]"
  end

  test :interpolation_to_string do
    assert Macro.to_string(quote do: "foo#{bar}baz") == ~S["foo#{bar}baz"]
  end

  test :charlist_to_string do
    assert Macro.to_string(quote do: []) == "[]"
    assert Macro.to_string(quote do: 'abc') == "'abc'"
  end

  test :last_arg_kw_list_to_string do
    assert Macro.to_string(quote do: foo([])) == "foo([])"
    assert Macro.to_string(quote do: foo(x: y)) == "foo(x: y)"
    assert Macro.to_string(quote do: foo(x: 1 + 2)) == "foo(x: 1 + 2)"
    assert Macro.to_string(quote do: foo(x: y, p: q)) == "foo(x: y, p: q)"
    assert Macro.to_string(quote do: foo(a, x: y, p: q)) == "foo(a, x: y, p: q)"

    assert Macro.to_string(quote do: {[]}) == "{[]}"
    assert Macro.to_string(quote do: {[a: b]}) == "{[a: b]}"
    assert Macro.to_string(quote do: {x, a: b}) == "{x, [a: b]}"
  end

  test :to_string_with_fun do
    assert Macro.to_string(quote(do: foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
           ":foo(:1:, :2:, :3:):"

    assert Macro.to_string(quote(do: Bar.foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
           "::Bar:.foo(:1:, :2:, :3:):"
  end

  ## validate

  test :validate do
    ref = make_ref()

    assert Macro.validate(1) == :ok
    assert Macro.validate(1.0) == :ok
    assert Macro.validate(:foo) == :ok
    assert Macro.validate("bar") == :ok
    assert Macro.validate(self()) == :ok
    assert Macro.validate({1, 2}) == :ok
    assert Macro.validate({:foo, [], :baz}) == :ok
    assert Macro.validate({:foo, [], []}) == :ok
    assert Macro.validate([1, 2, 3]) == :ok

    assert Macro.validate(<<0::4>>) == {:error, <<0::4>>}
    assert Macro.validate(ref) == {:error, ref}
    assert Macro.validate({1, ref}) == {:error, ref}
    assert Macro.validate({ref, 2}) == {:error, ref}
    assert Macro.validate([1, ref, 3]) == {:error, ref}
    assert Macro.validate({:foo, [], 0}) == {:error, {:foo, [], 0}}
    assert Macro.validate({:foo, 0, []}) == {:error, {:foo, 0, []}}
  end

  ## decompose_call

  test :decompose_call do
    assert Macro.decompose_call(quote do: foo)        == {:foo, []}
    assert Macro.decompose_call(quote do: foo())      == {:foo, []}
    assert Macro.decompose_call(quote do: foo(1, 2, 3)) == {:foo, [1, 2, 3]}
    assert Macro.decompose_call(quote do: M.N.foo(1, 2, 3)) ==
           {{:__aliases__, [alias: false], [:M, :N]}, :foo, [1, 2, 3]}
    assert Macro.decompose_call(quote do: :foo.foo(1, 2, 3)) ==
           {:foo, :foo, [1, 2, 3]}
    assert Macro.decompose_call(quote do: 1.(1, 2, 3))  == :error
    assert Macro.decompose_call(quote do: "some string")  == :error
  end

  ## env

  test :env_stacktrace do
    env = %{__ENV__ | file: "foo", line: 12}
    assert Macro.Env.stacktrace(env) ==
           [{__MODULE__, :"test env_stacktrace", 1, [file: "foo", line: 12]}]

    env = %{env | function: nil}
    assert Macro.Env.stacktrace(env) ==
           [{__MODULE__, :__MODULE__, 0, [file: "foo", line: 12]}]

    env = %{env | module: nil}
    assert Macro.Env.stacktrace(env) ==
           [{:elixir_compiler, :__FILE__, 1, [file: "foo", line: 12]}]
  end

  test :context_modules do
    defmodule Foo.Bar do
      assert __MODULE__ in __ENV__.context_modules
    end
  end

  ## pipe/unpipe

  test :pipe do
    assert Macro.pipe(1, quote(do: foo), 0) == quote(do: foo(1))
    assert Macro.pipe(1, quote(do: foo(2)), 0) == quote(do: foo(1, 2))
    assert Macro.pipe(1, quote(do: foo), -1) == quote(do: foo(1))
    assert Macro.pipe(2, quote(do: foo(1)), -1) == quote(do: foo(1, 2))

    assert_raise ArgumentError, ~r"cannot pipe 1 into 2", fn ->
      Macro.pipe(1, 2, 0)
    end
  end

  test :unpipe do
    assert Macro.unpipe(quote(do: foo)) == quote(do: [{foo, 0}])
    assert Macro.unpipe(quote(do: foo |> bar)) == quote(do: [{foo, 0}, {bar, 0}])
    assert Macro.unpipe(quote(do: foo |> bar |> baz)) == quote(do: [{foo, 0}, {bar, 0}, {baz, 0}])
  end

  ## pre/postwalk

  test :prewalk do
    assert prewalk({:foo, [], nil}) ==
           [{:foo, [], nil}]

    assert prewalk({:foo, [], [1, 2, 3]}) ==
           [{:foo, [], [1, 2, 3]}, 1, 2, 3]

    assert prewalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
           [{{:., [], [:foo, :bar]}, [], [1, 2, 3]}, {:., [], [:foo, :bar]}, :foo, :bar, 1, 2, 3]

    assert prewalk({[1, 2, 3], [4, 5, 6]}) ==
           [{[1, 2, 3], [4, 5, 6]}, [1, 2, 3], 1, 2, 3, [4, 5, 6], 4, 5, 6]
  end

  defp prewalk(ast) do
    Macro.prewalk(ast, [], &{&1, [&1|&2]}) |> elem(1) |> Enum.reverse
  end

  test :postwalk do
    assert postwalk({:foo, [], nil}) ==
           [{:foo, [], nil}]

    assert postwalk({:foo, [], [1, 2, 3]}) ==
           [1, 2, 3, {:foo, [], [1, 2, 3]}]

    assert postwalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
           [:foo, :bar, {:., [], [:foo, :bar]}, 1, 2, 3, {{:., [], [:foo, :bar]}, [], [1, 2, 3]}]

    assert postwalk({[1, 2, 3], [4, 5, 6]}) ==
           [1, 2, 3, [1, 2, 3], 4, 5, 6, [4, 5, 6], {[1, 2, 3], [4, 5, 6]}]
  end

  defp postwalk(ast) do
    Macro.postwalk(ast, [], &{&1, [&1|&2]}) |> elem(1) |> Enum.reverse
  end
end
