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

  test "escape handle tuples with size different than two" do
    assert {:{}, [], [:a]} == Macro.escape({:a})
    assert {:{}, [], [:a, :b, :c]} == Macro.escape({:a, :b, :c})
    assert {:{}, [], [:a, {:{}, [], [1, 2, 3]}, :c]} == Macro.escape({:a, {1, 2, 3}, :c})
  end

  test "escape simply returns tuples with size equal to two" do
    assert {:a, :b} == Macro.escape({:a, :b})
  end

  test "escape simply returns any other structure" do
    assert [1, 2, 3] == Macro.escape([1, 2, 3])
  end

  test "escape handles maps" do
    assert {:%{}, [], [a: 1]} = Macro.escape(%{a: 1})
  end

  test "escape handles bitstring" do
    assert {:<<>>, [], [{:::, [], [1, 4]}, ","]} == Macro.escape(<<300::12>>)
  end

  test "escape works recursively" do
    assert [1, {:{}, [], [:a, :b, :c]}, 3] == Macro.escape([1, {:a, :b, :c}, 3])
  end

  test "escape improper" do
    assert [{:|, [], [1, 2]}] == Macro.escape([1 | 2])
    assert [1, {:|, [], [2, 3]}] == Macro.escape([1, 2 | 3])
  end

  test "escape with unquote" do
    contents = quote unquote: false, do: unquote(1)
    assert Macro.escape(contents, unquote: true) == 1

    contents = quote unquote: false, do: unquote(x)
    assert Macro.escape(contents, unquote: true) == {:x, [], MacroTest}
  end

  defp eval_escaped(contents) do
    {eval, []} = Code.eval_quoted(Macro.escape(contents, unquote: true))
    eval
  end

  test "escape with remote unquote" do
    contents = quote unquote: false, do: Kernel.unquote(:is_atom)(:ok)
    assert eval_escaped(contents) == quote(do: Kernel.is_atom(:ok))
  end

  test "escape with nested unquote" do
    contents = quote do
      quote do: unquote(x)
    end
    assert eval_escaped(contents) == quote do: (quote do: unquote(x))
  end

  test "escape with alias or no args remote unquote" do
    contents = quote unquote: false, do: Kernel.unquote(:self)
    assert eval_escaped(contents) == quote(do: Kernel.self())

    contents = quote unquote: false, do: x.unquote(Foo)
    assert eval_escaped(contents) == quote(do: x.unquote(Foo))
  end

  test "escape with splicing" do
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

    contents = quote unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]) | [5]]
    assert eval_escaped(contents) == [1, 2, 3, 4, 5]
  end

  ## Expansion

  test "expand once" do
    assert {:||, _, _} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
  end

  test "expand once with raw atom" do
    assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
  end

  test "expand once with current module" do
    assert Macro.expand_once(quote(do: __MODULE__), __ENV__) == __MODULE__
  end

  test "expand once with main" do
    assert Macro.expand_once(quote(do: Elixir), __ENV__) == Elixir
  end

  test "expand once with simple alias" do
    assert Macro.expand_once(quote(do: Foo), __ENV__) == Foo
  end

  test "expand once with current module plus alias" do
    assert Macro.expand_once(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
  end

  test "expand once with main plus alias" do
    assert Macro.expand_once(quote(do: Elixir.Foo), __ENV__) == Foo
  end

  test "expand once with custom alias" do
    alias Foo, as: Bar
    assert Macro.expand_once(quote(do: Bar.Baz), __ENV__) == Foo.Baz
  end

  test "expand once with main plus custom alias" do
    alias Foo, as: Bar, warn: false
    assert Macro.expand_once(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
  end

  test "expand once with op" do
    assert Macro.expand_once(quote(do: Foo.bar.Baz), __ENV__) == (quote do
      Foo.bar.Baz
    end)
  end

  test "expand once with Erlang" do
    assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
  end

  test "expand once env" do
    env = %{__ENV__ | line: 0}
    assert Macro.expand_once(quote(do: __ENV__), env) == {:%{}, [], Map.to_list(env)}
    assert Macro.expand_once(quote(do: __ENV__.file), env) == env.file
    assert Macro.expand_once(quote(do: __ENV__.unknown), env) == quote(do: __ENV__.unknown)
  end

  defmacro local_macro do
    :local_macro
  end

  test "expand once local macro" do
    assert Macro.expand_once(quote(do: local_macro), __ENV__) == :local_macro
  end

  test "expand once checks vars" do
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

  test "expand once with imported macro" do
    temp_var = {:x, [], Kernel}
    assert expand_once_and_clean(quote(do: 1 || false), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
      end
    end)
  end

  test "expand once with require macro" do
    temp_var = {:x, [], Kernel}
    assert expand_once_and_clean(quote(do: Kernel.||(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
      end
    end)
  end

  test "expand once with not expandable expression" do
    expr = quote(do: other(1, 2, 3))
    assert Macro.expand_once(expr, __ENV__) == expr
  end

  test "expand once does not expand module attributes" do
    message = "could not call get_attribute on module #{inspect(__MODULE__)} " <>
              "because it was already compiled"
    assert_raise ArgumentError, message, fn ->
      Macro.expand_once(quote(do: @foo), __ENV__)
    end
  end

  defp expand_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])
    quoted
    |> Macro.expand(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test "expand" do
    temp_var = {:x, [], Kernel}
    assert expand_and_clean(quote(do: oror(1, false)), __ENV__) == (quote context: Kernel do
      case 1 do
        unquote(temp_var) when unquote(temp_var) in [false, nil] -> false
        unquote(temp_var) -> unquote(temp_var)
      end
    end)
  end

  test "var" do
    assert Macro.var(:foo, nil) == {:foo, [], nil}
    assert Macro.var(:foo, Other) == {:foo, [], Other}
  end

  ## to_string

  test "var to string" do
    assert Macro.to_string(quote do: foo) == "foo"
  end

  test "local call to string" do
    assert Macro.to_string(quote do: foo(1, 2, 3)) == "foo(1, 2, 3)"
    assert Macro.to_string(quote do: foo([1, 2, 3])) == "foo([1, 2, 3])"
  end

  test "remote call to string" do
    assert Macro.to_string(quote do: foo.bar(1, 2, 3)) == "foo.bar(1, 2, 3)"
    assert Macro.to_string(quote do: foo.bar([1, 2, 3])) == "foo.bar([1, 2, 3])"
  end

  test "atom remote call to string" do
    assert Macro.to_string(quote do: :foo.bar(1, 2, 3)) == ":foo.bar(1, 2, 3)"
  end

  test "remote and fun call to string" do
    assert Macro.to_string(quote do: foo.bar.(1, 2, 3)) == "foo.bar().(1, 2, 3)"
    assert Macro.to_string(quote do: foo.bar.([1, 2, 3])) == "foo.bar().([1, 2, 3])"
  end

  test "unusual remote atom fun call to string" do
    assert Macro.to_string(quote do: Foo."42") == ~s/Foo."42"()/
    assert Macro.to_string(quote do: Foo.'Bar') == ~s/Foo."Bar"()/
    assert Macro.to_string(quote do: Foo."bar baz"."") == ~s/Foo."bar baz"().""()/
    assert Macro.to_string(quote do: Foo."%{}") == ~s/Foo."%{}"()/
    assert Macro.to_string(quote do: Foo."...") == ~s/Foo."..."()/
  end

  test "atom fun call to string" do
    assert Macro.to_string(quote do: :foo.(1, 2, 3)) == ":foo.(1, 2, 3)"
  end

  test "aliases call to string" do
    assert Macro.to_string(quote do: Foo.Bar.baz(1, 2, 3)) == "Foo.Bar.baz(1, 2, 3)"
    assert Macro.to_string(quote do: Foo.Bar.baz([1, 2, 3])) == "Foo.Bar.baz([1, 2, 3])"
    assert Macro.to_string(quote do: Foo.bar(<<>>, [])) == "Foo.bar(<<>>, [])"
  end

  test "keyword call to string" do
    assert Macro.to_string(quote do: Foo.bar(foo: :bar)) == "Foo.bar(foo: :bar)"
    assert Macro.to_string(quote do: Foo.bar(["Elixir.Foo": :bar])) == "Foo.bar([{Foo, :bar}])"
  end

  test "sigil call to string" do
    assert Macro.to_string(quote do: ~r"123") == ~s/~r"123"/
    assert Macro.to_string(quote do: ~r"123"u) == ~s/~r"123"u/
    assert Macro.to_string(quote do: ~r"\n123") == ~s/~r"\\\\n123"/

    assert Macro.to_string(quote do: ~r"1#{two}3") == ~S/~r"1#{two}3"/
    assert Macro.to_string(quote do: ~r"1#{two}3"u) == ~S/~r"1#{two}3"u/

    assert Macro.to_string(quote do: ~R"123") == ~s/~R"123"/
    assert Macro.to_string(quote do: ~R"123"u) == ~s/~R"123"u/
    assert Macro.to_string(quote do: ~R"\n123") == ~s/~R"\\\\n123"/
  end

  test "arrow to string" do
    assert Macro.to_string(quote do: foo(1, (2 -> 3))) == "foo(1, (2 -> 3))"
  end

  test "blocks to string" do
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

  test "not in to string" do
    assert Macro.to_string(quote do: (false not in [])) == "false not in []"
  end

  test "if else to string" do
    assert Macro.to_string(quote do: (if foo, do: bar, else: baz)) <> "\n" == """
    if(foo) do
      bar
    else
      baz
    end
    """
  end

  test "case to string" do
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

  test "fn to string" do
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

    assert Macro.to_string(quote do: (fn(x) -> x end).(1)) == "(fn x -> x end).(1)"

    assert Macro.to_string(quote do: (fn %{} -> :map; _ -> :other end).(1)) <> "\n" == """
    (fn
      %{} ->
        :map
      _ ->
        :other
    end).(1)
    """
  end

  test "range to string" do
    assert Macro.to_string(quote do: unquote(-1..+2)) == "-1..2"
    assert Macro.to_string(quote do: Foo.integer..3) == "Foo.integer()..3"
  end

  test "when to string" do
    assert Macro.to_string(quote do: (() -> x)) == "(() -> x)"
    assert Macro.to_string(quote do: (x when y -> z)) == "(x when y -> z)"
    assert Macro.to_string(quote do: (x, y when z -> w)) == "((x, y) when z -> w)"
    assert Macro.to_string(quote do: ((x, y) when z -> w)) == "((x, y) when z -> w)"
  end

  test "nested to string" do
    assert Macro.to_string(quote do: (defmodule Foo do def foo do 1 + 1 end end)) <> "\n" == """
    defmodule(Foo) do
      def(foo) do
        1 + 1
      end
    end
    """
  end

  test "op precedence to string" do
    assert Macro.to_string(quote do: (1 + 2) * (3 - 4)) == "(1 + 2) * (3 - 4)"
    assert Macro.to_string(quote do: ((1 + 2) * 3) - 4) == "(1 + 2) * 3 - 4"
    assert Macro.to_string(quote do: (1 + 2 + 3) == "(1 + 2 + 3)")
    assert Macro.to_string(quote do: (1 + 2 - 3) == "(1 + 2 - 3)")
  end

  test "capture op to string" do
    assert Macro.to_string(quote do: &foo/0) == "&foo/0"
    assert Macro.to_string(quote do: &Foo.foo/0) == "&Foo.foo/0"
    assert Macro.to_string(quote do: & &1 + &2) == "&(&1 + &2)"
    assert Macro.to_string(quote do: & &1) == "&(&1)"
    assert Macro.to_string(quote do: &(&1).(:x)) == "&(&1.(:x))"
    assert Macro.to_string(quote do: (&(&1)).(:x)) == "(&(&1)).(:x)"
  end

  test "containers to string" do
    assert Macro.to_string(quote do: {})   == "{}"
    assert Macro.to_string(quote do: [])   == "[]"
    assert Macro.to_string(quote do: {1, 2, 3})   == "{1, 2, 3}"
    assert Macro.to_string(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
    assert Macro.to_string(quote do: ["Elixir.Foo": :bar]) == "[{Foo, :bar}]"
    assert Macro.to_string(quote do: %{})  == "%{}"
    assert Macro.to_string(quote do: %{:foo => :bar})  == "%{foo: :bar}"
    assert Macro.to_string(quote do: %{:"Elixir.Foo" => :bar}) == "%{Foo => :bar}"
    assert Macro.to_string(quote do: %{{1, 2} => [1, 2, 3]})  == "%{{1, 2} => [1, 2, 3]}"
    assert Macro.to_string(quote do: %{map | "a" => "b"})  == "%{map | \"a\" => \"b\"}"
    assert Macro.to_string(quote do: [ 1, 2, 3 ])   == "[1, 2, 3]"
  end

  test "struct to string" do
    assert Macro.to_string(quote do: %Test{})  == "%Test{}"
    assert Macro.to_string(quote do: %Test{foo: 1, bar: 1})  == "%Test{foo: 1, bar: 1}"
    assert Macro.to_string(quote do: %Test{struct | foo: 2})  == "%Test{struct | foo: 2}"
    assert Macro.to_string(quote do: %Test{} + 1)  == "%Test{} + 1"
  end

  test "binary ops to string" do
    assert Macro.to_string(quote do: 1 + 2)   == "1 + 2"
    assert Macro.to_string(quote do: [ 1, 2 | 3 ]) == "[1, 2 | 3]"
    assert Macro.to_string(quote do: [h | t] = [1, 2, 3]) == "[h | t] = [1, 2, 3]"
    assert Macro.to_string(quote do: (x ++ y) ++ z) == "(x ++ y) ++ z"
  end

  test "unary ops to string" do
    assert Macro.to_string(quote do: not 1) == "not 1"
    assert Macro.to_string(quote do: not foo) == "not foo"
    assert Macro.to_string(quote do: -1) == "-1"
    assert Macro.to_string(quote do: !(foo > bar)) == "!(foo > bar)"
    assert Macro.to_string(quote do: @foo(bar)) == "@foo(bar)"
    assert Macro.to_string(quote do: identity(&1)) == "identity(&1)"
  end

  test "access to string" do
    assert Macro.to_string(quote do: a[b]) == "a[b]"
    assert Macro.to_string(quote do: a[1 + 2]) == "a[1 + 2]"
    assert Macro.to_string(quote do: (a || [a: 1])[:a]) == "(a || [a: 1])[:a]"
    assert Macro.to_string(quote do: Map.put(%{}, :a, 1)[:a]) == "Map.put(%{}, :a, 1)[:a]"
  end

  test "kw list to string" do
    assert Macro.to_string(quote do: [a: a, b: b]) == "[a: a, b: b]"
    assert Macro.to_string(quote do: [a: 1, b: 1 + 2]) == "[a: 1, b: 1 + 2]"
    assert Macro.to_string(quote do: ["a.b": 1, c: 1 + 2]) == "[\"a.b\": 1, c: 1 + 2]"
  end

  test "interpolation to string" do
    assert Macro.to_string(quote do: "foo#{bar}baz") == ~S["foo#{bar}baz"]
  end

  test "bit syntax to string" do
    ast = quote(do: <<1::8*4>>)
    assert Macro.to_string(ast) == "<<1::8*4>>"

    ast = quote(do: @type foo :: <<_::8, _::_*4>>)
    assert Macro.to_string(ast) == "@type(foo :: <<_::8, _::_*4>>)"

    ast = quote(do: <<69 - 4::bits-size(8 - 4)-unit(1), 65>>)
    assert Macro.to_string(ast) == "<<69 - 4::bits-size(8 - 4)-unit(1), 65>>"

    ast = quote(do: << <<65>>, 65>>)
    assert Macro.to_string(ast) == "<<(<<65>>), 65>>"

    ast = quote(do: <<65, <<65>> >>)
    assert Macro.to_string(ast) == "<<65, (<<65>>)>>"

    ast = quote(do: (for <<a::4 <- <<1, 2>> >>, do: a))
    assert Macro.to_string(ast) == "for(<<(a :: 4 <- <<1, 2>>)>>) do\n  a\nend"
  end

  test "charlist to string" do
    assert Macro.to_string(quote do: []) == "[]"
    assert Macro.to_string(quote do: 'abc') == "'abc'"
  end

  test "last arg kw list to string" do
    assert Macro.to_string(quote do: foo([])) == "foo([])"
    assert Macro.to_string(quote do: foo(x: y)) == "foo(x: y)"
    assert Macro.to_string(quote do: foo(x: 1 + 2)) == "foo(x: 1 + 2)"
    assert Macro.to_string(quote do: foo(x: y, p: q)) == "foo(x: y, p: q)"
    assert Macro.to_string(quote do: foo(a, x: y, p: q)) == "foo(a, x: y, p: q)"

    assert Macro.to_string(quote do: {[]}) == "{[]}"
    assert Macro.to_string(quote do: {[a: b]}) == "{[a: b]}"
    assert Macro.to_string(quote do: {x, a: b}) == "{x, [a: b]}"
    assert Macro.to_string(quote do: foo(else: a)) == "foo(else: a)"
    assert Macro.to_string(quote do: foo(catch: a)) == "foo(catch: a)"
  end

  test "to string with fun" do
    assert Macro.to_string(quote(do: foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
           ":foo(:1:, :2:, :3:):"

    assert Macro.to_string(quote(do: Bar.foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
           "::Bar:.foo(:1:, :2:, :3:):"
  end

  ## validate

  test "validate" do
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

  test "decompose call" do
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

  test "env stacktrace" do
    env = %{__ENV__ | file: "foo", line: 12}
    assert Macro.Env.stacktrace(env) ==
           [{__MODULE__, :"test env stacktrace", 1, [file: "foo", line: 12]}]

    env = %{env | function: nil}
    assert Macro.Env.stacktrace(env) ==
           [{__MODULE__, :__MODULE__, 0, [file: "foo", line: 12]}]

    env = %{env | module: nil}
    assert Macro.Env.stacktrace(env) ==
           [{:elixir_compiler, :__FILE__, 1, [file: "foo", line: 12]}]
  end

  test "context modules" do
    defmodule Foo.Bar do
      assert __MODULE__ in __ENV__.context_modules
    end
  end

  ## pipe/unpipe

  test "pipe" do
    assert Macro.pipe(1, quote(do: foo), 0) == quote(do: foo(1))
    assert Macro.pipe(1, quote(do: foo(2)), 0) == quote(do: foo(1, 2))
    assert Macro.pipe(1, quote(do: foo), -1) == quote(do: foo(1))
    assert Macro.pipe(2, quote(do: foo(1)), -1) == quote(do: foo(1, 2))

    assert_raise ArgumentError, ~r"cannot pipe 1 into 2", fn ->
      Macro.pipe(1, 2, 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into {:ok}", fn ->
      Macro.pipe(1, {:ok}, 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into 1 \+ 1", fn ->
      Macro.pipe(1, quote(do: 1 + 1), 0) == quote(do: foo(1))
    end

    # TODO: restore this test when we drop unary operator support in pipes
    # assert_raise ArgumentError, ~r"cannot pipe 1 into \+1", fn ->
    #   Macro.pipe(1, quote(do: + 1), 0)
    # end

    assert_raise ArgumentError, ~r"cannot pipe Macro into Env", fn ->
      Macro.pipe(Macro, quote(do: Env), 0)
    end

    message = ~r"cannot pipe :foo into an anonymous function without calling"
    assert_raise ArgumentError, message, fn ->
      Macro.pipe(:foo, quote(do: fn x -> x end), 0)
    end
  end

  test "unpipe" do
    assert Macro.unpipe(quote(do: foo)) == quote(do: [{foo, 0}])
    assert Macro.unpipe(quote(do: foo |> bar)) == quote(do: [{foo, 0}, {bar, 0}])
    assert Macro.unpipe(quote(do: foo |> bar |> baz)) == quote(do: [{foo, 0}, {bar, 0}, {baz, 0}])
  end

  ## traverse/pre/postwalk

  test "traverse" do
    assert traverse({:foo, [], nil}) ==
           [{:foo, [], nil}, {:foo, [], nil}]

    assert traverse({:foo, [], [1, 2, 3]}) ==
           [{:foo, [], [1, 2, 3]}, 1, 1, 2, 2, 3, 3, {:foo, [], [1, 2, 3]}]

    assert traverse({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
           [{{:., [], [:foo, :bar]}, [], [1, 2, 3]}, {:., [], [:foo, :bar]}, :foo, :foo, :bar, :bar, {:., [], [:foo, :bar]},
            1, 1, 2, 2, 3, 3, {{:., [], [:foo, :bar]}, [], [1, 2, 3]}]

    assert traverse({[1, 2, 3], [4, 5, 6]}) ==
           [{[1, 2, 3], [4, 5, 6]}, [1, 2, 3], 1, 1, 2, 2, 3, 3, [1, 2, 3],
            [4, 5, 6], 4, 4, 5, 5, 6, 6, [4, 5, 6], {[1, 2, 3], [4, 5, 6]}]
  end

  defp traverse(ast) do
    Macro.traverse(ast, [], &{&1, [&1 | &2]}, &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse
  end

  test "prewalk" do
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
    Macro.prewalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse
  end

  test "postwalk" do
    assert postwalk({:foo, [], nil}) ==
           [{:foo, [], nil}]

    assert postwalk({:foo, [], [1, 2, 3]}) ==
           [1, 2, 3, {:foo, [], [1, 2, 3]}]

    assert postwalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
           [:foo, :bar, {:., [], [:foo, :bar]}, 1, 2, 3, {{:., [], [:foo, :bar]}, [], [1, 2, 3]}]

    assert postwalk({[1, 2, 3], [4, 5, 6]}) ==
           [1, 2, 3, [1, 2, 3], 4, 5, 6, [4, 5, 6], {[1, 2, 3], [4, 5, 6]}]
  end

  test "generate_arguments/2" do
    [] = Macro.generate_arguments(0, __MODULE__)
    [{:var1, [] , __MODULE__}] = Macro.generate_arguments(1, __MODULE__)
    assert Macro.generate_arguments(4, __MODULE__) |> length == 4
  end

  defp postwalk(ast) do
    Macro.postwalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse
  end

  test "underscore" do
    assert Macro.underscore("foo") == "foo"
    assert Macro.underscore("foo_bar") == "foo_bar"
    assert Macro.underscore("Foo") == "foo"
    assert Macro.underscore("FooBar") == "foo_bar"
    assert Macro.underscore("FOOBar") == "foo_bar"
    assert Macro.underscore("FooBAR") == "foo_bar"
    assert Macro.underscore("FOO_BAR") == "foo_bar"
    assert Macro.underscore("FoBaZa") == "fo_ba_za"
    assert Macro.underscore("Foo10") == "foo10"
    assert Macro.underscore("10Foo") == "10_foo"
    assert Macro.underscore("FooBar10") == "foo_bar10"
    assert Macro.underscore("Foo10Bar") == "foo10_bar"
    assert Macro.underscore("Foo.Bar") == "foo/bar"
    assert Macro.underscore(Foo.Bar) == "foo/bar"
    assert Macro.underscore("API.V1.User") == "api/v1/user"
    assert Macro.underscore("") == ""
  end

  test "camelize" do
    assert Macro.camelize("Foo") == "Foo"
    assert Macro.camelize("FooBar") == "FooBar"
    assert Macro.camelize("foo") == "Foo"
    assert Macro.camelize("foo_bar") == "FooBar"
    assert Macro.camelize("FOO_BAR") == "FooBar"
    assert Macro.camelize("foo_") == "Foo"
    assert Macro.camelize("_foo") == "Foo"
    assert Macro.camelize("foo10") == "Foo10"
    assert Macro.camelize("_10foo") == "10foo"
    assert Macro.camelize("foo_10") == "Foo10"
    assert Macro.camelize("foo__10") == "Foo10"
    assert Macro.camelize("foo__bar") == "FooBar"
    assert Macro.camelize("foo/bar") == "Foo.Bar"
    assert Macro.camelize("") == ""
  end
end
