Code.require_file("test_helper.exs", __DIR__)

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
    quote(do: unquote(left) || unquote(right))
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true
  doctest Macro

  # Changing the lines above will make compilation
  # fail since we are asserting on the caller lines
  import Macro.ExternalTest

  describe "escape/2" do
    test "returns tuples with size equal to two" do
      assert Macro.escape({:a, :b}) == {:a, :b}
    end

    test "returns lists" do
      assert Macro.escape([1, 2, 3]) == [1, 2, 3]
    end

    test "escapes tuples with size different than two" do
      assert Macro.escape({:a}) == {:{}, [], [:a]}
      assert Macro.escape({:a, :b, :c}) == {:{}, [], [:a, :b, :c]}
      assert Macro.escape({:a, {1, 2, 3}, :c}) == {:{}, [], [:a, {:{}, [], [1, 2, 3]}, :c]}
    end

    test "escapes maps" do
      assert Macro.escape(%{a: 1}) == {:%{}, [], [a: 1]}
    end

    test "escapes bitstring" do
      assert {:<<>>, [], args} = Macro.escape(<<300::12>>)
      assert [{:::, [], [1, {:size, [], [4]}]}, {:::, [], [",", {:binary, [], []}]}] = args
    end

    test "escapes recursively" do
      assert Macro.escape([1, {:a, :b, :c}, 3]) == [1, {:{}, [], [:a, :b, :c]}, 3]
    end

    test "escapes improper lists" do
      assert Macro.escape([1 | 2]) == [{:|, [], [1, 2]}]
      assert Macro.escape([1, 2 | 3]) == [1, {:|, [], [2, 3]}]
    end

    test "prunes metadata" do
      meta = [nothing: :important, counter: 1]
      assert Macro.escape({:foo, meta, []}) == {:{}, [], [:foo, meta, []]}
      assert Macro.escape({:foo, meta, []}, prune_metadata: true) == {:{}, [], [:foo, [], []]}
    end

    test "with unquote" do
      contents = quote(unquote: false, do: unquote(1))
      assert Macro.escape(contents, unquote: true) == 1

      contents = quote(unquote: false, do: unquote(x))
      assert Macro.escape(contents, unquote: true) == {:x, [], MacroTest}
    end

    defp eval_escaped(contents) do
      {eval, []} = Code.eval_quoted(Macro.escape(contents, unquote: true))
      eval
    end

    test "with remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:is_atom)(:ok))
      assert eval_escaped(contents) == quote(do: Kernel.is_atom(:ok))
    end

    test "with nested unquote" do
      contents =
        quote do
          quote(do: unquote(x))
        end

      assert eval_escaped(contents) == quote(do: quote(do: unquote(x)))
    end

    test "with alias or no arguments remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:self))
      assert eval_escaped(contents) == quote(do: Kernel.self())

      contents = quote(unquote: false, do: x.unquote(Foo))
      assert eval_escaped(contents) == quote(do: x.unquote(Foo))
    end

    test "with splicing" do
      contents = quote(unquote: false, do: [1, 2, 3, 4, 5])
      assert Macro.escape(contents, unquote: true) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [1, 2, unquote_splicing([3, 4, 5])])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [unquote_splicing([1, 2, 3]), 4, 5])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [unquote_splicing([1, 2, 3]), unquote_splicing([4, 5])])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]), 5])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]) | [5]])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]
    end

    test "does not add context to quote" do
      assert Macro.escape({:quote, [], [[do: :foo]]}) == {:{}, [], [:quote, [], [[do: :foo]]]}
    end
  end

  describe "expand_once/2" do
    test "with external macro" do
      assert {:||, _, [1, false]} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
    end

    test "with raw atom" do
      assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
    end

    test "with current module" do
      assert Macro.expand_once(quote(do: __MODULE__), __ENV__) == __MODULE__
    end

    test "with main" do
      assert Macro.expand_once(quote(do: Elixir), __ENV__) == Elixir
    end

    test "with simple alias" do
      assert Macro.expand_once(quote(do: Foo), __ENV__) == Foo
    end

    test "with current module plus alias" do
      assert Macro.expand_once(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
    end

    test "with main plus alias" do
      assert Macro.expand_once(quote(do: Elixir.Foo), __ENV__) == Foo
    end

    test "with custom alias" do
      alias Foo, as: Bar
      assert Macro.expand_once(quote(do: Bar.Baz), __ENV__) == Foo.Baz
    end

    test "with main plus custom alias" do
      alias Foo, as: Bar, warn: false
      assert Macro.expand_once(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
    end

    test "with call in alias" do
      assert Macro.expand_once(quote(do: Foo.bar().Baz), __ENV__) == quote(do: Foo.bar().Baz)
    end

    test "env" do
      env = %{__ENV__ | line: 0}
      assert Macro.expand_once(quote(do: __ENV__), env) == {:%{}, [], Map.to_list(env)}
      assert Macro.expand_once(quote(do: __ENV__.file), env) == env.file
      assert Macro.expand_once(quote(do: __ENV__.unknown), env) == quote(do: __ENV__.unknown)
    end

    defmacro local_macro() do
      :local_macro
    end

    test "local macro" do
      assert Macro.expand_once(quote(do: local_macro), __ENV__) == :local_macro
    end

    test "checks vars" do
      local_macro = 1
      assert local_macro == 1
      expr = {:local_macro, [], nil}
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    defp expand_once_and_clean(quoted, env) do
      cleaner = &Keyword.drop(&1, [:counter])

      quoted
      |> Macro.expand_once(env)
      |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
    end

    test "with imported macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: 1 || false), __ENV__) == quoted
    end

    test "with require macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: Kernel.||(1, false)), __ENV__) == quoted
    end

    test "with not expandable expression" do
      expr = quote(do: other(1, 2, 3))
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    test "does not expand module attributes" do
      message =
        "could not call Module.get_attribute/2 because the module #{inspect(__MODULE__)} " <>
          "is already compiled. Use the Module.__info__/1 callback or Code.fetch_docs/1 instead"

      assert_raise ArgumentError, message, fn ->
        Macro.expand_once(quote(do: @foo), __ENV__)
      end
    end
  end

  defp expand_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])

    quoted
    |> Macro.expand(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test "expand/2" do
    temp_var = {:x, [], Kernel}

    quoted =
      quote context: Kernel do
        case 1 do
          unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
          unquote(temp_var) -> unquote(temp_var)
        end
      end

    assert expand_and_clean(quote(do: oror(1, false)), __ENV__) == quoted
  end

  test "var/2" do
    assert Macro.var(:foo, nil) == {:foo, [], nil}
    assert Macro.var(:foo, Other) == {:foo, [], Other}
  end

  describe "to_string/1" do
    test "variable" do
      assert Macro.to_string(quote(do: foo)) == "foo"
    end

    test "local call" do
      assert Macro.to_string(quote(do: foo(1, 2, 3))) == "foo(1, 2, 3)"
      assert Macro.to_string(quote(do: foo([1, 2, 3]))) == "foo([1, 2, 3])"
    end

    test "remote call" do
      assert Macro.to_string(quote(do: foo.bar(1, 2, 3))) == "foo.bar(1, 2, 3)"
      assert Macro.to_string(quote(do: foo.bar([1, 2, 3]))) == "foo.bar([1, 2, 3])"

      quoted =
        quote do
          (foo do
             :ok
           end).bar([1, 2, 3])
        end

      assert Macro.to_string(quoted) == "(foo do\n  :ok\nend).bar([1, 2, 3])"
    end

    test "atom remote call" do
      assert Macro.to_string(quote(do: :foo.bar(1, 2, 3))) == ":foo.bar(1, 2, 3)"
    end

    test "remote and fun call" do
      assert Macro.to_string(quote(do: foo.bar.(1, 2, 3))) == "foo.bar().(1, 2, 3)"
      assert Macro.to_string(quote(do: foo.bar.([1, 2, 3]))) == "foo.bar().([1, 2, 3])"
    end

    test "unusual remote atom fun call" do
      assert Macro.to_string(quote(do: Foo."42"())) == ~s/Foo."42"()/
      assert Macro.to_string(quote(do: Foo."Bar"())) == ~s/Foo."Bar"()/
      assert Macro.to_string(quote(do: Foo."bar baz"()."")) == ~s/Foo."bar baz"().""()/
      assert Macro.to_string(quote(do: Foo."%{}"())) == ~s/Foo."%{}"()/
      assert Macro.to_string(quote(do: Foo."..."())) == ~s/Foo."..."()/
    end

    test "atom fun call" do
      assert Macro.to_string(quote(do: :foo.(1, 2, 3))) == ":foo.(1, 2, 3)"
    end

    test "aliases call" do
      assert Macro.to_string(quote(do: Foo.Bar.baz(1, 2, 3))) == "Foo.Bar.baz(1, 2, 3)"
      assert Macro.to_string(quote(do: Foo.Bar.baz([1, 2, 3]))) == "Foo.Bar.baz([1, 2, 3])"
      assert Macro.to_string(quote(do: Foo.bar(<<>>, []))) == "Foo.bar(<<>>, [])"
    end

    test "keyword call" do
      assert Macro.to_string(quote(do: Foo.bar(foo: :bar))) == "Foo.bar(foo: :bar)"
      assert Macro.to_string(quote(do: Foo.bar("Elixir.Foo": :bar))) == "Foo.bar([{Foo, :bar}])"
    end

    test "sigil call" do
      assert Macro.to_string(quote(do: ~r"123")) == ~S/~r"123"/
      assert Macro.to_string(quote(do: ~r"123"u)) == ~S/~r"123"u/
      assert Macro.to_string(quote(do: ~r"\n123")) == ~S/~r"\\n123"/

      assert Macro.to_string(quote(do: ~r"1#{two}3")) == ~S/~r"1#{two}3"/
      assert Macro.to_string(quote(do: ~r"1#{two}3"u)) == ~S/~r"1#{two}3"u/

      assert Macro.to_string(quote(do: ~R"123")) == ~S/~R"123"/
      assert Macro.to_string(quote(do: ~R"123"u)) == ~S/~R"123"u/
      assert Macro.to_string(quote(do: ~R"\n123")) == ~S/~R"\n123"/

      assert Macro.to_string(quote(do: ~S["'(123)'"])) == ~S/~S["'(123)'"]/

      assert Macro.to_string(
               quote do
                 ~S"""
                 "123"
                 """
               end
             ) == ~s[~S"""\n"123"\n"""]
    end

    test "tuple call" do
      assert Macro.to_string(quote(do: alias(Foo.{Bar, Baz, Bong}))) ==
               "alias(Foo.{Bar, Baz, Bong})"

      assert Macro.to_string(quote(do: foo(Foo.{}))) == "foo(Foo.{})"
    end

    test "arrow" do
      assert Macro.to_string(quote(do: foo(1, (2 -> 3)))) == "foo(1, (2 -> 3))"
    end

    test "block" do
      quoted =
        quote do
          1
          2

          (
            :foo
            :bar
          )

          3
        end

      expected = """
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

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "not in" do
      assert Macro.to_string(quote(do: false not in [])) == "false not in []"
    end

    test "if else" do
      expected = """
      if(foo) do
        bar
      else
        baz
      end
      """

      assert Macro.to_string(quote(do: if(foo, do: bar, else: baz))) <> "\n" == expected
    end

    test "case" do
      quoted =
        quote do
          case foo do
            true ->
              0

            false ->
              1
              2
          end
        end

      expected = """
      case(foo) do
        true ->
          0
        false ->
          1
          2
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "try" do
      quoted =
        quote do
          try do
            foo
          catch
            _, _ ->
              2
          rescue
            ArgumentError ->
              1
          after
            4
          else
            _ ->
              3
          end
        end

      expected = """
      try do
        foo
      rescue
        ArgumentError ->
          1
      catch
        _, _ ->
          2
      else
        _ ->
          3
      after
        4
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "fn" do
      assert Macro.to_string(quote(do: fn -> 1 + 2 end)) == "fn -> 1 + 2 end"
      assert Macro.to_string(quote(do: fn x -> x + 1 end)) == "fn x -> x + 1 end"

      quoted =
        quote do
          fn x ->
            y = x + 1
            y
          end
        end

      expected = """
      fn x ->
        y = x + 1
        y
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected

      quoted =
        quote do
          fn
            x ->
              y = x + 1
              y

            z ->
              z
          end
        end

      expected = """
      fn
        x ->
          y = x + 1
          y
        z ->
          z
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected

      assert Macro.to_string(quote(do: (fn x -> x end).(1))) == "(fn x -> x end).(1)"

      quoted =
        quote do
          (fn
             %{} -> :map
             _ -> :other
           end).(1)
        end

      expected = """
      (fn
        %{} ->
          :map
        _ ->
          :other
      end).(1)
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "range" do
      assert Macro.to_string(quote(do: unquote(-1..+2))) == "-1..2"
      assert Macro.to_string(quote(do: Foo.integer()..3)) == "Foo.integer()..3"
    end

    test "when" do
      assert Macro.to_string(quote(do: (() -> x))) == "(() -> x)"
      assert Macro.to_string(quote(do: (x when y -> z))) == "(x when y -> z)"
      assert Macro.to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
      assert Macro.to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
    end

    test "nested" do
      quoted =
        quote do
          defmodule Foo do
            def foo do
              1 + 1
            end
          end
        end

      expected = """
      defmodule(Foo) do
        def(foo) do
          1 + 1
        end
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "operator precedence" do
      assert Macro.to_string(quote(do: (1 + 2) * (3 - 4))) == "(1 + 2) * (3 - 4)"
      assert Macro.to_string(quote(do: (1 + 2) * 3 - 4)) == "(1 + 2) * 3 - 4"
      assert Macro.to_string(quote(do: 1 + 2 + 3)) == "1 + 2 + 3"
      assert Macro.to_string(quote(do: 1 + 2 - 3)) == "1 + 2 - 3"
    end

    test "capture operator" do
      assert Macro.to_string(quote(do: &foo/0)) == "&foo/0"
      assert Macro.to_string(quote(do: &Foo.foo/0)) == "&Foo.foo/0"
      assert Macro.to_string(quote(do: &(&1 + &2))) == "&(&1 + &2)"
      assert Macro.to_string(quote(do: & &1)) == "&(&1)"
      assert Macro.to_string(quote(do: & &1.(:x))) == "&(&1.(:x))"
      assert Macro.to_string(quote(do: (& &1).(:x))) == "(&(&1)).(:x)"
    end

    test "containers" do
      assert Macro.to_string(quote(do: {})) == "{}"
      assert Macro.to_string(quote(do: [])) == "[]"
      assert Macro.to_string(quote(do: {1, 2, 3})) == "{1, 2, 3}"
      assert Macro.to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
      assert Macro.to_string(quote(do: ["Elixir.Foo": :bar])) == "[{Foo, :bar}]"
      assert Macro.to_string(quote(do: %{})) == "%{}"
      assert Macro.to_string(quote(do: %{:foo => :bar})) == "%{foo: :bar}"
      assert Macro.to_string(quote(do: %{:"Elixir.Foo" => :bar})) == "%{Foo => :bar}"
      assert Macro.to_string(quote(do: %{{1, 2} => [1, 2, 3]})) == "%{{1, 2} => [1, 2, 3]}"
      assert Macro.to_string(quote(do: %{map | "a" => "b"})) == "%{map | \"a\" => \"b\"}"
      assert Macro.to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
    end

    test "struct" do
      assert Macro.to_string(quote(do: %Test{})) == "%Test{}"
      assert Macro.to_string(quote(do: %Test{foo: 1, bar: 1})) == "%Test{foo: 1, bar: 1}"
      assert Macro.to_string(quote(do: %Test{struct | foo: 2})) == "%Test{struct | foo: 2}"
      assert Macro.to_string(quote(do: %Test{} + 1)) == "%Test{} + 1"
      assert Macro.to_string(quote(do: %Test{foo(1)} + 2)) == "%Test{foo(1)} + 2"
    end

    test "binary operators" do
      assert Macro.to_string(quote(do: 1 + 2)) == "1 + 2"
      assert Macro.to_string(quote(do: [1, 2 | 3])) == "[1, 2 | 3]"
      assert Macro.to_string(quote(do: [h | t] = [1, 2, 3])) == "[h | t] = [1, 2, 3]"
      assert Macro.to_string(quote(do: (x ++ y) ++ z)) == "(x ++ y) ++ z"
    end

    test "unary operators" do
      assert Macro.to_string(quote(do: not 1)) == "not(1)"
      assert Macro.to_string(quote(do: not foo)) == "not(foo)"
      assert Macro.to_string(quote(do: -1)) == "-1"
      assert Macro.to_string(quote(do: +(+1))) == "+(+1)"
      assert Macro.to_string(quote(do: !(foo > bar))) == "!(foo > bar)"
      assert Macro.to_string(quote(do: @foo(bar))) == "@foo(bar)"
      assert Macro.to_string(quote(do: identity(&1))) == "identity(&1)"
    end

    test "access" do
      assert Macro.to_string(quote(do: a[b])) == "a[b]"
      assert Macro.to_string(quote(do: a[1 + 2])) == "a[1 + 2]"
      assert Macro.to_string(quote(do: (a || [a: 1])[:a])) == "(a || [a: 1])[:a]"
      assert Macro.to_string(quote(do: Map.put(%{}, :a, 1)[:a])) == "Map.put(%{}, :a, 1)[:a]"
    end

    test "keyword list" do
      assert Macro.to_string(quote(do: [a: a, b: b])) == "[a: a, b: b]"
      assert Macro.to_string(quote(do: [a: 1, b: 1 + 2])) == "[a: 1, b: 1 + 2]"
      assert Macro.to_string(quote(do: ["a.b": 1, c: 1 + 2])) == "[\"a.b\": 1, c: 1 + 2]"
    end

    test "interpolation" do
      assert Macro.to_string(quote(do: "foo#{bar}baz")) == ~S["foo#{bar}baz"]
    end

    test "bit syntax" do
      ast = quote(do: <<1::8*4>>)
      assert Macro.to_string(ast) == "<<1::8*4>>"

      ast = quote(do: @type(foo :: <<_::8, _::_*4>>))
      assert Macro.to_string(ast) == "@type(foo :: <<_::8, _::_*4>>)"

      ast = quote(do: <<69 - 4::bits-size(8 - 4)-unit(1), 65>>)
      assert Macro.to_string(ast) == "<<69 - 4::bits-size(8 - 4)-unit(1), 65>>"

      ast = quote(do: <<(<<65>>), 65>>)
      assert Macro.to_string(ast) == "<<(<<65>>), 65>>"

      ast = quote(do: <<65, (<<65>>)>>)
      assert Macro.to_string(ast) == "<<65, (<<65>>)>>"

      ast = quote(do: for(<<(a::4 <- <<1, 2>>)>>, do: a))
      assert Macro.to_string(ast) == "for(<<(a :: 4 <- <<1, 2>>)>>) do\n  a\nend"
    end

    test "charlist" do
      assert Macro.to_string(quote(do: [])) == "[]"
      assert Macro.to_string(quote(do: 'abc')) == "'abc'"
    end

    test "last arg keyword list" do
      assert Macro.to_string(quote(do: foo([]))) == "foo([])"
      assert Macro.to_string(quote(do: foo(x: y))) == "foo(x: y)"
      assert Macro.to_string(quote(do: foo(x: 1 + 2))) == "foo(x: 1 + 2)"
      assert Macro.to_string(quote(do: foo(x: y, p: q))) == "foo(x: y, p: q)"
      assert Macro.to_string(quote(do: foo(a, x: y, p: q))) == "foo(a, x: y, p: q)"

      assert Macro.to_string(quote(do: {[]})) == "{[]}"
      assert Macro.to_string(quote(do: {[a: b]})) == "{[a: b]}"
      assert Macro.to_string(quote(do: {x, a: b})) == "{x, [a: b]}"
      assert Macro.to_string(quote(do: foo(else: a))) == "foo(else: a)"
      assert Macro.to_string(quote(do: foo(catch: a))) == "foo(catch: a)"
    end

    test "with fun" do
      assert Macro.to_string(quote(do: foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
               ":foo(:1:, :2:, :3:):"

      assert Macro.to_string(quote(do: Bar.foo(1, 2, 3)), fn _, string -> ":#{string}:" end) ==
               "::Bar:.foo(:1:, :2:, :3:):"
    end
  end

  test "validate/1" do
    ref = make_ref()

    assert Macro.validate(1) == :ok
    assert Macro.validate(1.0) == :ok
    assert Macro.validate(:foo) == :ok
    assert Macro.validate("bar") == :ok
    assert Macro.validate(<<0::8>>) == :ok
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

  test "decompose_call/1" do
    assert Macro.decompose_call(quote(do: foo)) == {:foo, []}
    assert Macro.decompose_call(quote(do: foo())) == {:foo, []}
    assert Macro.decompose_call(quote(do: foo(1, 2, 3))) == {:foo, [1, 2, 3]}

    assert Macro.decompose_call(quote(do: M.N.foo(1, 2, 3))) ==
             {{:__aliases__, [alias: false], [:M, :N]}, :foo, [1, 2, 3]}

    assert Macro.decompose_call(quote(do: :foo.foo(1, 2, 3))) == {:foo, :foo, [1, 2, 3]}
    assert Macro.decompose_call(quote(do: 1.(1, 2, 3))) == :error
    assert Macro.decompose_call(quote(do: "some string")) == :error
  end

  describe "env" do
    test "stacktrace" do
      env = %{__ENV__ | file: "foo", line: 12}

      assert Macro.Env.stacktrace(env) ==
               [{__MODULE__, :"test env stacktrace", 1, [file: 'foo', line: 12]}]

      env = %{env | function: nil}
      assert Macro.Env.stacktrace(env) == [{__MODULE__, :__MODULE__, 0, [file: 'foo', line: 12]}]

      env = %{env | module: nil}

      assert Macro.Env.stacktrace(env) ==
               [{:elixir_compiler, :__FILE__, 1, [file: 'foo', line: 12]}]
    end

    test "context modules" do
      defmodule Foo.Bar do
        assert __MODULE__ in __ENV__.context_modules
      end
    end

    test "to_match/1" do
      quote = quote(do: x in [])

      assert {{:., _, [{:__aliases__, _, [Elixir, :Enum]}, :member?]}, _, _} =
               Macro.expand_once(quote, __ENV__)

      assert Macro.expand_once(quote, Macro.Env.to_match(__ENV__)) == false
    end
  end

  ## pipe/unpipe

  test "pipe/3" do
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

    assert_raise ArgumentError, ~r"cannot pipe 1 into <<1>>", fn ->
      Macro.pipe(1, quote(do: <<1>>), 0)
    end

    assert_raise ArgumentError, ~r"cannot pipe 1 into the special form unquote/1", fn ->
      Macro.pipe(1, quote(do: unquote()), 0)
    end

    # TODO: Restore this test when we drop unary operator support in pipes
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

  test "unpipe/1" do
    assert Macro.unpipe(quote(do: foo)) == quote(do: [{foo, 0}])
    assert Macro.unpipe(quote(do: foo |> bar)) == quote(do: [{foo, 0}, {bar, 0}])
    assert Macro.unpipe(quote(do: foo |> bar |> baz)) == quote(do: [{foo, 0}, {bar, 0}, {baz, 0}])
  end

  ## traverse/pre/postwalk

  test "traverse/4" do
    assert traverse({:foo, [], nil}) == [{:foo, [], nil}, {:foo, [], nil}]

    assert traverse({:foo, [], [1, 2, 3]}) ==
             [{:foo, [], [1, 2, 3]}, 1, 1, 2, 2, 3, 3, {:foo, [], [1, 2, 3]}]

    assert traverse({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]},
               {:., [], [:foo, :bar]},
               :foo,
               :foo,
               :bar,
               :bar,
               {:., [], [:foo, :bar]},
               1,
               1,
               2,
               2,
               3,
               3,
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]}
             ]

    assert traverse({[1, 2, 3], [4, 5, 6]}) ==
             [
               {[1, 2, 3], [4, 5, 6]},
               [1, 2, 3],
               1,
               1,
               2,
               2,
               3,
               3,
               [1, 2, 3],
               [4, 5, 6],
               4,
               4,
               5,
               5,
               6,
               6,
               [4, 5, 6],
               {[1, 2, 3], [4, 5, 6]}
             ]
  end

  defp traverse(ast) do
    Macro.traverse(ast, [], &{&1, [&1 | &2]}, &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "prewalk/3" do
    assert prewalk({:foo, [], nil}) == [{:foo, [], nil}]

    assert prewalk({:foo, [], [1, 2, 3]}) == [{:foo, [], [1, 2, 3]}, 1, 2, 3]

    assert prewalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]},
               {:., [], [:foo, :bar]},
               :foo,
               :bar,
               1,
               2,
               3
             ]

    assert prewalk({[1, 2, 3], [4, 5, 6]}) ==
             [{[1, 2, 3], [4, 5, 6]}, [1, 2, 3], 1, 2, 3, [4, 5, 6], 4, 5, 6]
  end

  defp prewalk(ast) do
    Macro.prewalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "postwalk/3" do
    assert postwalk({:foo, [], nil}) == [{:foo, [], nil}]

    assert postwalk({:foo, [], [1, 2, 3]}) == [1, 2, 3, {:foo, [], [1, 2, 3]}]

    assert postwalk({{:., [], [:foo, :bar]}, [], [1, 2, 3]}) ==
             [
               :foo,
               :bar,
               {:., [], [:foo, :bar]},
               1,
               2,
               3,
               {{:., [], [:foo, :bar]}, [], [1, 2, 3]}
             ]

    assert postwalk({[1, 2, 3], [4, 5, 6]}) ==
             [1, 2, 3, [1, 2, 3], 4, 5, 6, [4, 5, 6], {[1, 2, 3], [4, 5, 6]}]
  end

  test "generate_arguments/2" do
    assert Macro.generate_arguments(0, __MODULE__) == []
    assert Macro.generate_arguments(1, __MODULE__) == [{:arg1, [], __MODULE__}]
    assert Macro.generate_arguments(4, __MODULE__) |> length == 4
  end

  defp postwalk(ast) do
    Macro.postwalk(ast, [], &{&1, [&1 | &2]}) |> elem(1) |> Enum.reverse()
  end

  test "struct!/2 expands structs multiple levels deep" do
    defmodule StructBang do
      defstruct [:a, :b]

      assert Macro.struct!(StructBang, __ENV__) == %{__struct__: StructBang, a: nil, b: nil}

      def within_function do
        assert Macro.struct!(StructBang, __ENV__) == %{__struct__: StructBang, a: nil, b: nil}
      end

      defmodule Nested do
        assert Macro.struct!(StructBang, __ENV__) == %{__struct__: StructBang, a: nil, b: nil}
      end
    end

    assert Macro.struct!(StructBang, __ENV__) == %{__struct__: StructBang, a: nil, b: nil}
  end

  test "operator?/2" do
    assert Macro.operator?(:+, 2)
    assert Macro.operator?(:+, 1)
    refute Macro.operator?(:+, 0)
  end

  test "quoted_literal?/1" do
    assert Macro.quoted_literal?(quote(do: "foo"))
    assert Macro.quoted_literal?(quote(do: {"foo", 1}))
    refute Macro.quoted_literal?(quote(do: {"foo", var}))
  end

  test "underscore/1" do
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

  test "camelize/1" do
    assert Macro.camelize("Foo") == "Foo"
    assert Macro.camelize("FooBar") == "FooBar"
    assert Macro.camelize("foo") == "Foo"
    assert Macro.camelize("foo_bar") == "FooBar"
    assert Macro.camelize("foo_") == "Foo"
    assert Macro.camelize("_foo") == "Foo"
    assert Macro.camelize("foo10") == "Foo10"
    assert Macro.camelize("_10foo") == "10foo"
    assert Macro.camelize("foo_10") == "Foo10"
    assert Macro.camelize("foo__10") == "Foo10"
    assert Macro.camelize("foo__bar") == "FooBar"
    assert Macro.camelize("foo/bar") == "Foo.Bar"
    assert Macro.camelize("Foo.Bar") == "Foo.Bar"
    assert Macro.camelize("FOO_BAR") == "FOO_BAR"
    assert Macro.camelize("FOO.BAR") == "FOO.BAR"
    assert Macro.camelize("") == ""
  end
end
