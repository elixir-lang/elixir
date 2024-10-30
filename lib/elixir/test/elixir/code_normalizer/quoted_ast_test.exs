Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Normalizer.QuotedASTTest do
  use ExUnit.Case, async: true

  describe "quoted_to_algebra/2" do
    test "variable" do
      assert quoted_to_string(quote(do: foo)) == "foo"
      assert quoted_to_string({:{}, [], nil}) == "{}"
    end

    test "variable with colors" do
      opts = [syntax_colors: [variable: :blue]]
      assert quoted_to_string(quote(do: foo), opts) == "\e[34mfoo\e[0m"
    end

    test "local call" do
      assert quoted_to_string(quote(do: foo(1, 2, 3))) == "foo(1, 2, 3)"
      assert quoted_to_string(quote(do: foo([1, 2, 3]))) == "foo([1, 2, 3])"

      assert quoted_to_string(quote(do: foo(1, 2, 3)), locals_without_parens: [foo: 3]) ==
               "foo 1, 2, 3"

      # Mixing literals and non-literals
      assert quoted_to_string(quote(do: foo(a, 2))) == "foo(a, 2)"
      assert quoted_to_string(quote(do: foo(1, b))) == "foo(1, b)"

      # Mixing literals and non-literals with line
      assert quoted_to_string(quote(line: __ENV__.line, do: foo(a, 2))) == "foo(a, 2)"
      assert quoted_to_string(quote(line: __ENV__.line, do: foo(1, b))) == "foo(1, b)"
    end

    test "local call with colors" do
      opts = [syntax_colors: [call: :blue, number: :yellow, variable: :red]]

      assert quoted_to_string(quote(do: foo(1, a)), opts) ==
               "\e[34mfoo\e[0m(\e[33m1\e[0m, \e[31ma\e[0m)"
    end

    test "local call no parens" do
      assert quoted_to_string({:def, [], [1, 2]}) == "def 1, 2"
      assert quoted_to_string({:def, [closing: []], [1, 2]}) == "def(1, 2)"
    end

    test "remote call" do
      assert quoted_to_string(quote(do: foo.bar(1, 2, 3))) == "foo.bar(1, 2, 3)"
      assert quoted_to_string(quote(do: foo.bar([1, 2, 3]))) == "foo.bar([1, 2, 3])"

      quoted =
        quote do
          (foo do
             :ok
           end).bar([1, 2, 3])
        end

      assert quoted_to_string(quoted) == "(foo do\n   :ok\n end).bar([1, 2, 3])"
    end

    test "nullary remote call" do
      assert quoted_to_string(quote do: foo.bar) == "foo.bar"
      assert quoted_to_string(quote do: foo.bar()) == "foo.bar()"
    end

    test "atom remote call" do
      assert quoted_to_string(quote(do: :foo.bar(1, 2, 3))) == ":foo.bar(1, 2, 3)"
    end

    test "remote and fun call" do
      assert quoted_to_string(quote(do: foo.bar().(1, 2, 3))) == "foo.bar().(1, 2, 3)"
      assert quoted_to_string(quote(do: foo.bar().([1, 2, 3]))) == "foo.bar().([1, 2, 3])"
    end

    test "unusual remote atom fun call" do
      assert quoted_to_string(quote(do: Foo."42"())) == ~s/Foo."42"()/
      assert quoted_to_string(quote(do: Foo."Bar"())) == ~s/Foo."Bar"()/
      assert quoted_to_string(quote(do: Foo."bar baz"().""())) == ~s/Foo."bar baz"().""()/
      assert quoted_to_string(quote(do: Foo."%{}"())) == ~s/Foo."%{}"()/
      assert quoted_to_string(quote(do: Foo."..."())) == ~s/Foo."..."()/
    end

    test "atom fun call" do
      assert quoted_to_string(quote(do: :foo.(1, 2, 3))) == ":foo.(1, 2, 3)"
    end

    test "aliases call" do
      assert quoted_to_string(quote(do: Foo.Bar.baz(1, 2, 3))) == "Foo.Bar.baz(1, 2, 3)"
      assert quoted_to_string(quote(do: Foo.Bar.baz([1, 2, 3]))) == "Foo.Bar.baz([1, 2, 3])"
      assert quoted_to_string(quote(do: ?0.Bar.baz([1, 2, 3]))) == "48.Bar.baz([1, 2, 3])"
      assert quoted_to_string(quote(do: Foo.bar(<<>>, []))) == "Foo.bar(<<>>, [])"
    end

    test "remote call with colors" do
      opts = [syntax_colors: [call: :blue, number: :yellow, variable: :red, atom: :green]]

      assert quoted_to_string(quote(do: foo.bar(1, 2)), opts) ==
               "\e[31mfoo\e[0m.\e[34mbar\e[0m(\e[33m1\e[0m, \e[33m2\e[0m)"

      assert quoted_to_string(quote(do: :foo.bar(1, 2)), opts) ==
               "\e[32m:foo\e[0m.\e[34mbar\e[0m(\e[33m1\e[0m, \e[33m2\e[0m)"

      assert quoted_to_string(quote(do: Foo.Bar.bar(1, 2)), opts) ==
               "\e[32mFoo.Bar\e[0m.\e[34mbar\e[0m(\e[33m1\e[0m, \e[33m2\e[0m)"
    end

    test "keyword call" do
      assert quoted_to_string(quote(do: Foo.bar(foo: :bar))) == "Foo.bar(foo: :bar)"
      assert quoted_to_string(quote(do: Foo.bar("Elixir.Foo": :bar))) == "Foo.bar([{Foo, :bar}])"
    end

    test "sigil call" do
      assert quoted_to_string(quote(do: ~r"123")) == ~S/~r"123"/
      assert quoted_to_string(quote(do: ~r"\n123")) == ~S/~r"\n123"/
      assert quoted_to_string(quote(do: ~r"12\"3")) == ~S/~r"12\"3"/
      assert quoted_to_string(quote(do: ~r/12\/3/u)) == ~S"~r/12\/3/u"
      assert quoted_to_string(quote(do: ~r{\n123})) == ~S/~r{\n123}/
      assert quoted_to_string(quote(do: ~r((1\)(2\)3))) == ~S/~r((1\)(2\)3)/
      assert quoted_to_string(quote(do: ~r{\n1{1\}23})) == ~S/~r{\n1{1\}23}/
      assert quoted_to_string(quote(do: ~r|12\|3|)) == ~S"~r|12\|3|"

      assert quoted_to_string(quote(do: ~r[1#{two}3])) == ~S/~r[1#{two}3]/
      assert quoted_to_string(quote(do: ~r|1[#{two}]3|)) == ~S/~r|1[#{two}]3|/
      assert quoted_to_string(quote(do: ~r'1#{two}3'u)) == ~S/~r'1#{two}3'u/

      assert quoted_to_string(quote(do: ~R"123")) == ~S/~R"123"/
      assert quoted_to_string(quote(do: ~R"123"u)) == ~S/~R"123"u/
      assert quoted_to_string(quote(do: ~R"\n123")) == ~S/~R"\n123"/

      assert quoted_to_string(quote(do: ~S["'(123)'"])) == ~S/~S["'(123)'"]/
      assert quoted_to_string(quote(do: ~s"#{"foo"}")) == ~S/~s"#{"foo"}"/

      assert quoted_to_string(quote(do: ~S["'(123)'"]) |> strip_metadata()) == ~S/~S"\"'(123)'\""/
      assert quoted_to_string(quote(do: ~s"#{"foo"}") |> strip_metadata()) == ~S/~s"#{"foo"}"/

      assert quoted_to_string(
               quote do
                 ~s"""
                 "\""foo"\""
                 """
               end
             ) == ~s[~s"""\n"\\""foo"\\""\n"""]

      assert quoted_to_string(
               quote do
                 ~s'''
                 '\''foo'\''
                 '''
               end
             ) == ~s[~s'''\n'\\''foo'\\''\n''']

      assert quoted_to_string(
               quote do
                 ~s"""
                 "\"foo\""
                 """
               end
             ) == ~s[~s"""\n"\\"foo\\""\n"""]

      assert quoted_to_string(
               quote do
                 ~s'''
                 '\"foo\"'
                 '''
               end
             ) == ~s[~s'''\n'\\"foo\\"'\n''']

      assert quoted_to_string(
               quote do
                 ~S"""
                 "123"
                 """
               end
             ) == ~s[~S"""\n"123"\n"""]
    end

    test "regression: invalid sigil calls" do
      assert quoted_to_string(quote do: sigil_r(<<"foo", 123>>, [])) ==
               "sigil_r(<<\"foo\", 123>>, [])"

      assert quoted_to_string(quote do: sigil_r(<<"foo">>, :invalid_modifiers)) ==
               "sigil_r(\"foo\", :invalid_modifiers)"

      assert quoted_to_string(quote do: sigil_r(<<"foo">>, [:invalid_modifier])) ==
               "sigil_r(\"foo\", [:invalid_modifier])"

      assert quoted_to_string(quote do: sigil_r(<<"foo">>, [])) == "~r\"foo\""
      assert quoted_to_string(quote do: sigil_r(<<"foo">>, [?a, ?b, ?c])) == "~r\"foo\"abc"
    end

    test "tuple" do
      assert quoted_to_string(quote do: {1, 2}) == "{1, 2}"
      assert quoted_to_string(quote do: {1}) == "{1}"
      assert quoted_to_string(quote do: {1, 2, 3}) == "{1, 2, 3}"
      assert quoted_to_string(quote do: {1, 2, 3, foo: :bar}) == "{1, 2, 3, foo: :bar}"
    end

    test "tuple with colors" do
      opts = [syntax_colors: [tuple: :blue, number: :yellow]]

      assert quoted_to_string(quote(do: {1, 2, 3}), opts) ==
               "\e[34m{\e[0m\e[33m1\e[0m, \e[33m2\e[0m, \e[33m3\e[0m\e[34m}\e[0m"
    end

    test "tuple call" do
      assert quoted_to_string(quote(do: alias(Foo.{Bar, Baz, Bong}))) ==
               "alias Foo.{Bar, Baz, Bong}"

      assert quoted_to_string(quote(do: foo(Foo.{}))) == "foo(Foo.{})"
    end

    test "arrow" do
      assert quoted_to_string(quote(do: foo(1, (2 -> 3)))) == "foo(1, (2 -> 3))"
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
      1
      2

      (
        :foo
        :bar
      )

      3
      """

      assert quoted_to_string(quoted) <> "\n" == expected
    end

    test "not in" do
      assert quoted_to_string(quote(do: false not in [])) == "false not in []"
    end

    test "if else" do
      expected = """
      if foo do
        bar
      else
        baz
      end
      """

      assert quoted_to_string(quote(do: if(foo, do: bar, else: baz))) <> "\n" == expected
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
      case foo do
        true ->
          0

        false ->
          1
          2
      end
      """

      assert quoted_to_string(quoted) <> "\n" == expected
    end

    test "case if else" do
      expected = """
      case (if foo do
              bar
            else
              baz
            end) do
      end
      """

      assert quoted_to_string(
               quote(
                 do:
                   case if(foo, do: bar, else: baz) do
                   end
               )
             ) <> "\n" == expected
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
      catch
        _, _ -> 2
      rescue
        ArgumentError -> 1
      after
        4
      else
        _ -> 3
      end
      """

      assert quoted_to_string(quoted) <> "\n" == expected
    end

    test "fn" do
      assert quoted_to_string(quote(do: fn -> 1 + 2 end)) == "fn -> 1 + 2 end"
      assert quoted_to_string(quote(do: fn x -> x + 1 end)) == "fn x -> x + 1 end"

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

      assert quoted_to_string(quoted) <> "\n" == expected

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

      assert quoted_to_string(quoted) <> "\n" == expected

      assert quoted_to_string(quote(do: (fn x -> x end).(1))) == "(fn x -> x end).(1)"

      quoted =
        quote do
          (fn
             %{} -> :map
             _ -> :other
           end).(1)
        end

      expected = """
      (fn
         %{} -> :map
         _ -> :other
       end).(1)
      """

      assert quoted_to_string(quoted) <> "\n" == expected
    end

    test "range" do
      assert quoted_to_string(quote(do: unquote(-1..+2))) == "-1..2"
      assert quoted_to_string(quote(do: Foo.integer()..3)) == "Foo.integer()..3"
      assert quoted_to_string(quote(do: unquote(-1..+2//-3))) == "-1..2//-3"

      assert quoted_to_string(quote(do: Foo.integer()..3//Bar.bat())) ==
               "Foo.integer()..3//Bar.bat()"
    end

    test "when" do
      assert quoted_to_string(quote(do: (-> x))) == "(-> x)"
      assert quoted_to_string(quote(do: (x when y -> z))) == "(x when y -> z)"
      assert quoted_to_string(quote(do: (x, y when z -> w))) == "(x, y when z -> w)"
      assert quoted_to_string(quote(do: (x, y when z -> w))) == "(x, y when z -> w)"
      assert quoted_to_string(quote(do: (x, y when z -> w))) == "(x, y when z -> w)"
      assert quoted_to_string(quote(do: (x when y: z))) == "x when y: z"
      assert quoted_to_string(quote(do: (x when y: z, z: w))) == "x when y: z, z: w"
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
      defmodule Foo do
        def foo do
          1 + 1
        end
      end
      """

      assert quoted_to_string(quoted) <> "\n" == expected
    end

    test "operator precedence" do
      assert quoted_to_string(quote(do: (1 + 2) * (3 - 4))) == "(1 + 2) * (3 - 4)"
      assert quoted_to_string(quote(do: (1 + 2) * 3 - 4)) == "(1 + 2) * 3 - 4"
      assert quoted_to_string(quote(do: 1 + 2 + 3)) == "1 + 2 + 3"
      assert quoted_to_string(quote(do: 1 + 2 - 3)) == "1 + 2 - 3"
    end

    test "capture operator" do
      assert quoted_to_string(quote(do: &foo/0)) == "&foo/0"
      assert quoted_to_string(quote(do: &Foo.foo/0)) == "&Foo.foo/0"
      assert quoted_to_string(quote(do: &(&1 + &2))) == "&(&1 + &2)"
      assert quoted_to_string(quote(do: & &1)) == "& &1"
      assert quoted_to_string(quote(do: & &1.(:x))) == "& &1.(:x)"
      assert quoted_to_string(quote(do: (& &1).(:x))) == "(& &1).(:x)"
    end

    test "operators" do
      assert quoted_to_string(quote(do: foo |> {1, 2})) == "foo |> {1, 2}"
      assert quoted_to_string(quote(do: foo |> {:map, arg})) == "foo |> {:map, arg}"
    end

    test "containers" do
      assert quoted_to_string(quote(do: {})) == "{}"
      assert quoted_to_string(quote(do: [])) == "[]"
      assert quoted_to_string(quote(do: {1, 2, 3})) == "{1, 2, 3}"
      assert quoted_to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
      assert quoted_to_string(quote(do: ["Elixir.Foo": :bar])) == "[{Foo, :bar}]"
      assert quoted_to_string(quote(do: %{})) == "%{}"
      assert quoted_to_string(quote(do: %{:foo => :bar})) == "%{foo: :bar}"
      assert quoted_to_string(quote(do: %{:"Elixir.Foo" => :bar})) == "%{Foo => :bar}"
      assert quoted_to_string(quote(do: %{{1, 2} => [1, 2, 3]})) == "%{{1, 2} => [1, 2, 3]}"
      assert quoted_to_string(quote(do: %{map | "a" => "b"})) == "%{map | \"a\" => \"b\"}"
      assert quoted_to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
    end

    test "false positive containers" do
      assert quoted_to_string({:%{}, [], nil}) == "%{}"
    end

    test "struct" do
      assert quoted_to_string(quote(do: %Test{})) == "%Test{}"
      assert quoted_to_string(quote(do: %Test{foo: 1, bar: 1})) == "%Test{foo: 1, bar: 1}"
      assert quoted_to_string(quote(do: %Test{struct | foo: 2})) == "%Test{struct | foo: 2}"
      assert quoted_to_string(quote(do: %Test{} + 1)) == "%Test{} + 1"
      assert quoted_to_string(quote(do: %Test{foo(1)} + 2)) == "%Test{foo(1)} + 2"
    end

    test "binary operators" do
      assert quoted_to_string(quote(do: 1 + 2)) == "1 + 2"
      assert quoted_to_string(quote(do: [1, 2 | 3])) == "[1, 2 | 3]"
      assert quoted_to_string(quote(do: [h | t] = [1, 2, 3])) == "[h | t] = [1, 2, 3]"
      assert quoted_to_string(quote(do: (x ++ y) ++ z)) == "(x ++ y) ++ z"
      assert quoted_to_string(quote(do: (x +++ y) +++ z)) == "(x +++ y) +++ z"
    end

    test "unary operators" do
      assert quoted_to_string(quote(do: not 1)) == "not 1"
      assert quoted_to_string(quote(do: not foo)) == "not foo"
      assert quoted_to_string(quote(do: -1)) == "-1"
      assert quoted_to_string(quote(do: +(+1))) == "+(+1)"
      assert quoted_to_string(quote(do: !(foo > bar))) == "!(foo > bar)"
      assert quoted_to_string(quote(do: @foo(bar))) == "@foo bar"
      assert quoted_to_string(quote(do: identity(&1))) == "identity(&1)"
    end

    test "operators with colors" do
      opts = [syntax_colors: [operator: :blue, number: :yellow]]
      assert quoted_to_string(quote(do: !!1), opts) == "\e[34m!\e[0m\e[34m!\e[0m\e[33m1\e[0m"
      assert quoted_to_string(quote(do: 1 + 2), opts) == "\e[33m1\e[0m\e[34m +\e[0m \e[33m2\e[0m"
    end

    test "access" do
      assert quoted_to_string(quote(do: a[b])) == "a[b]"
      assert quoted_to_string(quote(do: a[1 + 2])) == "a[1 + 2]"
      assert quoted_to_string(quote(do: (a || [a: 1])[:a])) == "(a || [a: 1])[:a]"
      assert quoted_to_string(quote(do: Map.put(%{}, :a, 1)[:a])) == "Map.put(%{}, :a, 1)[:a]"
    end

    test "keyword list" do
      assert quoted_to_string(quote(do: [a: a, b: b])) == "[a: a, b: b]"
      assert quoted_to_string(quote(do: [a: 1, b: 1 + 2])) == "[a: 1, b: 1 + 2]"
      assert quoted_to_string(quote(do: ["a.b": 1, c: 1 + 2])) == "[\"a.b\": 1, c: 1 + 2]"

      tuple = {{:__block__, [format: :keyword], [:a]}, {:b, [], nil}}
      assert quoted_to_string([tuple, :foo, tuple]) == "[{:a, b}, :foo, a: b]"
      assert quoted_to_string([tuple, :foo, {:c, :d}, tuple]) == "[{:a, b}, :foo, c: :d, a: b]"

      # Not keyword lists
      assert quoted_to_string(quote(do: [{binary(), integer()}])) == "[{binary(), integer()}]"
    end

    test "keyword list with colors" do
      opts = [syntax_colors: [list: :blue, atom: :green, number: :yellow]]

      assert quoted_to_string(quote(do: [a: 1, b: 2]), opts) ==
               "\e[34m[\e[0m\e[32ma:\e[0m \e[33m1\e[0m, \e[32mb:\e[0m \e[33m2\e[0m\e[34m]\e[0m"
    end

    test "keyword list with :do as operand" do
      assert quoted_to_string(quote(do: a = [do: 1])) == "a = [do: 1]"
    end

    test "interpolation" do
      assert quoted_to_string(quote(do: "foo#{bar}baz")) == ~S["foo#{bar}baz"]
    end

    test "bit syntax" do
      ast = quote(do: <<1::8*4>>)
      assert quoted_to_string(ast) == "<<1::8*4>>"

      ast = quote(do: @type(foo :: <<_::8, _::_*4>>))
      assert quoted_to_string(ast) == "@type foo :: <<_::8, _::_*4>>"

      ast = quote(do: <<69 - 4::bits-size(8 - 4)-unit(1), 65>>)
      assert quoted_to_string(ast) == "<<69 - 4::bits-size(8 - 4)-unit(1), 65>>"

      ast = quote(do: <<(<<65>>), 65>>)
      assert quoted_to_string(ast) == "<<(<<65>>), 65>>"

      ast = quote(do: <<65, (<<65>>)>>)
      assert quoted_to_string(ast) == "<<65, (<<65>>)>>"

      ast = quote(do: for(<<(a::4 <- <<1, 2>>)>>, do: a))
      assert quoted_to_string(ast) == "for <<(a::4 <- <<1, 2>>)>> do\n  a\nend"
    end

    test "integer/float" do
      assert quoted_to_string(1) == "1"
      assert quoted_to_string({:__block__, [], [1]}) == "1"
      assert quoted_to_string(1.23) == "1.23"
    end

    test "integer/float with colors" do
      opts = [syntax_colors: [number: :yellow]]
      assert quoted_to_string(1, opts) == "\e[33m1\e[0m"
      assert quoted_to_string(1.23, opts) == "\e[33m1.23\e[0m"
    end

    test "charlist" do
      assert quoted_to_string(quote(do: [])) == "[]"
      assert quoted_to_string(quote(do: ~c"abc")) == ~S/~c"abc"/

      # False positive
      assert quoted_to_string(
               quote do
                 :"Elixir.List".to_charlist([
                   case var do
                     var -> var
                   end
                 ])
               end
             ) =~ "List.to_charlist([\n  case var do\n    var -> var\n  end\n])"
    end

    test "string" do
      assert quoted_to_string(quote(do: "")) == ~S/""/
      assert quoted_to_string(quote(do: "abc")) == ~S/"abc"/
      assert quoted_to_string(quote(do: "#{"abc"}")) == ~S/"#{"abc"}"/
    end

    test "string with colors" do
      opts = [syntax_colors: [string: :green]]
      assert quoted_to_string(quote(do: "abc"), opts) == "\e[32m\"abc\"\e[0m"
    end

    test "catch-all" do
      assert quoted_to_string(quote do: {unquote(self())}) == "{#{inspect(self())}}"
      assert quoted_to_string(quote do: foo(unquote(self()))) == "foo(#{inspect(self())})"
    end

    test "last arg keyword list" do
      assert quoted_to_string(quote(do: foo([]))) == "foo([])"
      assert quoted_to_string(quote(do: foo(x: y))) == "foo(x: y)"
      assert quoted_to_string(quote(do: foo(x: 1 + 2))) == "foo(x: 1 + 2)"
      assert quoted_to_string(quote(do: foo(x: y, p: q))) == "foo(x: y, p: q)"
      assert quoted_to_string(quote(do: foo(a, x: y, p: q))) == "foo(a, x: y, p: q)"

      assert quoted_to_string(quote(do: {[]})) == "{[]}"
      assert quoted_to_string(quote(do: {[a: b]})) == "{[a: b]}"
      assert quoted_to_string(quote(do: {x, a: b})) == "{x, a: b}"
      assert quoted_to_string(quote(do: foo(else: a))) == "foo(else: a)"
      assert quoted_to_string(quote(do: foo(catch: a))) == "foo(catch: a)"
      assert quoted_to_string(quote(do: foo |> [bar: :baz])) == "foo |> [bar: :baz]"
    end

    test "keyword arg with cursor" do
      input = "def foo, do: :bar, __cursor__()"
      expected = "def foo, [{:do, :bar}, __cursor__()]"

      ast = Code.string_to_quoted!(input)
      assert quoted_to_string(ast) == expected

      encoder = &{:ok, {:__block__, &2, [&1]}}
      ast = Code.string_to_quoted!(input, literal_encoder: encoder)
      assert quoted_to_string(ast) == expected

      ast = Code.string_to_quoted!(input, token_metadata: true)
      assert quoted_to_string(ast) == expected

      ast = Code.string_to_quoted!(input, literal_encoder: encoder, token_metadata: true)
      assert quoted_to_string(ast) == expected
    end

    test "keyword arg with literal encoder and no metadata" do
      input = """
      foo(Bar) do
        :ok
      end
      """

      encoder = &{:ok, {:__block__, &2, [&1]}}
      ast = Code.string_to_quoted!(input, literal_encoder: encoder)
      assert quoted_to_string(ast) == "foo(Bar, do: :ok)"
    end

    test "list in module attribute" do
      assert quoted_to_string(
               quote do
                 @foo []
               end
             ) == "@foo []"

      assert quoted_to_string(
               quote do
                 @foo [1]
               end
             ) == "@foo [1]"

      assert quoted_to_string(
               quote do
                 @foo [foo: :bar]
               end
             ) == "@foo foo: :bar"

      assert quoted_to_string(
               quote do
                 @foo [1, foo: :bar]
               end
             ) == "@foo [1, foo: :bar]"
    end
  end

  describe "quoted_to_algebra/2 escapes" do
    test "strings with slash escapes" do
      assert quoted_to_string(quote(do: "\a\b\d\e\f\n\r\t\v"), escape: false) ==
               ~s/"\a\b\d\e\f\n\r\t\v"/

      assert quoted_to_string(quote(do: "\a\b\d\e\f\n\r\t\v")) ==
               ~s/"\\a\\b\\d\\e\\f\\n\\r\\t\\v"/

      assert quoted_to_string({:__block__, [], ["\a\b\d\e\f\n\r\t\v"]}, escape: false) ==
               ~s/"\a\b\d\e\f\n\r\t\v"/

      assert quoted_to_string({:__block__, [], ["\a\b\d\e\f\n\r\t\v"]}) ==
               ~s/"\\a\\b\\d\\e\\f\\n\\r\\t\\v"/
    end

    test "strings with non printable characters" do
      assert quoted_to_string(quote(do: "\x00\x01\x10"), escape: false) == ~s/"\x00\x01\x10"/
      assert quoted_to_string(quote(do: "\x00\x01\x10")) == ~S/"\0\x01\x10"/
    end

    test "charlists with slash escapes" do
      assert quoted_to_string(~c"\a\b\e\n\r\t\v", escape: false) ==
               ~s/~c"\a\b\e\n\r\t\v"/

      assert quoted_to_string(~c"\a\b\e\n\r\t\v") ==
               ~s/~c"\\a\\b\\e\\n\\r\\t\\v"/

      assert quoted_to_string({:__block__, [], [~c"\a\b\e\n\r\t\v"]}, escape: false) ==
               ~s/~c"\a\b\e\n\r\t\v"/

      assert quoted_to_string({:__block__, [], [~c"\a\b\e\n\r\t\v"]}) ==
               ~s/~c"\\a\\b\\e\\n\\r\\t\\v"/
    end

    test "charlists with non printable characters" do
      assert quoted_to_string(~c"\x00\x01\x10", escape: false) == ~S/[0, 1, 16]/
      assert quoted_to_string(~c"\x00\x01\x10") == ~S/[0, 1, 16]/
    end

    test "atoms" do
      assert quoted_to_string(quote(do: :"a\nb\tc"), escape: false) == ~s/:"a\nb\tc"/
      assert quoted_to_string(quote(do: :"a\nb\tc")) == ~S/:"a\nb\tc"/

      assert quoted_to_string({:__block__, [], [:"a\nb\tc"]}, escape: false) == ~s/:"a\nb\tc"/
      assert quoted_to_string({:__block__, [], [:"a\nb\tc"]}) == ~S/:"a\nb\tc"/

      assert quoted_to_string(quote(do: :"Elixir")) == "Elixir"
      assert quoted_to_string(quote(do: :"Elixir.Foo")) == "Foo"
      assert quoted_to_string(quote(do: :"Elixir.Foo.Bar")) == "Foo.Bar"
      assert quoted_to_string(quote(do: :"Elixir.foobar")) == ~S/:"Elixir.foobar"/
    end

    test "atoms with non printable characters" do
      assert quoted_to_string(quote(do: :"\x00\x01\x10"), escape: false) == ~s/:"\0\x01\x10"/
      assert quoted_to_string(quote(do: :"\x00\x01\x10")) == ~S/:"\0\x01\x10"/
    end

    test "atoms with interpolations" do
      assert quoted_to_string(quote(do: :"foo\n#{bar}\tbaz"), escape: false) ==
               ~s[:"foo\n\#{bar}\tbaz"]

      assert quoted_to_string(quote(do: :"foo\n#{bar}\tbaz")) == ~S[:"foo\n#{bar}\tbaz"]

      assert quoted_to_string(quote(do: :"foo\"bar"), escape: false) == ~S[:"foo\"bar"]
      assert quoted_to_string(quote(do: :"foo\"bar")) == ~S[:"foo\"bar"]

      assert quoted_to_string(quote(do: :"foo#{~s/\n/}bar"), escape: false) ==
               ~S[:"foo#{~s/\n/}bar"]

      assert quoted_to_string(quote(do: :"foo#{~s/\n/}bar")) == ~S[:"foo#{~s/\n/}bar"]

      assert quoted_to_string(quote(do: :"one\n\"#{2}\"\nthree"), escape: false) ==
               ~s[:"one\n\\"\#{2}\\"\nthree"]

      assert quoted_to_string(quote(do: :"one\n\"#{2}\"\nthree")) == ~S[:"one\n\"#{2}\"\nthree"]
    end

    test ":erlang.binary_to_atom/2 edge cases" do
      assert quoted_to_string(quote(do: :erlang.binary_to_atom(<<>>, :utf8))) == ~S[:""]

      assert quoted_to_string(quote(do: :erlang.binary_to_atom(<<1>>, :utf8))) ==
               ~S":erlang.binary_to_atom(<<1>>, :utf8)"
    end
  end

  describe "quoted_to_algebra/2 with invalid" do
    test "block" do
      assert quoted_to_string({:__block__, [], {:bar, [], []}}) ==
               "{:__block__, [], {:bar, [], []}}"

      assert quoted_to_string({:foo, [], [{:do, :ok}, :not_keyword]}) ==
               "foo({:do, :ok}, :not_keyword)"

      assert quoted_to_string({:foo, [], [[{:do, :ok}, :not_keyword]]}) ==
               "foo([{:do, :ok}, :not_keyword])"
    end

    test "ode" do
      assert quoted_to_string(1..3) == "1..3"
    end
  end

  describe "quoted_to_algebra/2 does not escape" do
    test "sigils" do
      assert quoted_to_string(quote(do: ~s/a\nb\tc/), escape: false) == ~S"~s/a\nb\tc/"
      assert quoted_to_string(quote(do: ~s/a\nb\tc/)) == ~S"~s/a\nb\tc/"

      assert quoted_to_string(quote(do: ~s/\a\b\d\e\f\n\r\t\v/), escape: false) ==
               ~S"~s/\a\b\d\e\f\n\r\t\v/"

      assert quoted_to_string(quote(do: ~s/\a\b\d\e\f\n\r\t\v/)) == ~S"~s/\a\b\d\e\f\n\r\t\v/"

      assert quoted_to_string(quote(do: ~s/\x00\x01\x10/), escape: false) == ~S"~s/\x00\x01\x10/"
      assert quoted_to_string(quote(do: ~s/\x00\x01\x10/)) == ~S"~s/\x00\x01\x10/"
    end
  end

  defp strip_metadata(ast) do
    Macro.prewalk(ast, &Macro.update_meta(&1, fn _ -> [] end))
  end

  defp quoted_to_string(quoted, opts \\ []) do
    doc = Code.quoted_to_algebra(quoted, opts)

    Inspect.Algebra.format(doc, 98)
    |> IO.iodata_to_binary()
  end
end
