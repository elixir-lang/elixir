Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Normalizer.GeneralTest do
  use ExUnit.Case, async: true

  import CodeNormalizerHelpers

  defp quoted_to_string(quoted, opts \\ []) do
    doc = Code.quoted_to_algebra(quoted, opts)

    Inspect.Algebra.format(doc, 98)
    |> IO.iodata_to_binary()
  end

  describe "quoted_to_algebra/2" do
    test "variable" do
      assert quoted_to_string(quote(do: foo)) == "foo"
    end

    test "local call" do
      assert quoted_to_string(quote(do: foo(1, 2, 3))) == "foo(1, 2, 3)"
      assert quoted_to_string(quote(do: foo([1, 2, 3]))) == "foo([1, 2, 3])"
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
      assert quoted_to_string(quote(do: Foo.bar(<<>>, []))) == "Foo.bar(<<>>, [])"
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
      assert quoted_to_string(quote(do: (() -> x))) == "(() -> x)"
      assert quoted_to_string(quote(do: (x when y -> z))) == "(x when y -> z)"
      assert quoted_to_string(quote(do: (x, y when z -> w))) == "(x, y when z -> w)"
      assert quoted_to_string(quote(do: (x, y when z -> w))) == "(x, y when z -> w)"
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
    end

    test "interpolation" do
      assert quoted_to_string(quote(do: "foo#{bar}baz")) == ~S["foo#{bar}baz"]
    end

    test "atom with interpolation" do
      assert quoted_to_string(quote(do: :"foo#{bar}baz")) == ~S[:"foo#{bar}baz"]
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

    test "charlist" do
      assert quoted_to_string(quote(do: [])) == "[]"
      assert quoted_to_string(quote(do: 'abc')) == "'abc'"
    end

    test "string" do
      assert quoted_to_string(quote(do: "")) == ~S/""/
      assert quoted_to_string(quote(do: "abc")) == ~S/"abc"/
      assert quoted_to_string(quote(do: "#{"abc"}")) == ~S/"#{"abc"}"/
    end

    test "last arg keyword list" do
      assert quoted_to_string(quote(do: foo([]))) == "foo([])"
      assert quoted_to_string(quote(do: foo(x: y))) == "foo(x: y)"
      assert quoted_to_string(quote(do: foo(x: 1 + 2))) == "foo(x: 1 + 2)"
      assert quoted_to_string(quote(do: foo(x: y, p: q))) == "foo(x: y, p: q)"
      assert quoted_to_string(quote(do: foo(a, x: y, p: q))) == "foo(a, x: y, p: q)"

      assert quoted_to_string(quote(do: {[]})) == "{[]}"
      assert quoted_to_string(quote(do: {[a: b]})) == "{[a: b]}"
      assert quoted_to_string(quote(do: {x, a: b})) == "{x, [a: b]}"
      assert quoted_to_string(quote(do: foo(else: a))) == "foo(else: a)"
      assert quoted_to_string(quote(do: foo(catch: a))) == "foo(catch: a)"
    end
  end

  describe "preserves formatting for sigils" do
    test "without interpolation" do
      assert_same ~S[~s(foo)]
      assert_same ~S[~s{foo bar}]
      assert_same ~S[~r/Bar Baz/]
      assert_same ~S[~w<>]
      assert_same ~S[~W()]
    end

    test "with escapes" do
      assert_same ~S[~s(foo \) bar)]
      assert_same ~S[~s(f\a\b\ro)]

      assert_same ~S"""
      ~S(foo\
      bar)
      """
    end

    test "with nested new lines" do
      assert_same ~S"""
      foo do
        ~S(foo\
      bar)
      end
      """

      assert_same ~S"""
      foo do
        ~s(#{bar}
      )
      end
      """
    end

    test "with interpolation" do
      assert_same ~S[~s(one #{2} three)]
    end

    test "with modifiers" do
      assert_same ~S[~w(one two three)a]
      assert_same ~S[~z(one two three)foo]
    end

    test "with heredoc syntax" do
      assert_same ~S"""
      ~s'''
      one\a
      #{:two}\r
      three\0
      '''
      """

      assert_same ~S'''
      ~s"""
      one\a
      #{:two}\r
      three\0
      """
      '''
    end

    test "with heredoc syntax and modifier" do
      assert_same ~S"""
      ~s'''
      foo
      '''rsa
      """
    end
  end

  describe "quoted_to_algebra/2 escapes" do
    test "strings" do
      assert quoted_to_string(quote(do: "\a\b\d\e\f\n\r\t\v"), escape: false) ==
               ~s|"\a\b\d\e\f\n\r\t\v"|

      assert quoted_to_string(quote(do: "\a\b\d\e\f\n\r\t\v")) ==
               ~s|"\\a\\b\\d\\e\\f\\n\\r\\t\\v"|

      assert quoted_to_string(quote(do: "\x00\x01\x10"), escape: false) == ~s|"\0\x01\x10"|
      assert quoted_to_string(quote(do: "\x00\x01\x10")) == ~s|"\\0\\x01\\x10"|
    end
  end
end
