Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ParserTest do
  use ExUnit.Case, async: true

  describe "unary ops" do
    test "in keywords" do
      assert parse!("f(!: :ok)") == {:f, [line: 1], [[!: :ok]]}
      assert parse!("f @: :ok") == {:f, [line: 1], [[@: :ok]]}
    end

    test "ambiguous ops in keywords" do
      assert parse!("f(+: :ok)") == {:f, [line: 1], [[+: :ok]]}
      assert parse!("f +: :ok") == {:f, [line: 1], [[+: :ok]]}
    end
  end

  describe "ternary ops" do
    test "root" do
      assert parse!("1..2//3") == {:"..//", [line: 1], [1, 2, 3]}
      assert parse!("(1..2)//3") == {:"..//", [line: 1], [1, 2, 3]}
    end

    test "with do-blocks" do
      assert parse!("foo do end..bar do end//baz do end") == {
               :"..//",
               [line: 1],
               [
                 {:foo, [line: 1], [[do: {:__block__, [], []}]]},
                 {:bar, [line: 1], [[do: {:__block__, [], []}]]},
                 {:baz, [line: 1], [[do: {:__block__, [], []}]]}
               ]
             }
    end

    test "with no parens" do
      assert parse!("1..foo do end//bar bat, baz") == {
               :"..//",
               [line: 1],
               [
                 1,
                 {:foo, [line: 1], [[do: {:__block__, [], []}]]},
                 {:bar, [line: 1], [{:bat, [line: 1], nil}, {:baz, [line: 1], nil}]}
               ]
             }
    end

    test "errors" do
      msg =
        ~r/the range step operator \(\/\/\) must immediately follow the range definition operator \(\.\.\)/

      assert_syntax_error(msg, "foo..bar baz//bat")
      assert_syntax_error(msg, "foo++bar//bat")
      assert_syntax_error(msg, "foo..(bar//bat)")
    end
  end

  describe "strings/sigils" do
    test "delimiter information for sigils is included" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: false)

      assert parse!("~r/foo/") ==
               {:sigil_r, [delimiter: "/", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      assert string_to_quoted.("~r[foo]") ==
               {:sigil_r, [delimiter: "[", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      assert string_to_quoted.("~r\"foo\"") ==
               {:sigil_r, [delimiter: "\"", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      meta = [delimiter: "\"\"\"", line: 1]
      args = {:sigil_S, meta, [{:<<>>, [indentation: 0, line: 1], ["sigil heredoc\n"]}, []]}
      assert string_to_quoted.("~S\"\"\"\nsigil heredoc\n\"\"\"") == args

      meta = [delimiter: "'''", line: 1]
      args = {:sigil_S, meta, [{:<<>>, [indentation: 0, line: 1], ["sigil heredoc\n"]}, []]}
      assert string_to_quoted.("~S'''\nsigil heredoc\n'''") == args
    end

    test "sigil newlines" do
      assert {:sigil_s, _, [{:<<>>, _, ["here\ndoc"]}, []]} =
               Code.string_to_quoted!(~s|~s"here\ndoc"|)

      assert {:sigil_s, _, [{:<<>>, _, ["here\r\ndoc"]}, []]} =
               Code.string_to_quoted!(~s|~s"here\r\ndoc"|)
    end

    test "string newlines" do
      assert Code.string_to_quoted!(~s|"here\ndoc"|) == "here\ndoc"
      assert Code.string_to_quoted!(~s|"here\r\ndoc"|) == "here\r\ndoc"
      assert Code.string_to_quoted!(~s|"here\\\ndoc"|) == "heredoc"
      assert Code.string_to_quoted!(~s|"here\\\r\ndoc"|) == "heredoc"
    end

    test "heredoc newlines" do
      assert Code.string_to_quoted!(~s|"""\nhere\ndoc\n"""|) == "here\ndoc\n"
      assert Code.string_to_quoted!(~s|"""\r\nhere\r\ndoc\r\n"""|) == "here\r\ndoc\r\n"
      assert Code.string_to_quoted!(~s|  """\n  here\n  doc\n  """|) == "here\ndoc\n"
      assert Code.string_to_quoted!(~s|  """\r\n  here\r\n  doc\r\n  """|) == "here\r\ndoc\r\n"
      assert Code.string_to_quoted!(~s|"""\nhere\\\ndoc\\\n"""|) == "heredoc"
      assert Code.string_to_quoted!(~s|"""\r\nhere\\\r\ndoc\\\r\n"""|) == "heredoc"
    end

    test "heredoc indentation" do
      meta = [delimiter: "'''", line: 1]
      args = {:sigil_S, meta, [{:<<>>, [indentation: 2, line: 1], ["  sigil heredoc\n"]}, []]}
      assert Code.string_to_quoted!("~S'''\n    sigil heredoc\n  '''") == args
    end
  end

  describe "string_to_quoted/2" do
    test "converts strings to quoted expressions" do
      assert Code.string_to_quoted("1 + 2") == {:ok, {:+, [line: 1], [1, 2]}}

      assert Code.string_to_quoted("a.1") ==
               {:error, {[line: 1, column: 3], "syntax error before: ", "\"1\""}}
    end
  end

  describe "string_to_quoted/2 and atom handling" do
    test "ensures :existing_atoms_only" do
      assert Code.string_to_quoted(":there_is_no_such_atom", existing_atoms_only: true) ==
               {:error,
                {[line: 1, column: 1], "unsafe atom does not exist: ", "there_is_no_such_atom"}}
    end

    test "encodes atoms" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "there_is_no_such_atom"
        assert meta[:line] == 1
        assert meta[:column] == 1
        assert meta[:file] == "nofile"
        {:ok, {:my, "atom", ref}}
      end

      assert {:ok, {:my, "atom", ^ref}} =
               Code.string_to_quoted(":there_is_no_such_atom", static_atoms_encoder: encoder)
    end

    test "encodes vars" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "there_is_no_such_var"
        assert meta[:line] == 1
        assert meta[:column] == 1
        assert meta[:file] == "nofile"
        {:ok, {:my, "atom", ref}}
      end

      assert {:ok, {{:my, "atom", ^ref}, [line: 1], nil}} =
               Code.string_to_quoted("there_is_no_such_var", static_atoms_encoder: encoder)
    end

    test "addresses ambiguities" do
      encoder = fn string, _meta -> {:ok, {:atom, string}} end

      # We check a=1 for precedence issues with a!=1, make sure it works
      assert Code.string_to_quoted!("a = 1", static_atoms_encoder: encoder)
      assert Code.string_to_quoted!("a=1", static_atoms_encoder: encoder)
    end

    test "does not encode keywords" do
      encoder = fn atom, _meta -> raise "shouldn't be invoked for #{atom}" end

      assert {:ok, {:fn, [line: 1], [{:->, [line: 1], [[1], 2]}]}} =
               Code.string_to_quoted("fn 1 -> 2 end", static_atoms_encoder: encoder)

      assert {:ok, {:or, [line: 1], [true, false]}} =
               Code.string_to_quoted("true or false", static_atoms_encoder: encoder)

      encoder = fn atom, _meta -> {:ok, {:encoded, atom}} end

      assert {:ok, [encoded: "true", encoded: "do", encoded: "and"]} =
               Code.string_to_quoted("[:true, :do, :and]", static_atoms_encoder: encoder)

      assert {:ok, [{{:encoded, "do"}, 1}, {{:encoded, "true"}, 2}, {{:encoded, "end"}, 3}]} =
               Code.string_to_quoted("[do: 1, true: 2, end: 3]", static_atoms_encoder: encoder)
    end

    test "returns errors on long atoms even when using static_atoms_encoder" do
      atom = String.duplicate("a", 256)

      encoder = fn atom, _meta -> {:ok, atom} end

      assert Code.string_to_quoted(atom, static_atoms_encoder: encoder) ==
               {:error,
                {[line: 1, column: 1], "atom length must be less than system limit: ", atom}}
    end

    test "may return errors" do
      encoder = fn _atom, _meta ->
        {:error, "Invalid atom name"}
      end

      assert {:error, {[line: 1, column: 1], "Invalid atom name: ", "there_is_no_such_atom"}} =
               Code.string_to_quoted(":there_is_no_such_atom", static_atoms_encoder: encoder)
    end

    test "may return tuples" do
      encoder = fn string, _metadata ->
        try do
          {:ok, String.to_existing_atom(string)}
        rescue
          ArgumentError ->
            {:ok, {:user_atom, string}}
        end
      end

      assert {:ok, {:try, _, [[do: {:test, _, [{{:user_atom, "atom_does_not_exist"}, _, []}]}]]}} =
               Code.string_to_quoted("try do: test(atom_does_not_exist())",
                 static_atoms_encoder: encoder
               )
    end
  end

  describe "string_to_quoted/2 with :columns" do
    test "includes column information" do
      string_to_quoted = &Code.string_to_quoted(&1, columns: true)
      assert string_to_quoted.("1 + 2") == {:ok, {:+, [line: 1, column: 3], [1, 2]}}

      foo = {:foo, [line: 1, column: 1], nil}
      bar = {:bar, [line: 1, column: 7], nil}
      assert string_to_quoted.("foo + bar") == {:ok, {:+, [line: 1, column: 5], [foo, bar]}}
    end
  end

  describe "string_to_quoted/2 with :token_metadata" do
    test "adds end_of_expression information to blocks" do
      file = """
      one();two()
      three()

      four()


      five()
      """

      args = [
        {:one,
         [
           end_of_expression: [newlines: 0, line: 1, column: 6],
           closing: [line: 1, column: 5],
           line: 1,
           column: 1
         ], []},
        {:two,
         [
           end_of_expression: [newlines: 1, line: 1, column: 12],
           closing: [line: 1, column: 11],
           line: 1,
           column: 7
         ], []},
        {:three,
         [
           end_of_expression: [newlines: 2, line: 2, column: 8],
           closing: [line: 2, column: 7],
           line: 2,
           column: 1
         ], []},
        {:four,
         [
           end_of_expression: [newlines: 3, line: 4, column: 7],
           closing: [line: 4, column: 6],
           line: 4,
           column: 1
         ], []},
        {:five, [closing: [line: 7, column: 6], line: 7, column: 1], []}
      ]

      assert Code.string_to_quoted!(file, token_metadata: true, columns: true) ==
               {:__block__, [], args}
    end

    test "adds pairing information" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: true)

      assert string_to_quoted.("foo") == {:foo, [line: 1], nil}
      assert string_to_quoted.("foo()") == {:foo, [closing: [line: 1], line: 1], []}

      assert string_to_quoted.("foo(\n)") ==
               {:foo, [newlines: 1, closing: [line: 2], line: 1], []}

      assert string_to_quoted.("%{\n}") == {:%{}, [newlines: 1, closing: [line: 2], line: 1], []}

      assert string_to_quoted.("foo(\n) do\nend") ==
               {:foo, [do: [line: 2], end: [line: 3], newlines: 1, closing: [line: 2], line: 1],
                [[do: {:__block__, [], []}]]}
    end

    test "with :literal_encoder" do
      opts = [literal_encoder: &{:ok, {:__block__, &2, [&1]}}, token_metadata: true]
      string_to_quoted = &Code.string_to_quoted!(&1, opts)

      assert string_to_quoted.(~s("one")) == {:__block__, [delimiter: "\"", line: 1], ["one"]}
      assert string_to_quoted.("'one'") == {:__block__, [delimiter: "'", line: 1], ['one']}
      assert string_to_quoted.("?é") == {:__block__, [token: "?é", line: 1], [233]}
      assert string_to_quoted.("0b10") == {:__block__, [token: "0b10", line: 1], [2]}
      assert string_to_quoted.("12") == {:__block__, [token: "12", line: 1], [12]}
      assert string_to_quoted.("0o123") == {:__block__, [token: "0o123", line: 1], [83]}
      assert string_to_quoted.("0xEF") == {:__block__, [token: "0xEF", line: 1], [239]}
      assert string_to_quoted.("12.3") == {:__block__, [token: "12.3", line: 1], [12.3]}
      assert string_to_quoted.("nil") == {:__block__, [line: 1], [nil]}
      assert string_to_quoted.(":one") == {:__block__, [line: 1], [:one]}

      assert string_to_quoted.("[one: :two]") == {
               :__block__,
               [{:closing, [line: 1]}, {:line, 1}],
               [
                 [
                   {{:__block__, [format: :keyword, line: 1], [:one]},
                    {:__block__, [line: 1], [:two]}}
                 ]
               ]
             }

      assert string_to_quoted.("[1]") ==
               {:__block__, [closing: [line: 1], line: 1],
                [[{:__block__, [token: "1", line: 1], [1]}]]}

      assert string_to_quoted.(~s("""\nhello\n""")) ==
               {:__block__, [delimiter: ~s["""], line: 1], ["hello\n"]}

      assert string_to_quoted.("'''\nhello\n'''") ==
               {:__block__, [delimiter: ~s['''], line: 1], ['hello\n']}

      assert string_to_quoted.(~s[fn (1) -> "hello" end]) ==
               {:fn, [closing: [line: 1], line: 1],
                [
                  {:->, [line: 1],
                   [
                     [{:__block__, [token: "1", line: 1, closing: [line: 1], line: 1], [1]}],
                     {:__block__, [delimiter: "\"", line: 1], ["hello"]}
                   ]}
                ]}
    end

    test "adds identifier_location for qualified identifiers" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: true, columns: true)

      assert string_to_quoted.("foo.\nbar") ==
               {{:., [line: 1, column: 4],
                 [
                   {:foo, [line: 1, column: 1], nil},
                   :bar
                 ]}, [no_parens: true, line: 2, column: 1], []}

      assert string_to_quoted.("foo\n.\nbar") ==
               {{:., [line: 2, column: 1],
                 [
                   {:foo, [line: 1, column: 1], nil},
                   :bar
                 ]}, [no_parens: true, line: 3, column: 1], []}

      assert string_to_quoted.(~s[Foo.\nbar(1)]) ==
               {{:., [line: 1, column: 4],
                 [
                   {:__aliases__, [last: [line: 1, column: 1], line: 1, column: 1], [:Foo]},
                   :bar
                 ]}, [closing: [line: 2, column: 6], line: 2, column: 1], [1]}
    end

    test "adds metadata for the last alias segment" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: true)

      assert string_to_quoted.("Foo") == {:__aliases__, [last: [line: 1], line: 1], [:Foo]}

      assert string_to_quoted.("Foo.\nBar\n.\nBaz") ==
               {:__aliases__, [last: [line: 4], line: 1], [:Foo, :Bar, :Baz]}

      assert string_to_quoted.("foo.\nBar\n.\nBaz") ==
               {:__aliases__, [last: [line: 4], line: 1], [{:foo, [line: 1], nil}, :Bar, :Baz]}
    end
  end

  describe "token missing errors" do
    test "missing paren" do
      assert_token_missing(
        "nofile:1:9: missing terminator: ) (for \"(\" starting at line 1)",
        'case 1 ('
      )
    end

    test "dot terminator" do
      assert_token_missing(
        "nofile:1:9: missing terminator: \" (for function name starting at line 1)",
        'foo."bar'
      )
    end

    test "sigil terminator" do
      assert_token_missing(
        "nofile:3:1: missing terminator: \" (for sigil ~r\" starting at line 1)",
        '~r"foo\n\n'
      )

      assert_token_missing(
        "nofile:3:1: missing terminator: } (for sigil ~r{ starting at line 1)",
        '~r{foo\n\n'
      )
    end

    test "string terminator" do
      assert_token_missing(
        "nofile:1:5: missing terminator: \" (for string starting at line 1)",
        '"bar'
      )
    end

    test "heredoc with incomplete interpolation" do
      assert_token_missing(
        "nofile:2:1: missing interpolation terminator: \"}\" (for heredoc starting at line 1)",
        '"""\n\#{\n'
      )
    end

    test "heredoc terminator" do
      assert_token_missing(
        "nofile:2:4: missing terminator: \"\"\" (for heredoc starting at line 1)",
        '"""\nbar'
      )

      assert_token_missing(
        "nofile:2:7: missing terminator: \"\"\" (for heredoc starting at line 1)",
        '"""\nbar"""'
      )
    end

    test "missing end" do
      assert_token_missing(
        "nofile:1:9: missing terminator: end (for \"do\" starting at line 1)",
        'foo do 1'
      )

      assert_token_missing(
        ~r"HINT: it looks like the \"do\" on line 2 does not have a matching \"end\"",
        '''
        defmodule MyApp do
          def one do
          # end

          def two do
          end
        end
        '''
      )
    end
  end

  describe "syntax errors" do
    test "invalid heredoc start" do
      assert_syntax_error(
        "nofile:1:1: heredoc allows only zero or more whitespace characters followed by a new line after \"\"\"",
        '"""bar\n"""'
      )
    end

    test "invalid fn" do
      assert_syntax_error(
        "nofile:1: expected anonymous functions to be defined with -> inside: 'fn'",
        'fn 1 end'
      )

      assert_syntax_error(
        ~r"nofile:2: unexpected operator ->. If you want to define multiple clauses, ",
        'fn 1\n2 -> 3 end'
      )
    end

    test "invalid token" do
      assert_syntax_error(
        "nofile:1:7: unexpected token: \"\u200B\" (column 7, code point U+200B)",
        '[foo: \u200B]\noops'
      )
    end

    test "reserved tokens" do
      assert_syntax_error("nofile:1:1: reserved token: __aliases__", '__aliases__')
      assert_syntax_error("nofile:1:1: reserved token: __block__", '__block__')
    end

    test "invalid alias terminator" do
      assert_syntax_error(~r"nofile:1:5: unexpected \( after alias Foo", 'Foo()')
    end

    test "invalid quoted token" do
      assert_syntax_error(
        "nofile:1:9: syntax error before: \"world\"",
        '"hello" "world"'
      )

      assert_syntax_error(
        "nofile:1:3: syntax error before: 'Foobar'",
        '1 Foobar'
      )

      assert_syntax_error(
        "nofile:1:5: syntax error before: foo",
        'Foo.:foo'
      )

      assert_syntax_error(
        "nofile:1:5: syntax error before: \"foo\"",
        'Foo.:"foo\#{:bar}"'
      )

      assert_syntax_error(
        "nofile:1:5: syntax error before: \"",
        'Foo.:"\#{:bar}"'
      )
    end

    test "invalid identifier" do
      message = fn name ->
        "nofile:1:1: invalid character \"@\" (code point U+0040) in identifier: #{name}"
      end

      assert_syntax_error(message.("foo@"), 'foo@')
      assert_syntax_error(message.("foo@"), 'foo@ ')
      assert_syntax_error(message.("foo@bar"), 'foo@bar')

      message = fn name ->
        "nofile:1:1: invalid character \"@\" (code point U+0040) in alias: #{name}"
      end

      assert_syntax_error(message.("Foo@"), 'Foo@')
      assert_syntax_error(message.("Foo@bar"), 'Foo@bar')

      message = "nofile:1:1: invalid character \"!\" (code point U+0021) in alias: Foo!"
      assert_syntax_error(message, 'Foo!')

      message = "nofile:1:1: invalid character \"?\" (code point U+003F) in alias: Foo?"
      assert_syntax_error(message, 'Foo?')

      message =
        "nofile:1:1: invalid character \"ó\" (code point U+00F3) in alias (only ASCII characters are allowed): Foó"

      assert_syntax_error(message, 'Foó')

      message = ~r"""
      Elixir expects unquoted Unicode atoms, variables, and calls to be in NFC form.

      Got:

          "foó" \(code points 0x0066 0x006F 0x006F 0x0301\)

      Expected:

          "foó" \(code points 0x0066 0x006F 0x00F3\)

      """

      assert_syntax_error(message, :unicode.characters_to_nfd_list("foó"))
    end

    test "kw missing space" do
      msg = "nofile:1:1: keyword argument must be followed by space after: foo:"

      assert_syntax_error(msg, "foo:bar")
      assert_syntax_error(msg, "foo:+")
      assert_syntax_error(msg, "foo:+1")
    end

    test "invalid map start" do
      assert_syntax_error(
        "nofile:1:7: expected %{ to define a map, got: %[",
        "{:ok, %[], %{}}"
      )
    end

    test "unexpected end" do
      assert_syntax_error("nofile:1:3: unexpected reserved word: end", '1 end')

      assert_syntax_error(
        ~r" HINT: it looks like the \"end\" on line 2 does not have a matching \"do\" defined before it",
        '''
        defmodule MyApp do
          def one end
          def two do end
        end
        '''
      )

      assert_syntax_error(
        ~r" HINT: it looks like the \"end\" on line 3 does not have a matching \"do\" defined before it",
        '''
        defmodule MyApp do
          def one
          end

          def two do
          end
        end
        '''
      )

      assert_syntax_error(
        ~r" HINT: it looks like the \"end\" on line 6 does not have a matching \"do\" defined before it",
        '''
        defmodule MyApp do
          def one do
          end

          def two
          end
        end
        '''
      )

      assert_syntax_error(
        ~r"HINT: it looks like the \"do\" on line 3 does not have a matching \"end\"",
        '''
        defmodule MyApp do
          (
            def one do
            # end

            def two do
            end
          )
        end
        '''
      )
    end

    test "invalid keywords" do
      assert_syntax_error(
        "nofile:1:2: syntax error before: '.'",
        '+.foo'
      )

      assert_syntax_error(
        ~r"nofile:1:1: syntax error before: after. \"after\" is a reserved word",
        'after = 1'
      )
    end

    test "before sigil" do
      msg = fn x -> "nofile:1:9: syntax error before: sigil ~s starting with content '#{x}'" end

      assert_syntax_error(msg.("bar baz"), '~s(foo) ~s(bar baz)')
      assert_syntax_error(msg.(""), '~s(foo) ~s()')
      assert_syntax_error(msg.("bar "), '~s(foo) ~s(bar \#{:baz})')
      assert_syntax_error(msg.(""), '~s(foo) ~s(\#{:bar} baz)')
    end

    test "invalid do" do
      assert_syntax_error(
        ~r/nofile:1:10: unexpected reserved word: do./,
        'if true, do\n'
      )

      assert_syntax_error(~r/nofile:1: unexpected keyword: do:./, 'if true do:\n')
    end

    test "invalid parens call" do
      msg =
        "nofile:1: unexpected parentheses. If you are making a function call, do not " <>
          "insert spaces between the function name and the opening parentheses. " <>
          "Syntax error before: '('"

      assert_syntax_error(msg, 'foo (hello, world)')
    end

    test "invalid nested no parens call" do
      msg = ~r"nofile:1: unexpected comma. Parentheses are required to solve ambiguity"

      assert_syntax_error(msg, '[foo 1, 2]')
      assert_syntax_error(msg, '[foo bar 1, 2]')
      assert_syntax_error(msg, '[do: foo 1, 2]')
      assert_syntax_error(msg, 'foo(do: bar 1, 2)')
      assert_syntax_error(msg, '{foo 1, 2}')
      assert_syntax_error(msg, '{foo bar 1, 2}')
      assert_syntax_error(msg, 'foo 1, foo 2, 3')
      assert_syntax_error(msg, 'foo 1, @bar 3, 4')
      assert_syntax_error(msg, 'foo 1, 2 + bar 3, 4')
      assert_syntax_error(msg, 'foo(1, foo 2, 3)')

      interpret = fn x -> Macro.to_string(Code.string_to_quoted!(x)) end
      assert interpret.("f 1 + g h 2, 3") == "f(1 + g(h(2, 3)))"

      assert interpret.("assert [] = TestRepo.all from p in Post, where: p.title in ^[]") ==
               "assert [] = TestRepo.all(from(p in Post, where: p.title in ^[]))"
    end

    test "invalid atom dot alias" do
      msg =
        "nofile:1: atom cannot be followed by an alias. If the '.' was meant to be " <>
          "part of the atom's name, the atom name must be quoted. Syntax error before: '.'"

      assert_syntax_error(msg, ':foo.Bar')
      assert_syntax_error(msg, ':"+".Bar')
    end

    test "invalid map/struct" do
      assert_syntax_error("nofile:1:5: syntax error before: '}'", '%{:a}')
      assert_syntax_error("nofile:1:11: syntax error before: '}'", '%{{:a, :b}}')
      assert_syntax_error("nofile:1:8: syntax error before: '{'", '%{a, b}{a: :b}')
    end

    test "invalid interpolation" do
      assert_syntax_error(
        "nofile:1:17: unexpected token: ). The \"do\" at line 1 is missing terminator \"end\"",
        '"foo\#{case 1 do )}bar"'
      )
    end

    test "invalid end of expression" do
      # All valid examples
      Code.eval_quoted('''
      1;
      2;
      3

      (;)
      (;1)
      (1;)
      (1; 2)

      fn -> 1; 2 end
      fn -> ; end

      if true do
        ;
      end

      try do
        ;
      catch
        _, _ -> ;
      after
        ;
      end
      ''')

      # All invalid examples
      assert_syntax_error("nofile:1:3: syntax error before: ';'", '1+;\n2')

      assert_syntax_error("nofile:1:8: syntax error before: ';'", 'max(1, ;2)')
    end

    test "invalid new line" do
      assert_syntax_error(
        "nofile:3:6: unexpectedly reached end of line. The current expression is invalid or incomplete",
        'if true do\n  foo = [],\n  baz\nend'
      )
    end

    test "invalid \"fn do expr end\"" do
      assert_syntax_error(
        "nofile:1:4: unexpected reserved word: do. Anonymous functions are written as:\n\n    fn pattern -> expression end",
        'fn do :ok end'
      )
    end

    test "characters literal are printed correctly in syntax errors" do
      assert_syntax_error("nofile:1:5: syntax error before: ?a", ':ok ?a')
      assert_syntax_error("nofile:1:5: syntax error before: ?\\s", ':ok ?\\s')
      assert_syntax_error("nofile:1:5: syntax error before: ?す", ':ok ?す')
    end

    test "numbers are printed correctly in syntax errors" do
      assert_syntax_error("nofile:1:5: syntax error before: \"12\"", ':ok 12')
      assert_syntax_error("nofile:1:5: syntax error before: \"0b1\"", ':ok 0b1')
      assert_syntax_error("nofile:1:5: syntax error before: \"12.3\"", ':ok 12.3')

      assert_syntax_error(
        ~r"nofile:1:1: invalid character _ after number 123_456",
        '123_456_foo'
      )
    end

    test "on hex errors" do
      msg =
        "invalid hex escape character, expected \\xHH where H is a hexadecimal digit. Syntax error after: \\x"

      assert_syntax_error("nofile:1:2: #{msg}", ~S["\x"])
      assert_syntax_error("nofile:1:1: #{msg}", ~S[:"\x"])
      assert_syntax_error("nofile:1:2: #{msg}", ~S["\x": 123])
      assert_syntax_error("nofile:1:1: #{msg}", ~s["""\n\\x\n"""])
    end

    test "on unicode errors" do
      msg =
        "invalid Unicode escape character, expected \\uHHHH or \\u{H*} where H is a hexadecimal digit. Syntax error after: \\u"

      assert_syntax_error("nofile:1:2: #{msg}", ~S["\u"])
      assert_syntax_error("nofile:1:1: #{msg}", ~S[:"\u"])
      assert_syntax_error("nofile:1:2: #{msg}", ~S["\u": 123])
      assert_syntax_error("nofile:1:1: #{msg}", ~s["""\n\\u\n"""])

      assert_syntax_error(
        "nofile:1:2: invalid or reserved Unicode code point \\u{FFFFFF}. Syntax error after: \\u",
        ~S["\u{FFFFFF}"]
      )
    end

    test "on interpolation in calls" do
      msg =
        ~r"interpolation is not allowed when calling function/macro. Found interpolation in a call starting with: \""

      assert_syntax_error(msg, ".\"\#{}\"")
      assert_syntax_error(msg, ".\"a\#{:b}\"c")
    end

    test "on long atoms" do
      atom =
        "@GR{+z]`_XrNla!d<GTZ]iw[s'l2N<5hGD0(.xh&}>0ptDp(amr.oS&<q(FA)5T3=},^{=JnwIOE*DPOslKV KF-kb7NF&Y#Lp3D7l/!s],^hnz1iB |E8~Y'-Rp&*E(O}|zoB#xsE.S/~~'=%H'2HOZu0PCfz6j=eHq5:yk{7&|}zeRONM+KWBCAUKWFw(tv9vkHTu#Ek$&]Q:~>,UbT}v$L|rHHXGV{;W!>avHbD[T-G5xrzR6m?rQPot-37B@"

      assert_syntax_error(
        ~r"atom length must be less than system limit: ",
        ~s[:"#{atom}"]
      )
    end
  end

  describe "down to Erlang" do
    test "contains of non-literals" do
      assert to_erl!("{:ok, make_ref()}") ==
               {:tuple, 1,
                [
                  {:atom, 0, :ok},
                  {:call, 1, {:remote, 1, {:atom, 0, :erlang}, {:atom, 1, :make_ref}}, []}
                ]}

      assert to_erl!("[:ok, make_ref()]") ==
               {:cons, 1, {:atom, 0, :ok},
                {:cons, 1,
                 {:call, 1, {:remote, 1, {:atom, 0, :erlang}, {:atom, 1, :make_ref}}, []},
                 {nil, 0}}}
    end

    defp to_erl!(code) do
      {expr, _, _} =
        code
        |> Code.string_to_quoted!()
        |> :elixir.quoted_to_erl(:elixir.env_for_eval([]))

      expr
    end
  end

  defp parse!(string), do: Code.string_to_quoted!(string)

  defp assert_token_missing(given_message, string) do
    assert_raise TokenMissingError, given_message, fn -> parse!(string) end
  end

  defp assert_syntax_error(given_message, string) do
    assert_raise SyntaxError, given_message, fn -> parse!(string) end
  end
end
