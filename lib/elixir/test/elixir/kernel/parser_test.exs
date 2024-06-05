Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ParserTest do
  use ExUnit.Case, async: true

  describe "nullary ops" do
    test "in expressions" do
      assert parse!("..") == {:.., [line: 1], []}
      assert parse!("...") == {:..., [line: 1], []}
    end

    test "in capture" do
      assert parse!("&../0") == {:&, [line: 1], [{:/, [line: 1], [{:.., [line: 1], nil}, 0]}]}
      assert parse!("&.../0") == {:&, [line: 1], [{:/, [line: 1], [{:..., [line: 1], nil}, 0]}]}
    end

    test "raises on ambiguous uses when also binary" do
      assert_raise SyntaxError, ~r/syntax error before: do/, fn ->
        parse!("if .. do end")
      end
    end
  end

  describe "unary ops" do
    test "in keywords" do
      assert parse!("f(!: :ok)") == {:f, [line: 1], [[!: :ok]]}
      assert parse!("f @: :ok") == {:f, [line: 1], [[@: :ok]]}
    end

    test "in maps" do
      assert parse!("%{+foo, bar => bat, ...baz}") ==
               {:%{}, [line: 1],
                [
                  {:+, [line: 1], [{:foo, [line: 1], nil}]},
                  {{:bar, [line: 1], nil}, {:bat, [line: 1], nil}},
                  {:..., [line: 1], [{:baz, [line: 1], nil}]}
                ]}
    end

    test "ambiguous ops" do
      assert parse!("f -var") ==
               {:f, [ambiguous_op: nil, line: 1], [{:-, [line: 1], [{:var, [line: 1], nil}]}]}

      assert parse!("f -(var)") ==
               {:f, [ambiguous_op: nil, line: 1], [{:-, [line: 1], [{:var, [line: 1], nil}]}]}

      assert parse!("f +-var") ==
               {:f, [{:ambiguous_op, nil}, {:line, 1}],
                [{:+, [line: 1], [{:-, [line: 1], [{:var, [line: 1], nil}]}]}]}

      assert parse!("f - var") ==
               {:-, [line: 1], [{:f, [line: 1], nil}, {:var, [line: 1], nil}]}

      assert parse!("f --var") ==
               {:--, [line: 1], [{:f, [line: 1], nil}, {:var, [line: 1], nil}]}

      assert parse!("(f ->var)") ==
               [{:->, [line: 1], [[{:f, [line: 1], nil}], {:var, [line: 1], nil}]}]
    end

    test "ambiguous ops in keywords" do
      assert parse!("f(+: :ok)") == {:f, [line: 1], [[+: :ok]]}
      assert parse!("f +: :ok") == {:f, [line: 1], [[+: :ok]]}
    end
  end

  describe "ternary ops" do
    test "root" do
      assert parse!("1..2//3") == {:..//, [line: 1], [1, 2, 3]}
      assert parse!("(1..2)//3") == {:..//, [line: 1], [1, 2, 3]}
    end

    test "with do-blocks" do
      assert parse!("foo do end..bar do end//baz do end") == {
               :..//,
               [line: 1],
               [
                 {:foo, [line: 1], [[do: {:__block__, [], []}]]},
                 {:bar, [line: 1], [[do: {:__block__, [], []}]]},
                 {:baz, [line: 1], [[do: {:__block__, [], []}]]}
               ]
             }
    end

    test "with no parens" do
      assert parse!("1..foo do end//bar bat") == {
               :..//,
               [line: 1],
               [
                 1,
                 {:foo, [line: 1], [[do: {:__block__, [], []}]]},
                 {:bar, [line: 1], [{:bat, [line: 1], nil}]}
               ]
             }
    end

    test "errors" do
      msg =
        "the range step operator (//) must immediately follow the range definition operator (..)"

      assert_syntax_error([msg], "foo..bar baz//bat")
      assert_syntax_error([msg], "foo++bar//bat")
      assert_syntax_error([msg], "foo..(bar//bat)")
    end
  end

  describe "identifier unicode normalization" do
    test "stops at ascii codepoints" do
      assert {:ok, {:ç, _, nil}} = Code.string_to_quoted("ç\n")
      assert {:ok, {:\\, _, [{:ç, _, nil}, 1]}} = Code.string_to_quoted(~S"ç\\1")
    end

    test "nfc normalization is performed" do
      # before elixir 1.14, non-nfc would error
      #  non-nfc:        "ç" (code points 0x0063 0x0327)
      #  nfc-normalized: "ç" (code points 0x00E7)
      assert Code.eval_string("ç = 1; ç") == {1, [ç: 1]}
    end

    test "elixir's additional normalization is performed" do
      # Common micro => Greek mu. See code formatter test too.
      assert Code.eval_string("µs = 1; μs") == {1, [{:μs, 1}]}

      # commented out: math symbols capability in elixir
      # normalizations, to ensure that we *can* handle codepoints
      # that are Common-script and non-ASCII
      # assert Code.eval_string("_ℕ𝕩 = 1") == {1, [{:"_ℕ𝕩", 1}]}
    end

    test "handles graphemes inside quoted identifiers" do
      assert {{:., _, [{:foo, _, nil}, :"➡️"]}, _, []} = Code.string_to_quoted!(~s|foo."➡️"|)
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

    test "valid multi-letter sigils" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: false)

      assert string_to_quoted.("~REGEX/foo/") ==
               {:sigil_REGEX, [delimiter: "/", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      assert string_to_quoted.("~REGEX/foo/mods") ==
               {:sigil_REGEX, [delimiter: "/", line: 1], [{:<<>>, [line: 1], ["foo"]}, ~c"mods"]}

      assert string_to_quoted.("~REGEX[foo]") ==
               {:sigil_REGEX, [delimiter: "[", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      meta = [delimiter: "\"\"\"", line: 1]
      args = {:sigil_MAT, meta, [{:<<>>, [indentation: 0, line: 1], ["1,2,3\n"]}, []]}
      assert string_to_quoted.("~MAT\"\"\"\n1,2,3\n\"\"\"") == args

      args = {:sigil_FOO1, meta, [{:<<>>, [indentation: 0, line: 1], ["1,2,3\n"]}, []]}
      assert string_to_quoted.("~FOO1\"\"\"\n1,2,3\n\"\"\"") == args

      args = {:sigil_BAR321, meta, [{:<<>>, [indentation: 0, line: 1], ["1,2,3\n"]}, []]}
      assert string_to_quoted.("~BAR321\"\"\"\n1,2,3\n\"\"\"") == args

      args = {:sigil_I18N, meta, [{:<<>>, [indentation: 0, line: 1], ["1,2,3\n"]}, []]}
      assert string_to_quoted.("~I18N\"\"\"\n1,2,3\n\"\"\"") == args
    end

    test "invalid multi-letter sigils" do
      msg =
        ~r/invalid sigil name, it should be either a one-letter lowercase letter or an uppercase letter optionally followed by uppercase letters and digits/

      assert_syntax_error(["nofile:1:1:", msg], "~Regex/foo/")

      assert_syntax_error(["nofile:1:1:", msg], "~FOo1{bar]")

      assert_syntax_error(["nofile:1:1:", msg], "~foo1{bar]")
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

      assert Code.string_to_quoted("~UNKNOWN'foo bar'", existing_atoms_only: true) ==
               {:error, {[line: 1, column: 1], "unsafe atom does not exist: ", "sigil_UNKNOWN"}}
    end

    test "encodes atoms" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "there_is_no_such_atom"
        assert meta[:line] == 1
        assert meta[:column] == 1
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
        {:ok, {:my, "atom", ref}}
      end

      assert {:ok, {{:my, "atom", ^ref}, [line: 1], nil}} =
               Code.string_to_quoted("there_is_no_such_var", static_atoms_encoder: encoder)
    end

    test "encodes quoted keyword keys" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "there is no such key"
        assert meta[:line] == 1
        assert meta[:column] == 2
        {:ok, {:my, "atom", ref}}
      end

      assert {:ok, [{{:my, "atom", ^ref}, true}]} =
               Code.string_to_quoted(~S(["there is no such key": true]),
                 static_atoms_encoder: encoder
               )
    end

    test "encodes multi-letter sigils" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "sigil_UNKNOWN"
        assert meta[:line] == 1
        assert meta[:column] == 1
        {:ok, ref}
      end

      assert {:ok, {^ref, [delimiter: "'", line: 1], [{:<<>>, [line: 1], ["abc"]}, []]}} =
               Code.string_to_quoted("~UNKNOWN'abc'", static_atoms_encoder: encoder)
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

    test "does not encode one-letter sigils" do
      encoder = fn atom, _meta -> raise "shouldn't be invoked for #{atom}" end

      assert {:ok, {:sigil_z, [{:delimiter, "'"}, {:line, 1}], [{:<<>>, [line: 1], ["foo"]}, []]}} =
               Code.string_to_quoted("~z'foo'", static_atoms_encoder: encoder)

      assert {:ok, {:sigil_Z, [{:delimiter, "'"}, {:line, 1}], [{:<<>>, [line: 1], ["foo"]}, []]}} =
               Code.string_to_quoted("~Z'foo'", static_atoms_encoder: encoder)
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

      assert {:error, {[line: 1, column: 1], "Invalid atom name: ", "sigil_UNKNOWN"}} =
               Code.string_to_quoted("~UNKNOWN'foo bar'", static_atoms_encoder: encoder)
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

      nfc_abba = [225, 98, 98, 224]
      nfd_abba = [97, 769, 98, 98, 97, 768]
      context = [line: 1, column: 8]
      expr = "\"ábbà\" = 1"

      assert string_to_quoted.(String.normalize(expr, :nfc)) ==
               {:ok, {:=, context, [List.to_string(nfc_abba), 1]}}

      assert string_to_quoted.(String.normalize(expr, :nfd)) ==
               {:ok, {:=, context, [List.to_string(nfd_abba), 1]}}
    end

    test "handles maps and structs" do
      assert Code.string_to_quoted("%{}", columns: true) ==
               {:ok, {:%{}, [line: 1, column: 1], []}}

      assert Code.string_to_quoted("%:atom{}", columns: true) ==
               {:ok, {:%, [line: 1, column: 1], [:atom, {:%{}, [line: 1, column: 7], []}]}}
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
        {:five,
         [
           end_of_expression: [newlines: 1, line: 7, column: 7],
           closing: [line: 7, column: 6],
           line: 7,
           column: 1
         ], []}
      ]

      assert Code.string_to_quoted!(file, token_metadata: true, columns: true) ==
               {:__block__, [], args}
    end

    test "adds end_of_expression to the right hand side of ->" do
      file = """
      case true do
        :foo -> bar(); two()
        :baz -> bat()
      end
      """

      assert Code.string_to_quoted!(file, token_metadata: true) ==
               {:case,
                [
                  end_of_expression: [newlines: 1, line: 4],
                  do: [line: 1],
                  end: [line: 4],
                  line: 1
                ],
                [
                  true,
                  [
                    do: [
                      {:->, [line: 2],
                       [
                         [:foo],
                         {:__block__, [],
                          [
                            {:bar,
                             [
                               end_of_expression: [newlines: 0, line: 2],
                               closing: [line: 2],
                               line: 2
                             ], []},
                            {:two,
                             [
                               end_of_expression: [newlines: 1, line: 2],
                               closing: [line: 2],
                               line: 2
                             ], []}
                          ]}
                       ]},
                      {:->, [line: 3],
                       [
                         [:baz],
                         {:bat,
                          [
                            end_of_expression: [newlines: 1, line: 3],
                            closing: [line: 3],
                            line: 3
                          ], []}
                       ]}
                    ]
                  ]
                ]}
    end

    test "end of expression with literal" do
      file = """
      a do
        d ->
          (
            b -> c
          )
      end
      """

      assert Code.string_to_quoted!(file,
               token_metadata: true,
               literal_encoder: &{:ok, {:__block__, &2, [&1]}}
             ) ==
               {:a,
                [
                  end_of_expression: [newlines: 1, line: 6],
                  do: [line: 1],
                  end: [line: 6],
                  line: 1
                ],
                [
                  [
                    {{:__block__, [line: 1], [:do]},
                     [
                       {:->, [newlines: 1, line: 2],
                        [
                          [{:d, [line: 2], nil}],
                          {:__block__,
                           [
                             end_of_expression: [newlines: 1, line: 5],
                             newlines: 1,
                             closing: [line: 5],
                             line: 3
                           ],
                           [
                             [
                               {:->, [line: 4],
                                [
                                  [{:b, [line: 4], nil}],
                                  {:c, [end_of_expression: [newlines: 1, line: 4], line: 4], nil}
                                ]}
                             ]
                           ]}
                        ]}
                     ]}
                  ]
                ]}
    end

    test "does not add end of expression to ->" do
      file = """
      case true do
        :foo -> :bar
        :baz -> :bat
      end\
      """

      assert Code.string_to_quoted!(file, token_metadata: true) ==
               {:case, [do: [line: 1], end: [line: 4], line: 1],
                [
                  true,
                  [
                    do: [
                      {:->, [line: 2], [[:foo], :bar]},
                      {:->, [line: 3], [[:baz], :bat]}
                    ]
                  ]
                ]}
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
               {:__block__, [delimiter: ~s["""], indentation: 0, line: 1], ["hello\n"]}

      assert string_to_quoted.("'''\nhello\n'''") ==
               {:__block__, [delimiter: ~s['''], indentation: 0, line: 1], [~c"hello\n"]}

      assert string_to_quoted.(~s[fn (1) -> "hello" end]) ==
               {:fn, [closing: [line: 1], line: 1],
                [
                  {:->, [line: 1],
                   [
                     [{:__block__, [token: "1", line: 1], [1]}],
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

  describe "syntax errors" do
    test "invalid heredoc start" do
      assert_syntax_error(
        [
          "nofile:1:4:",
          ~r/heredoc allows only whitespace characters followed by a new line after opening \"\"\"/
        ],
        ~c"\"\"\"bar\n\"\"\""
      )
    end

    test "invalid fn" do
      assert_syntax_error(
        ["nofile:1:1:", "expected anonymous functions to be defined with -> inside: 'fn'"],
        ~c"fn 1 end"
      )

      assert_syntax_error(
        ["nofile:2:", "unexpected operator ->. If you want to define multiple clauses,"],
        ~c"fn 1\n2 -> 3 end"
      )
    end

    test "invalid token" do
      assert_syntax_error(
        ["nofile:1:1:", ~s/unexpected token: "#{"\u3164"}" (column 1, code point U+3164)/],
        ~c"ㅤ = 1"
      )

      assert_syntax_error(
        ["nofile:1:7:", ~s/unexpected token: "#{"\u200B"}" (column 7, code point U+200B)/],
        ~c"[foo: \u200B]\noops"
      )

      assert_syntax_error(
        ["nofile:1:1:", ~s/unexpected token: carriage return (column 1, code point U+000D)/],
        ~c"\r"
      )
    end

    test "invalid bidi in source" do
      assert_syntax_error(
        ["nofile:1:1:", ~s/invalid bidirectional formatting character in comment: \\u202A/],
        ~c"# This is a \u202A"
      )

      assert_syntax_error(
        ["nofile:1:5:", "invalid bidirectional formatting character in comment: \\u202A"],
        ~c"foo. # This is a \u202A"
      )

      assert_syntax_error(
        [
          "nofile:1:12:",
          "invalid bidirectional formatting character in string: \\u202A. If you want to use such character, use it in its escaped \\u202A form instead"
        ],
        ~c"\"this is a \u202A\""
      )

      assert_syntax_error(
        [
          "nofile:1:13:",
          "invalid bidirectional formatting character in string: \\u202A. If you want to use such character, use it in its escaped \\u202A form instead"
        ],
        ~c"\"this is a \\\u202A\""
      )
    end

    test "reserved tokens" do
      assert_syntax_error(["nofile:1:1:", "reserved token: __aliases__"], ~c"__aliases__")
      assert_syntax_error(["nofile:1:1:", "reserved token: __block__"], ~c"__block__")
    end

    test "invalid alias terminator" do
      assert_syntax_error(["nofile:1:4:", "unexpected ( after alias Foo"], ~c"Foo()")
    end

    test "invalid quoted token" do
      assert_syntax_error(
        ["nofile:1:9:", "syntax error before: \"world\""],
        ~c"\"hello\" \"world\""
      )

      assert_syntax_error(
        ["nofile:1:3:", "syntax error before: 'Foobar'"],
        ~c"1 Foobar"
      )

      assert_syntax_error(
        ["nofile:1:5:", "syntax error before: foo"],
        ~c"Foo.:foo"
      )

      assert_syntax_error(
        ["nofile:1:5:", "syntax error before: \"foo\""],
        ~c"Foo.:\"foo\#{:bar}\""
      )

      assert_syntax_error(
        ["nofile:1:5:", "syntax error before: \""],
        ~c"Foo.:\"\#{:bar}\""
      )
    end

    test "invalid identifier" do
      message =
        &["nofile:1:1:", ~s/invalid character "@" (code point U+0040) in identifier: #{&1}/]

      assert_syntax_error(message.("foo@"), ~c"foo@")
      assert_syntax_error(message.("foo@"), ~c"foo@ ")
      assert_syntax_error(message.("foo@bar"), ~c"foo@bar")

      message =
        &["nofile:1:1:", "invalid character \"@\" (code point U+0040) in alias: #{&1}"]

      assert_syntax_error(message.("Foo@"), ~c"Foo@")
      assert_syntax_error(message.("Foo@bar"), ~c"Foo@bar")

      message =
        [
          "nofile:1:1:",
          ~s/invalid character "!" (code point U+0021) in alias (only ASCII characters, without punctuation, are allowed): Foo!/
        ]

      assert_syntax_error(message, ~c"Foo!")

      message =
        [
          "nofile:1:1:",
          ~s/invalid character "?" (code point U+003F) in alias (only ASCII characters, without punctuation, are allowed): Foo?/
        ]

      assert_syntax_error(message, ~c"Foo?")

      message =
        [
          "nofile:1:1:",
          ~s/invalid character "ó" (code point U+00F3) in alias (only ASCII characters, without punctuation, are allowed): Foó/
        ]

      assert_syntax_error(message, ~c"Foó")

      # token suggestion heuristic:
      #  "for foO𝚳, NFKC isn't enough because 𝚳 nfkc's to Greek Μ, would be mixed script.
      #   however the 'confusability skeleton' for that token produces an all-Latin foOM
      #   and would tokenize -- so suggest that, in case that's what they want"
      message = [
        "Codepoint failed identifier tokenization, but a simpler form was found.",
        "Got:",
        ~s/"foO𝚳" (code points 0x00066 0x0006F 0x0004F 0x1D6B3)/,
        "Hint: You could write the above in a similar way that is accepted by Elixir:",
        ~s/"foOM" (code points 0x00066 0x0006F 0x0004F 0x0004D)/,
        "See https://hexdocs.pm/elixir/unicode-syntax.html for more information."
      ]

      assert_syntax_error(message, ~c"foO𝚳")

      # token suggestion heuristic:
      #  "for fooی𝚳, both NKFC and confusability would result in mixed scripts,
      #   because the Farsi letter is confusable with a different Arabic letter.
      #   Well, can't fix it all at once -- let's check for a suggestion just on
      #   the one codepoint that triggered this, the 𝚳 -- that would at least
      #   nudge them forwards."
      message = [
        "Elixir expects unquoted Unicode atoms, variables, and calls to use allowed codepoints and to be in NFC form.",
        "Got:",
        ~s/"𝚳" (code points 0x1D6B3)/,
        "Hint: You could write the above in a compatible format that is accepted by Elixir:",
        ~s/"Μ" (code points 0x0039C)/,
        "See https://hexdocs.pm/elixir/unicode-syntax.html for more information."
      ]

      assert_syntax_error(message, ~c"fooی𝚳")
    end

    test "keyword missing space" do
      msg = ["nofile:1:1:", "keyword argument must be followed by space after: foo:"]

      assert_syntax_error(msg, "foo:bar")
      assert_syntax_error(msg, "foo:+")
      assert_syntax_error(msg, "foo:+1")
    end

    test "invalid keyword list in tuple/binary" do
      assert_syntax_error(
        ["unexpected keyword list inside tuple"],
        ~c"{foo: :bar}"
      )

      assert_syntax_error(
        ["unexpected keyword list inside tuple"],
        ~c"{foo: :bar, baz: :bar}"
      )

      assert_syntax_error(
        ["unexpected keyword list inside bitstring"],
        ~c"<<foo: :bar, baz: :bar>>"
      )
    end

    test "expression after keyword lists" do
      assert_syntax_error(
        ["unexpected expression after keyword list"],
        ~c"call foo: 1, :bar"
      )

      assert_syntax_error(
        ["unexpected expression after keyword list"],
        ~c"call(foo: 1, :bar)"
      )

      assert_syntax_error(
        ["unexpected expression after keyword list"],
        ~c"[foo: 1, :bar]"
      )

      assert_syntax_error(
        ["unexpected expression after keyword list"],
        ~c"%{foo: 1, :bar => :bar}"
      )
    end

    test "syntax errors include formatted snippet" do
      message = ["nofile:1:5:", "syntax error before:", "1 + * 3", "^"]
      assert_syntax_error(message, "1 + * 3")
    end

    test "invalid map start" do
      assert_syntax_error(
        ["nofile:1:7:", "expected %{ to define a map, got: %["],
        "{:ok, %[], %{}}"
      )

      assert_syntax_error(
        ["nofile:1:3:", "unexpected space between % and {"],
        "% {1, 2, 3}"
      )
    end

    test "invalid access" do
      msg = ["nofile:1:6:", "too many arguments when accessing a value"]
      assert_syntax_error(msg, "foo[1, 2]")
      assert_syntax_error(msg, "foo[1, 2, 3]")
      assert_syntax_error(msg, "foo[1, 2, 3,]")
    end

    test "unexpected end" do
      assert_syntax_error(["nofile:1:3:", "unexpected reserved word: end"], ~c"1 end")

      assert_syntax_error(
        [
          "hint:",
          "the \"end\" on line 2 may not have a matching \"do\" defined before it (based on indentation)"
        ],
        ~c"""
        defmodule MyApp do
          def one end
          def two do end
        end
        """
      )

      assert_syntax_error(
        [
          "hint:",
          "the \"end\" on line 3 may not have a matching \"do\" defined before it (based on indentation)"
        ],
        ~c"""
        defmodule MyApp do
          def one
          end

          def two do
          end
        end
        """
      )

      assert_syntax_error(
        [
          "hint:",
          "the \"end\" on line 6 may not have a matching \"do\" defined before it (based on indentation)"
        ],
        ~c"""
        defmodule MyApp do
          def one do
          end

          def two
          end
        end
        """
      )
    end

    test "invalid keywords" do
      assert_syntax_error(
        ["nofile:1:2:", "syntax error before: '.'"],
        ~c"+.foo"
      )

      assert_syntax_error(
        ["nofile:1:1:", "syntax error before: after. \"after\" is a reserved word"],
        ~c"after = 1"
      )
    end

    test "before sigil" do
      msg = &["nofile:1:9:", "syntax error before: sigil ~s starting with content '#{&1}'"]

      assert_syntax_error(msg.("bar baz"), ~c"~s(foo) ~s(bar baz)")
      assert_syntax_error(msg.(""), ~c"~s(foo) ~s()")
      assert_syntax_error(msg.("bar "), ~c"~s(foo) ~s(bar \#{:baz})")
      assert_syntax_error(msg.(""), ~c"~s(foo) ~s(\#{:bar} baz)")
    end

    test "invalid do" do
      assert_syntax_error(
        ["nofile:1:10:", "unexpected reserved word: do."],
        ~c"if true, do\n"
      )

      assert_syntax_error(["nofile:1:9:", "unexpected keyword: do:."], ~c"if true do:\n")
    end

    test "invalid parens call" do
      msg =
        [
          "nofile:1:5:",
          "unexpected parentheses",
          "If you are making a function call, do not insert spaces between the function name and the opening parentheses.",
          "Syntax error before: '\('"
        ]

      assert_syntax_error(msg, ~c"foo (hello, world)")
    end

    test "invalid nested no parens call" do
      msg = ["nofile:1:", "unexpected comma. Parentheses are required to solve ambiguity"]

      assert_syntax_error(msg, ~c"[foo 1, 2]")
      assert_syntax_error(msg, ~c"[foo bar 1, 2]")
      assert_syntax_error(msg, ~c"[do: foo 1, 2]")
      assert_syntax_error(msg, ~c"foo(do: bar 1, 2)")
      assert_syntax_error(msg, ~c"{foo 1, 2}")
      assert_syntax_error(msg, ~c"{foo bar 1, 2}")
      assert_syntax_error(msg, ~c"foo 1, foo 2, 3")
      assert_syntax_error(msg, ~c"foo 1, @bar 3, 4")
      assert_syntax_error(msg, ~c"foo 1, 2 + bar 3, 4")
      assert_syntax_error(msg, ~c"foo(1, foo 2, 3)")

      interpret = fn x -> Macro.to_string(Code.string_to_quoted!(x)) end
      assert interpret.("f 1 + g h 2, 3") == "f(1 + g(h(2, 3)))"

      assert interpret.("assert [] = TestRepo.all from p in Post, where: p.title in ^[]") ==
               "assert [] = TestRepo.all(from(p in Post, where: p.title in ^[]))"
    end

    test "invalid atom dot alias" do
      msg =
        [
          "nofile:1:6:",
          "atom cannot be followed by an alias. If the '.' was meant to be " <>
            "part of the atom's name, the atom name must be quoted. Syntax error before: '.'"
        ]

      assert_syntax_error(msg, ~c":foo.Bar")
      assert_syntax_error(msg, ~c":\"+\".Bar")
    end

    test "invalid map/struct" do
      assert_syntax_error(["nofile:1:15:", "syntax error before: '}'"], ~c"%{foo bar, baz}")
      assert_syntax_error(["nofile:1:8:", "syntax error before: '{'"], ~c"%{a, b}{a: :b}")
    end

    test "mismatching delimiters" do
      assert_mismatched_delimiter_error(
        [
          "nofile:1:9:",
          "unexpected token:",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"fn a -> )"
      )

      assert_mismatched_delimiter_error(
        [
          "nofile:1:16:",
          "unexpected token:",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"defmodule A do ]"
      )

      assert_mismatched_delimiter_error(
        [
          "nofile:1:9:",
          "unexpected token:",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"(1, 2, 3}"
      )

      assert_mismatched_delimiter_error(
        [
          "nofile:1:14:",
          "unexpected reserved word:",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"<<1, 2, 3, 4 end"
      )
    end

    test "invalid interpolation" do
      assert_mismatched_delimiter_error(
        [
          "nofile:1:17:",
          "unexpected token:",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"\"foo\#{case 1 do )}bar\""
      )

      assert_mismatched_delimiter_error(
        [
          "nofile:8:3:",
          "unexpected token: )",
          "└ unclosed delimiter",
          "└ mismatched closing delimiter"
        ],
        ~c"""
        defmodule MyApp do
          (
            def one do
            # end

            def two do
            end
          )
        end
        """
      )
    end

    test "invalid end of expression" do
      # All valid examples
      Code.eval_quoted(~c"""
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
      """)

      # All invalid examples
      assert_syntax_error(["nofile:1:3:", "syntax error before: ';'"], ~c"1+;\n2")

      assert_syntax_error(["nofile:1:8:", "syntax error before: ';'"], ~c"max(1, ;2)")
    end

    test "invalid new line" do
      assert_syntax_error(
        [
          "nofile:3:6:",
          "unexpectedly reached end of line. The current expression is invalid or incomplete",
          "baz",
          "^"
        ],
        ~c"if true do\n  foo = [],\n  baz\nend"
      )
    end

    test "invalid \"fn do expr end\"" do
      assert_syntax_error(
        [
          "nofile:1:4:",
          "unexpected reserved word: do. Anonymous functions are written as:",
          "fn pattern -> expression end",
          "Please remove the \"do\" keyword",
          "fn do :ok end",
          "^"
        ],
        ~c"fn do :ok end"
      )
    end

    test "characters literal are printed correctly in syntax errors" do
      assert_syntax_error(["nofile:1:5:", "syntax error before: ?a"], ~c":ok ?a")
      assert_syntax_error(["nofile:1:5:", "syntax error before: ?\\s"], ~c":ok ?\\s")
      assert_syntax_error(["nofile:1:5:", "syntax error before: ?す"], ~c":ok ?す")
    end

    test "numbers are printed correctly in syntax errors" do
      assert_syntax_error(["nofile:1:5:", ~s/syntax error before: "12"/], ~c":ok 12")
      assert_syntax_error(["nofile:1:5:", ~s/syntax error before: "0b1"/], ~c":ok 0b1")
      assert_syntax_error(["nofile:1:5:", ~s/syntax error before: "12.3"/], ~c":ok 12.3")

      assert_syntax_error(
        ["nofile:1:1:", ~s/invalid character "_" after number 123_456/],
        ~c"123_456_foo"
      )
    end

    test "on hex errors" do
      msg =
        "invalid hex escape character, expected \\xHH where H is a hexadecimal digit. Syntax error after: \\x"

      assert_syntax_error(["nofile:1:2:", msg], ~S["\x"])
      assert_syntax_error(["nofile:1:1:", msg], ~S[:"\x"])
      assert_syntax_error(["nofile:1:2:", msg], ~S["\x": 123])
      assert_syntax_error(["nofile:1:1:", msg], ~s["""\n\\x\n"""])
    end

    test "on unicode errors" do
      msg = "invalid Unicode escape character"

      assert_syntax_error(["nofile:1:2:", msg], ~S["\u"])
      assert_syntax_error(["nofile:1:1:", msg], ~S[:"\u"])
      assert_syntax_error(["nofile:1:2:", msg], ~S["\u": 123])
      assert_syntax_error(["nofile:1:1:", msg], ~s["""\n\\u\n"""])

      assert_syntax_error(
        [
          "nofile:1:2:",
          "invalid or reserved Unicode code point \\u{FFFFFF}. Syntax error after: \\u"
        ],
        ~S["\u{FFFFFF}"]
      )
    end

    test "on interpolation in calls" do
      msg =
        "interpolation is not allowed when calling function/macro. Found interpolation in a call starting with: \""

      assert_syntax_error([msg], ".\"\#{}\"")
      assert_syntax_error([msg], ".\"a\#{:b}\"c")
    end

    test "on long atoms" do
      atom =
        "@GR{+z]`_XrNla!d<GTZ]iw[s'l2N<5hGD0(.xh&}>0ptDp(amr.oS&<q(FA)5T3=},^{=JnwIOE*DPOslKV KF-kb7NF&Y#Lp3D7l/!s],^hnz1iB |E8~Y'-Rp&*E(O}|zoB#xsE.S/~~'=%H'2HOZu0PCfz6j=eHq5:yk{7&|}zeRONM+KWBCAUKWFw(tv9vkHTu#Ek$&]Q:~>,UbT}v$L|rHHXGV{;W!>avHbD[T-G5xrzR6m?rQPot-37B@"

      assert_syntax_error(
        ["atom length must be less than system limit: "],
        ~s{:"#{atom}"}
      )

      assert_syntax_error(
        ["atom length must be less than system limit: "],
        ~s{["#{atom}": 123]}
      )
    end
  end

  defp parse!(string), do: Code.string_to_quoted!(string)

  defp assert_syntax_error(given_messages, source) do
    e = assert_raise SyntaxError, fn -> parse!(source) end
    assert_exception_msg(e, given_messages)
  end

  defp assert_mismatched_delimiter_error(given_messages, source) do
    e = assert_raise MismatchedDelimiterError, fn -> parse!(source) end
    assert_exception_msg(e, given_messages)
  end

  defp assert_exception_msg(exception, messages) do
    error_msg = Exception.format(:error, exception, [])

    for msg <- messages do
      assert error_msg =~ msg
    end
  end
end
