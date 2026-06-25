# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TypingSyntaxTest do
  use ExUnit.Case, async: true

  describe "@ with -> in quote" do
    test "does not parse arrows at the root" do
      assert_raise SyntaxError, ~r/syntax error before: '->'/, fn ->
        parse_quote("@sig foo -> bar")
      end
    end

    test "wraps parenthesized arrows as the attribute call argument at the root" do
      assert parse_quote("(@sig foo -> bar)") ==
               {:@, [line: 1],
                [
                  {:sig, [line: 1],
                   [{:->, [line: 1], [[{:foo, [line: 1], nil}], {:bar, [line: 1], nil}]}]}
                ]}
    end

    test "does not parse multi-argument arrows at the root" do
      assert_raise SyntaxError, ~r/syntax error before: '->'/, fn ->
        parse_quote("@sig foo, bar -> baz")
      end
    end

    test "wraps parenthesized multi-argument arrows as the attribute call argument at the root" do
      assert parse_quote("(@sig foo, bar -> baz)") ==
               {:@, [line: 1],
                [
                  {:sig, [line: 1],
                   [
                     {:->, [line: 1],
                      [[{:foo, [line: 1], nil}, {:bar, [line: 1], nil}], {:baz, [line: 1], nil}]}
                   ]}
                ]}
    end

    test "does not parse nullary arrows at the root" do
      assert_raise SyntaxError, ~r/syntax error before: '->'/, fn ->
        parse_quote("@sig -> bar")
      end
    end

    test "wraps the arrow as the attribute call argument" do
      assert parse_quote("""
             quote do
               pre
               @sig foo -> bar
               post
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:__block__, [line: 1],
                       [
                         {:pre, [line: 2], nil},
                         {:@, [line: 3],
                          [
                            {:sig, [line: 3],
                             [
                               {:->, [line: 3],
                                [[{:foo, [line: 3], nil}], {:bar, [line: 3], nil}]}
                             ]}
                          ]},
                         {:post, [line: 4], nil}
                       ]}
                  ]
                ]}
    end

    test "wraps multi-argument arrows as the attribute call argument" do
      assert parse_quote("""
             quote do
               pre
               @sig foo, bar -> baz
               post
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:__block__, [line: 1],
                       [
                         {:pre, [line: 2], nil},
                         {:@, [line: 3],
                          [
                            {:sig, [line: 3],
                             [
                               {:->, [line: 3],
                                [
                                  [{:foo, [line: 3], nil}, {:bar, [line: 3], nil}],
                                  {:baz, [line: 3], nil}
                                ]}
                             ]}
                          ]},
                         {:post, [line: 4], nil}
                       ]}
                  ]
                ]}
    end

    test "raises on ambiguous nullary arrows" do
      assert_raise SyntaxError, ~r/ambiguous use of @ with ->/, fn ->
        parse_quote("""
        quote do
          pre
          @sig -> bar
          post
        end
        """)
      end
    end

    test "keeps nullary arrows as do-block stabs without surrounding expressions" do
      assert parse_quote("""
             quote do
               @sig -> bar
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do: [
                      {:->, [line: 2],
                       [[{:@, [line: 2], [{:sig, [line: 2], nil}]}], {:bar, [line: 2], nil}]}
                    ]
                  ]
                ]}
    end

    test "wraps explicit empty parentheses as the attribute call argument" do
      assert parse_quote(
               """
               quote do
                 @sig () -> bar
               end
               """,
               emit_warnings: false
             ) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:@, [line: 2],
                       [
                         {:sig, [line: 2],
                          [
                            {:->, [line: 2], [[{:__block__, [], []}], {:bar, [line: 2], nil}]}
                          ]}
                       ]}
                  ]
                ]}
    end

    test "allows explicit parenthesized arrows as attribute call arguments" do
      assert parse_quote("""
             quote do
               @sig (-> bar)
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:@, [line: 2],
                       [{:sig, [line: 2], [{:->, [line: 2], [[], {:bar, [line: 2], nil}]}]}]}
                  ]
                ]}
    end

    test "normalizes parenthesized arrows to the same AST as arrow arguments" do
      assert parse_quote("""
             quote do
               pre
               @sig foo -> bar
               post
             end
             """) ==
               parse_quote("""
               quote do
                 pre
                 @sig (foo -> bar)
                 post
               end
               """)
    end

    test "normalizes explicit parenthesized arrows with arguments as attribute call arguments" do
      assert parse_quote("""
             quote do
               @sig (foo -> bar)
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:@, [line: 2],
                       [
                         {:sig, [line: 2],
                          [{:->, [line: 2], [[{:foo, [line: 2], nil}], {:bar, [line: 2], nil}]}]}
                       ]}
                  ]
                ]}
    end

    test "does not allow arrows inside parenthesized attribute calls" do
      assert_raise SyntaxError, ~r/syntax error before: '->'/, fn ->
        parse_quote("""
        quote do
          @sig(foo -> bar)
        end
        """)
      end
    end

    test "applies to attributes other than sig" do
      assert parse_quote("""
             quote do
               @foo bar -> baz
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do:
                      {:@, [line: 2],
                       [
                         {:foo, [line: 2],
                          [{:->, [line: 2], [[{:bar, [line: 2], nil}], {:baz, [line: 2], nil}]}]}
                       ]}
                  ]
                ]}
    end

    test "does not apply to regular calls without @" do
      assert_raise SyntaxError, ~r/unexpected operator ->/, fn ->
        parse_quote("""
        quote do
          pre
          omg bar -> baz
          post
        end
        """)
      end
    end

    test "does not apply to parenthesized attribute calls with pre/post expressions" do
      assert_raise SyntaxError, ~r/unexpected operator ->/, fn ->
        parse_quote("""
        quote do
          pre
          (@sig foo, bar) -> baz
          post
        end
        """)
      end
    end

    test "keeps parenthesized attribute calls as do-block stabs without pre/post expressions" do
      assert parse_quote("""
             quote do
               (@sig foo, bar) -> baz
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do: [
                      {:->, [line: 2],
                       [
                         [
                           {:@, [line: 2],
                            [
                              {:sig, [line: 2], [{:foo, [line: 2], nil}, {:bar, [line: 2], nil}]}
                            ]}
                         ],
                         {:baz, [line: 2], nil}
                       ]}
                    ]
                  ]
                ]}
    end

    test "keeps ambiguous nullary arrows as separate do-block stabs inside stabs" do
      assert parse_quote("""
             quote do
               subject ->
                 setup
                 @sig -> result
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do: [
                      {:->, [line: 2], [[{:subject, [line: 2], nil}], {:setup, [line: 3], nil}]},
                      {:->, [line: 4],
                       [
                         [{:@, [line: 4], [{:sig, [line: 4], nil}]}],
                         {:result, [line: 4], nil}
                       ]}
                    ]
                  ]
                ]}
    end

    test "keeps explicit empty parentheses inside stab bodies" do
      assert parse_quote(
               """
               quote do
                 subject ->
                   setup
                   @sig () -> result
               end
               """,
               emit_warnings: false
             ) ==
               {:quote, [line: 1],
                [
                  [
                    do: [
                      {:->, [line: 2],
                       [
                         [{:subject, [line: 2], nil}],
                         {:__block__, [],
                          [
                            {:setup, [line: 3], nil},
                            {:@, [line: 4],
                             [
                               {:sig, [line: 4],
                                [
                                  {:->, [line: 4],
                                   [[{:__block__, [], []}], {:result, [line: 4], nil}]}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]
                  ]
                ]}
    end

    test "keeps typed arrows inside stab bodies" do
      assert parse_quote("""
             quote do
               subject ->
                 setup
                 @sig input -> output
             end
             """) ==
               {:quote, [line: 1],
                [
                  [
                    do: [
                      {:->, [line: 2],
                       [
                         [{:subject, [line: 2], nil}],
                         {:__block__, [],
                          [
                            {:setup, [line: 3], nil},
                            {:@, [line: 4],
                             [
                               {:sig, [line: 4],
                                [
                                  {:->, [line: 4],
                                   [
                                     [{:input, [line: 4], nil}],
                                     {:output, [line: 4], nil}
                                   ]}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]
                  ]
                ]}
    end
  end

  defp parse_quote(source, opts \\ []), do: Code.string_to_quoted!(source, opts)
end
