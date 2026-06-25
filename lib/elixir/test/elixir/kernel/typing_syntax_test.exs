# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.TypingSyntaxTest do
  use ExUnit.Case, async: true

  describe "@ with -> in quote" do
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

    test "raises on ambiguous nullary arrows without surrounding expressions" do
      assert_raise SyntaxError, ~r/write @sig \(\) -> bar or @sig \(-> bar\)/, fn ->
        parse_quote("""
        quote do
          @sig -> bar
        end
        """)
      end
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
                       [
                         {:sig, [line: 2], [[{:->, [line: 2], [[], {:bar, [line: 2], nil}]}]]}
                       ]}
                  ]
                ]}
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
  end

  defp parse_quote(source, opts \\ []), do: Code.string_to_quoted!(source, opts)
end
