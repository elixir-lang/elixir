Code.require_file("test_helper.exs", __DIR__)

defmodule Code.NormalizerTest do
  use ExUnit.Case

  alias Code.Normalizer

  test "integers" do
    normalized = Normalizer.normalize(10)

    assert {:__block__, meta, [10]} = normalized
    assert Keyword.has_key?(meta, :line)
    assert meta[:token] == "10"
  end

  test "floats" do
    normalized = Normalizer.normalize(10.42)

    assert {:__block__, meta, [10.42]} = normalized
    assert Keyword.has_key?(meta, :line)
    assert meta[:token] == "10.42"
  end

  test "atoms" do
    normalized = Normalizer.normalize(:hello)

    assert {:__block__, meta, [:hello]} = normalized
    assert Keyword.has_key?(meta, :line)
  end

  test "strings" do
    normalized = Normalizer.normalize("hello world")

    assert {:__block__, meta, ["hello world"]} = normalized
    assert Keyword.has_key?(meta, :line)
    assert meta[:token] == ~s["hello world"]
  end

  test "charlists" do
    normalized = Normalizer.normalize('hello world')

    assert {:__block__, meta, ['hello world']} = normalized
    assert Keyword.has_key?(meta, :line)
    assert meta[:delimiter] == "'"
  end

  test "two elements tuples" do
    normalized = Normalizer.normalize({:foo, :bar})

    assert {:__block__, meta,
            [
              {
                {:__block__, left_meta, [:foo]},
                {:__block__, right_meta, [:bar]}
              }
            ]} = normalized

    assert Keyword.has_key?(meta, :line)
    assert Keyword.has_key?(left_meta, :line)
    assert Keyword.has_key?(right_meta, :line)
  end

  test "regular lists" do
    normalized = Normalizer.normalize([1, 2, 3])

    assert {:__block__, meta,
            [
              [
                {:__block__, first_meta, [1]},
                {:__block__, second_meta, [2]},
                {:__block__, third_meta, [3]}
              ]
            ]} = normalized

    metas = [meta, first_meta, second_meta, third_meta]

    assert Enum.all?(metas, &Keyword.has_key?(&1, :line))

    assert [line: _] = meta[:closing]

    assert first_meta[:token] == "1"
    assert second_meta[:token] == "2"
    assert third_meta[:token] == "3"
  end

  test "keyword lists" do
    quoted = Code.string_to_quoted!("[a: :b, c: :d]")
    normalized = Normalizer.normalize(quoted)

    assert {:__block__, meta,
            [
              [
                {{:__block__, first_key_meta, [:a]}, {:__block__, _, [:b]}},
                {{:__block__, second_key_meta, [:c]}, {:__block__, _, [:d]}}
              ]
            ]} = normalized

    assert Keyword.has_key?(meta, :line)
    assert get_in(meta, [:closing, :line]) |> is_integer()
    assert first_key_meta[:format] == :keyword
    assert second_key_meta[:format] == :keyword
  end

  test "list with keyword list as last element" do
    quoted = Code.string_to_quoted!("[1, {:a, :b}, 2, c: :d, e: :f]")
    normalized = Normalizer.normalize(quoted)

    assert {:__block__, _,
            [
              [
                {:__block__, _, [1]},
                {:__block__, _,
                 [
                   {
                     {:__block__, a_meta, [:a]},
                     {:__block__, _, [:b]}
                   }
                 ]},
                {:__block__, _, [2]},
                {{:__block__, c_meta, [:c]}, {:__block__, _, [:d]}},
                {{:__block__, e_meta, [:e]}, {:__block__, _, [:f]}}
              ]
            ]} = normalized

    refute a_meta[:format] == :keyword

    assert c_meta[:format] == :keyword
    assert e_meta[:format] == :keyword
  end

  test "keyword list in guard with ommited square brackets" do
    quoted = Code.string_to_quoted!("a when foo: :bar, baz: :qux")
    normalized = Normalizer.normalize(quoted)

    assert {:when, _,
            [
              {:a, _, _},
              [
                {{:__block__, _, [:foo]}, {:__block__, _, [:bar]}},
                {{:__block__, _, [:baz]}, {:__block__, _, [:qux]}}
              ]
            ]} = normalized
  end

  test "maps" do
    quoted = Code.string_to_quoted!("%{foo: :bar}")
    normalized = Normalizer.normalize(quoted)

    assert {:%{}, _,
            [
              {{:__block__, key_meta, [:foo]}, {:__block__, _, [:bar]}}
            ]} = normalized

    assert Keyword.get(key_meta, :format) == :keyword
  end

  test "map with update syntax" do
    quoted = Code.string_to_quoted!("%{base | foo: :bar}")
    normalized = Normalizer.normalize(quoted)

    assert {:%{}, _,
            [
              {:|, _,
               [
                 {:base, _, _},
                 [{{:__block__, key_meta, [:foo]}, {:__block__, _, [:bar]}}]
               ]}
            ]} = normalized

    assert Keyword.get(key_meta, :format) == :keyword
  end

  test "aliases are skipped" do
    quoted = Code.string_to_quoted!("Foo.Bar")
    normalized = Normalizer.normalize(quoted)

    assert quoted == normalized
  end

  test "`Access` in access syntax is skipped" do
    quoted = Code.string_to_quoted!("foo[:bar]")
    normalized = Normalizer.normalize(quoted)

    assert {
             {:., _, [Access, :get]},
             _,
             [
               {:foo, _, _},
               {:__block__, _, [:bar]}
             ]
           } = normalized
  end

  test "only the left side of a dot call is normalized" do
    quoted = Code.string_to_quoted!("foo.bar()")
    normalized = Normalizer.normalize(quoted)

    assert {{:., _, [{:foo, _, _}, :bar]}, _, []} = normalized
  end

  test "do blocks" do
    quoted =
      Code.string_to_quoted!("""
      case do
        :ok
      end
      """)

    normalized = Normalizer.normalize(quoted)

    assert {:case, _,
            [
              {:__block__, _,
               [
                 [
                   {{:__block__, _, [:do]}, {:__block__, _, [:ok]}}
                 ]
               ]}
            ]} = normalized
  end

  test "do block when token_metadata: true" do
    quoted =
      Code.string_to_quoted!(
        """
        case do
          :ok
        end
        """,
        token_metadata: true
      )

    normalized = Normalizer.normalize(quoted)

    assert {:case, meta,
            [
              [
                {{:__block__, _, [:do]}, {:__block__, _, [:ok]}}
              ]
            ]} = normalized

    assert [line: _] = Keyword.get(meta, :do)
    assert [line: _] = Keyword.get(meta, :end)
  end

  test "function with arguments and do block" do
    quoted =
      Code.string_to_quoted!("""
      foo(:bar) do
        :ok
      end
      """)

    normalized = Normalizer.normalize(quoted)

    assert {:foo, _,
            [
              {:__block__, _, [:bar]},
              {:__block__, _,
               [
                 [{{:__block__, _, [:do]}, {:__block__, _, [:ok]}}]
               ]}
            ]} = normalized
  end

  test "do, else, catch, rescue and after blocks" do
    quoted =
      Code.string_to_quoted!("""
      foo do
        :in_do
      else
        :in_else
      catch
        :in_catch
      rescue
        :in_rescue
      after
        :in_after
      end
      """)

    normalized = Normalizer.normalize(quoted)

    assert {:foo, _,
            [
              {:__block__, _,
               [
                 [
                   {{:__block__, _, [:do]}, {:__block__, _, [:in_do]}},
                   {{:__block__, _, [:else]}, {:__block__, _, [:in_else]}},
                   {{:__block__, _, [:catch]}, {:__block__, _, [:in_catch]}},
                   {{:__block__, _, [:rescue]}, {:__block__, _, [:in_rescue]}},
                   {{:__block__, _, [:after]}, {:__block__, _, [:in_after]}}
                 ]
               ]}
            ]} = normalized
  end

  test "left to right arrows" do
    quoted =
      Code.string_to_quoted!("""
      case do
        :left -> :right
      end
      """)

    normalized = Normalizer.normalize(quoted)

    assert {:case, _,
            [
              {:__block__, _,
               [
                 [
                   {{:__block__, _, [:do]},
                    [
                      {:->, _,
                       [
                         [{:__block__, _, [:left]}],
                         {:__block__, _, [:right]}
                       ]}
                    ]}
                 ]
               ]}
            ]} = normalized
  end

  test "left to right arrow with multiple arguments" do
    quoted =
      Code.string_to_quoted!("""
      case do
        :foo, :bar -> :baz
      end
      """)

    normalized = Normalizer.normalize(quoted)

    assert {:case, _,
            [
              {:__block__, _,
               [
                 [
                   {{:__block__, _, [:do]},
                    [
                      {:->, _,
                       [
                         [
                           {:__block__, _, [:foo]},
                           {:__block__, _, [:bar]}
                         ],
                         {:__block__, _, [:baz]}
                       ]}
                    ]}
                 ]
               ]}
            ]} = normalized
  end

  test "unqualified call's form is skipped" do
    quoted = Code.string_to_quoted!("foo()")
    normalized = Normalizer.normalize(quoted)

    assert {:foo, _, []} = normalized
  end
end
