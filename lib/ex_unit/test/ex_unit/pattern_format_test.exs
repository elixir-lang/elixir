Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternFormatTest do
  use ExUnit.Case

  alias ExUnit.{Pattern, Pattern.FormatValue}

  test "Two primatives" do
    # send(self(), 2)
    # assert_receive 1

    left =
      quote do
        1
      end

    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, 2)

    expected = [
      diff_delete: "2"
    ]

    assert actual == expected
  end

  test "message received, map" do
    # send(self(), %{a: 1, b: 1, d: 2, c: 3})
    # assert_receive %{a: 2}

    left =
      quote do
        %{a: 2}
      end

    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{a: 1, b: 1, d: 2, c: 3})

    expected = [
      "%{",
      "a: ",
      {:diff_delete, "1"},
      ", ",
      "b: 1",
      ", ",
      "c: 3",
      ", ",
      "d: 2",
      "}"
    ]

    assert actual == expected
  end

  test "message received, map with mulitble keys" do
    # send(self(), %{a: 1, b: 1, d: 2, c: 3})
    # assert_receive %{a: 1, b: 2}

    left = quote do: %{a: 1, b: 2}

    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{a: 1, b: 1, d: 2, c: 3})

    expected = [
      "%{",
      "a: ",
      "1",
      ", ",
      "b: ",
      {:diff_delete, "1"},
      ", ",
      "c: 3",
      ", ",
      "d: 2",
      "}"
    ]

    assert actual == expected
  end

  test "message received, map with multible keys don't match" do
    # send(self(), %{a: 2, b: 1})
    # assert_receive %{a: 1, b: 2}

    left = quote do: %{a: 1, b: 2}

    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{a: 2, b: 1})

    expected = ["%{", "a: ", {:diff_delete, "2"}, ", ", "b: ", {:diff_delete, "1"}, "}"]
    assert actual == expected
  end

  test "message received, list" do
    # send(self(), [1, 2, 3])
    # send(self(), [1, 2])
    # send(self(), [1, 2, 3, 4, 5])
    # assert_receive [1, 2, 3, 4]

    left = quote do: [1, 2, 3, 4]
    pattern = Pattern.new(left, [], %{})

    actual = FormatValue.script(pattern, [1, 2, 3])
    expected = [{:diff_delete, ["[", ["1"], ", ", ["2"], ", ", ["3"], "]"]}]

    assert actual == expected

    actual = FormatValue.script(pattern, [1, 2, 3, 4, 5])
    expected = ["[", "1", ", ", "2", ", ", "3", ", ", "4", ", ", {:diff_delete, "5"}, "]"]

    assert actual == expected
  end

  test "cons list" do
    # send(self(), [1, 2, 3])
    # assert_receive [2 | _rest]

    left = quote do: [2 | _rest]
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, [1, 2, 3])
    expected = ["[", {:diff_delete, "1"}, " | ", "[2, 3]", "]"]
    assert actual == expected
  end

  test "multiple cons list" do
    # send(self(), [1, 2, 3])
    # b = 3
    # assert_receive [1 | [^b | _rest]]
    left = quote do: [1 | [^b | _rest]]

    pattern = Pattern.new(left, [b: 3], %{_rest: :ex_unit_unbound_var})

    actual = FormatValue.script(pattern, [1, 2, 3])
    expected = ["[", "1", " | ", "[", {:diff_delete, "2"}, ", ", "[3]", "]", "]"]
    assert actual == expected
  end

  test "Large, nested map, doesn't match" do
    # send(self(), %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})
    # assert_receive %{a: 1, b: %{a: 2, c: %{d: 4, f: "worl"}}, e: "hello"}

    left = quote do: %{a: 1, b: %{a: 2, c: %{d: 4, f: "worl"}}, e: "hello"}
    pattern = Pattern.new(left, [], %{})

    actual = FormatValue.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})

    expected = [
      "%{",
      "a: ",
      "1",
      ", ",
      "b: ",
      "%{",
      "a: ",
      "2",
      ", ",
      "c: ",
      "%{",
      "d: ",
      "4",
      ", ",
      "f: ",
      {:diff_delete, "\"world\""},
      "}",
      "}",
      ", ",
      "e: ",
      "\"hello\"",
      "}"
    ]

    assert actual == expected
  end

  test "Large, nested map, missing" do
    # send(self(), %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"})
    # assert_receive %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"}

    left = quote do: %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"}
    pattern = Pattern.new(left, [], %{})

    actual = FormatValue.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"})

    expected = [
      "%{",
      "a: ",
      "1",
      ", ",
      "b: ",
      "%{",
      "a: ",
      "2",
      ", ",
      "c: ",
      "%{",
      "d: ",
      "4",
      "}",
      "}",
      ", ",
      "e: ",
      "\"hello\"",
      "}"
    ]

    assert actual == expected
  end

  test "Large, nested map, extra" do
    # this test passes
    # send(self(), %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})
    # assert_receive %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"}

    left = quote do: %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"}

    pattern = Pattern.new(left, [], %{})

    actual = FormatValue.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})

    expected = [
      "%{",
      "a: ",
      "1",
      ", ",
      "b: ",
      "%{",
      "a: ",
      "2",
      ", ",
      "c: ",
      "%{",
      "d: ",
      "4",
      ", ",
      "f: \"world\"",
      "}",
      "}",
      ", ",
      "e: ",
      "\"hello\"",
      "}"
    ]

    assert actual == expected
  end

  test "using variables" do
    # send(self(), [1, 2, 3])
    # assert_receive([a, a, 3])

    left = quote do: [a, a, 3]
    pattern = Pattern.new(left, [], %{a: :ex_unit_unbound_var})

    actual = FormatValue.script(pattern, [1, 2, 3])

    expected = ["[", "1", ", ", {:diff_delete, "2"}, ", ", "3", "]"]
    assert actual == expected
  end

  test "using guards" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, 2, 3] when is_binary(a)

    left = quote do: [a, 2, 3] when is_binary(a)

    pattern = Pattern.new(left, [], %{{:a, __MODULE__} => :ex_unit_unbound_var})

    actual = FormatValue.script(pattern, [1, 2, 3])

    expected = ["[", "1", ", ", "2", ", ", "3", "]", " ", {:diff_delete, "when is_binary(a)"}]
    assert actual == expected
  end

  test "using guards, but guard matches" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, b, 3, 4] when is_integer(a)

    left = quote do: [a, b, 3, 4] when is_integer(a)

    pattern =
      Pattern.new(left, [], %{
        {:a, __MODULE__} => :ex_unit_unbound_var,
        {:b, __MODULE__} => :ex_unit_unbound_var
      })

    actual = FormatValue.script(pattern, [1, 2, 3])

    expected = [
      {:diff_delete, ["[", ["1"], ", ", ["2"], ", ", ["3"], "]"]},
      " ",
      "when is_integer(a)"
    ]

    assert actual == expected
  end

  test "using multiple when and clauses" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, b, 3] when is_binary(a) and is_integer(b)

    left = quote do: [a, b, 3] when is_binary(a) and is_integer(b)

    pattern =
      Pattern.new(left, [], %{
        {:a, __MODULE__} => :ex_unit_unbound_var,
        {:b, __MODULE__} => :ex_unit_unbound_var
      })

    actual = FormatValue.script(pattern, [1, 2, 3])

    expected = [
      "[",
      "1",
      ", ",
      "2",
      ", ",
      "3",
      "]",
      " ",
      "when ",
      {:diff_delete, "is_binary(a)"},
      {:diff_delete, " and "},
      "is_integer(b)"
    ]

    assert actual == expected
  end

  test "using multiple when or clauses" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, b, 3] when is_binary(a) or is_binary(b)

    left = quote do: [a, b, 3] when is_binary(a) or is_binary(b)

    pattern =
      Pattern.new(left, [], %{
        {:a, __MODULE__} => :ex_unit_unbound_var,
        {:b, __MODULE__} => :ex_unit_unbound_var
      })

    actual = FormatValue.script(pattern, [1, 2, 3])

    expected = [
      "[",
      "1",
      ", ",
      "2",
      ", ",
      "3",
      "]",
      " ",
      "when ",
      {:diff_delete, "is_binary(a)"},
      {:diff_delete, " or "},
      {:diff_delete, "is_binary(b)"}
    ]

    assert actual == expected
  end

  test "mixed key types" do
    # send(
    #   self(),
    #   {:save_doc, %{:status => :created, :sync_history => %{"map" => true}, "other" => true}}
    # )

    # assert_receive {:save_doc, %{status: :creted, sync_history: []} = doc}

    left = quote do: {:save_doc, %{status: :creted, sync_history: []} = doc}

    pattern = Pattern.new(left, [], %{doc: :ex_unit_unbound_var})

    actual =
      FormatValue.script(
        pattern,
        {:save_doc, %{:status => :created, :sync_history => %{"map" => true}, "other" => true}}
      )

    expected = [
      "{",
      ":save_doc",
      ", ",
      "%{",
      ":status => ",
      {:diff_delete, ":created"},
      ", ",
      ":sync_history => ",
      {:diff_delete, "%{\"map\" => true}"},
      ", ",
      "\"other\" => true",
      "}",
      "}"
    ]

    assert actual == expected
  end

  test "pinned map value does exist, but is different" do
    # val = 3
    # send(self(), %{a: 1, b: 2})
    # assert_receive(%{a: ^val, b: 2})

    left =
      quote do
        %{a: ^val, b: 2}
      end

    pattern = Pattern.new(left, [val: 3], %{})
    actual = FormatValue.script(pattern, %{a: 1, b: 2})

    expected = ["%{", "a: ", {:diff_delete, "1"}, ", ", "b: ", "2", "}"]

    assert actual == expected
  end

  test "pinned map value matches, but others don't" do
    # val = 3
    # send(self(), %{a: 3, b: 3})
    # assert_receive(%{a: ^val, b: 2})

    left =
      quote do
        %{a: ^val, b: 2}
      end

    pattern = Pattern.new(left, [val: 3], %{})
    actual = FormatValue.script(pattern, %{a: 3, b: 3})

    expected = ["%{", "a: ", "3", ", ", "b: ", {:diff_delete, "3"}, "}"]

    assert actual == expected
  end

  test "pinned map key doesn't exist" do
    # val = 3
    # send(self(), %{a: 1, b: 2, c: 3})
    # assert_receive(%{a: 1, d: ^val})

    left =
      quote do
        %{a: 1, d: ^val}
      end

    pattern = Pattern.new(left, [val: 3], %{})
    actual = FormatValue.script(pattern, %{a: 1, b: 2, c: 3})

    expected = ["%{", "a: ", "1", ", ", "b: 2", ", ", "c: 3", "}"]

    assert actual == expected
  end

  test "date time match" do
    # now = ~N[2018-01-01 12:30:00]
    # then = ~N[2008-01-01 12:30:00]
    # send(self(), now)

    # assert match?(%{a: ^now}, %{b: then})
    # assert now == then
    left = quote do: %{a: ^now}
    pattern = Pattern.new(left, [now: ~N[2018-01-01 12:30:00]], %{})

    actual = FormatValue.script(pattern, %{b: ~N[2008-01-01 12:30:00]})

    expected = [
      "%{",
      "b: ~N[2008-01-01 12:30:00]",
      "}"
    ]

    assert actual == expected
  end

  test "mismatched tuple size - left" do
    # assert match?({:a, :b, :c, :d}, {:a, :b, :c})
    left = quote do: {:a, :b, :c, :d}
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, {:a, :b, :c})

    expected = [{:diff_delete, ["{", [":a"], ", ", [":b"], ", ", [":c"], "}"]}]
    assert actual == expected
  end

  test "nested map, nested does not match" do
    # assert match?(%{a: 1, b: %{a: 1}}, %{a: 1, b: %{a: 2}})
    left = quote do: %{a: 1, b: %{a: 1}}
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{a: 1, b: %{a: 2}})

    expected = ["%{", "a: ", "1", ", ", "b: ", "%{", "a: ", {:diff_delete, "2"}, "}", "}"]
    assert actual == expected
  end

  test "nested tuple" do
    # assert match?({1, {1, 2}, {3, 4}}, {1, {3, 4}, {1, 2}})
    left = quote do: {1, {1, 2}, {3, 4}}
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, {1, {3, 4}, {1, 2}})

    expected = [
      "{",
      "1",
      ", ",
      "{",
      {:diff_delete, "3"},
      ", ",
      {:diff_delete, "4"},
      "}",
      ", ",
      "{",
      {:diff_delete, "1"},
      ", ",
      {:diff_delete, "2"},
      "}",
      "}"
    ]

    assert actual == expected
  end

  test "nested list" do
    # assert match?([1, [1, 2], [3, 4]], [1, [3, 4], [1, 2]])
    left = quote do: [1, [1, 2], [3, 4]]
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, [1, [3, 4], [1, 2]])

    expected = [
      "[",
      "1",
      ", ",
      "[",
      {:diff_delete, "3"},
      ", ",
      {:diff_delete, "4"},
      "]",
      ", ",
      "[",
      {:diff_delete, "1"},
      ", ",
      {:diff_delete, "2"},
      "]",
      "]"
    ]

    assert actual == expected
  end

  test "nested list cons operator in a map" do
    # assert match?(%{nested: [2 | [2 | _rest]]}, %{nested: [2, 3, 4]})
    left = quote do: %{nested: [2 | [2 | _rest]]}
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{nested: [2, 3, 4]})

    expected = [
      "%{",
      "nested: ",
      "[",
      "2",
      " | ",
      "[",
      {:diff_delete, "3"},
      " | ",
      "[4]",
      "]",
      "]",
      "}"
    ]

    assert actual == expected
  end

  test "unmatched cons" do
    # assert match?([1 | [2, 3]], %{a: 1})
    left = quote do: [1 | [2, 3]]
    pattern = Pattern.new(left, [], %{})
    actual = FormatValue.script(pattern, %{a: 1})

    expected = [diff_delete: "%{a: 1}"]
    assert actual == expected
  end
end
