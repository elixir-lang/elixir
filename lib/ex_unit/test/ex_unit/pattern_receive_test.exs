Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternReceiveTest do
  use ExUnit.Case

  alias ExUnit.{Pattern, PatternDiff, ReceivePatternFormat}

  # test "message recevied, no messages" do
  #   assert_receive 1
  # end

  test "Two primatives" do
    #   send(self(), 2)
    #   assert_receive 1

    left =
      quote do
        1
      end

    pattern = Pattern.new(left, [], [])
    actual = ReceivePatternFormat.script(pattern, 2)

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

    pattern = Pattern.new(left, [], [])
    actual = ReceivePatternFormat.script(pattern, %{a: 1, b: 1, d: 2, c: 3})

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

    pattern = Pattern.new(left, [], [])
    actual = ReceivePatternFormat.script(pattern, %{a: 1, b: 1, d: 2, c: 3})

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

    pattern = Pattern.new(left, [], [])
    actual = ReceivePatternFormat.script(pattern, %{a: 2, b: 1})

    expected = ["%{", "a: ", {:diff_delete, "2"}, ", ", "b: ", {:diff_delete, "1"}, "}"]
    assert actual == expected
  end

  test "message received, list" do
    # send(self(), [1, 2, 3])
    # send(self(), [1, 2])
    # send(self(), [1, 2, 3, 4, 5])
    # assert_receive [1, 2, 3, 4]

    left = quote do: [1, 2, 3, 4]
    pattern = Pattern.new(left, [], [])

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])
    expected = ["[", "1", ", ", "2", ", ", "3", ", ", {:diff_insert, "4"}, "]"]

    assert actual == expected

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3, 4, 5])
    expected = ["[", "1", ", ", "2", ", ", "3", ", ", "4", ", ", {:diff_delete, "5"}, "]"]

    assert actual == expected
  end

  test "cons list" do
    # send(self(), [1, 2, 3])
    # assert_receive [2 | _rest]

    left = quote do: [2 | _rest]
    pattern = Pattern.new(left, [], [])
    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])
    expected = ["[", {:diff_delete, "1"}, " | ", {:diff_delete, "[2, 3]"}, "]"]
    assert actual == expected
  end

  test "multiple cons list" do
    # send(self(), [1, 2, 3])
    # b = 3
    # assert_receive [1 | [^b | _rest]]
    left = quote do: [1 | [^b | _rest]]

    pattern = Pattern.new(left, [b: 3], _rest: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])
    expected = ["[", "1", " | ", "[", {:diff_delete, "2"}, ", ", "[3]", "]", "]"]
    assert actual == expected
  end

  test "Large, nested map, doesn't match" do
    # send(self(), %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})
    # assert_receive %{a: 1, b: %{a: 2, c: %{d: 4, f: "worl"}}, e: "hello"}

    left = quote do: %{a: 1, b: %{a: 2, c: %{d: 4, f: "worl"}}, e: "hello"}
    pattern = Pattern.new(left, [], [])

    actual =
      ReceivePatternFormat.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})

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
    pattern = Pattern.new(left, [], [])

    actual = ReceivePatternFormat.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4}}, e: "hello"})

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
      {:diff_insert, "f: \"world\""},
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

    pattern = Pattern.new(left, [], [])

    actual =
      ReceivePatternFormat.script(pattern, %{a: 1, b: %{a: 2, c: %{d: 4, f: "world"}}, e: "hello"})

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
    pattern = Pattern.new(left, [], a: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])

    expected = ["[", "1", ", ", {:diff_delete, "2"}, ", ", "3", "]"]
    assert actual == expected
  end

  test "using guards" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, 2, 3] when is_binary(a)

    left = quote do: [a, 2, 3] when is_binary(a)

    pattern = Pattern.new(left, [], a: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])

    expected = ["[", "1", ", ", "2", ", ", "3", "]", " ", {:diff_delete, "when is_binary(a)"}]
    assert actual == expected
  end

  test "using guards, but guard matches" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, b, 3, 4] when is_integer(a)

    left = quote do: [a, b, 3, 4] when is_binary(a)

    pattern = Pattern.new(left, [], a: :ex_unit_unbound_var, b: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])

    expected = [
      "[",
      "1",
      ", ",
      "2",
      ", ",
      "3",
      ", ",
      {:diff_insert, "4"},
      "]",
      " ",
      {:diff_delete, "when is_binary(a)"}
    ]

    assert actual == expected
  end

  test "using multiple when and clauses" do
    # send(self(), [1, 2, 3])
    # assert_receive [a, b, 3] when is_binary(a) and is_integer(b)

    left = quote do: [a, b, 3] when is_binary(a) and is_integer(b)

    pattern = Pattern.new(left, [], a: :ex_unit_unbound_var, b: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])

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

    pattern = Pattern.new(left, [], a: :ex_unit_unbound_var, b: :ex_unit_unbound_var)

    actual = ReceivePatternFormat.script(pattern, [1, 2, 3])

    expected = [
      "[",
      "1",
      ", ",
      "2",
      ", ",
      "3",
      "]",
      " ",
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

    pattern = Pattern.new(left, [], doc: :ex_unit_unbound_var)

    actual =
      ReceivePatternFormat.script(
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

  # test "simple" do
  #   map = %{a: "1"}
  #   send(self(), map)

  #   match = %{a: "1", b: "2"}
  #   assert_receive(^match)
  # end

  # describe "string" do
  #   test "simple" do
  #     string = "abc"
  #     send(self(), string)

  #     match = "cde"
  #     assert_receive(^match)
  #   end
  # end

  # describe "map" do
  #   test "simple" do
  #     map = %{a: "1"}
  #     send(self(), map)

  #     match = %{a: "1", b: "2"}
  #     assert_receive(^match)
  #   end
  # end

  # describe "array" do
  #   test "simple" do
  #     array = [1, 2, 3, 4, 5, 6]
  #     send(self(), array)

  #     match = [3, 4, 5, 6]
  #     assert_receive(^match)
  #   end
  # end

  # describe "map nested" do
  #   test "map with atom keys" do
  #     map = %{a: "1", b: "string", c: [1, 2, 3], d: %{foo: :bar, a: 1, b: 2, c: 3}}
  #     send(self(), map)

  #     match = %{a: "2", b: "string", c: [1, 2, 4, 5], d: %{bar: :foo}}
  #     assert_receive(^match)
  #   end
  # end

  test "find the match" do
    # a = 1
    # assert match?(%{a: ^a, b: 3}, %{a: 2, b: 3})
    # assert %{a: 1} == %{a: 2, b: 3}
    # assert %{a: b, b: b} = %{a: 2, b: 3}
    # assert match?(%{a: b, b: b}, %{a: 2, b: 3})
    # send(self(), %{a: 2, b: 3})
    # assert_receive(%{a: a, b: b})
  end

  # test "binary match" do
  #   assert match?("hello" <> world, "hello world")
  # end

  # test "pinned map key doesn't exist" do
  #   val = 3
  #   send(self(), %{a: 1, b: 2, c: 3})
  #   assert_receive(%{a: 1, d: ^val})
  # end

  # test "date time match" do
  #   now = ~N[2018-01-01 12:30:00]
  #   then = ~N[2008-01-01 12:30:00]
  #   send(self(), now)
  #   # assert_receive ^then
  #   assert match?(%{a: ^now}, %{b: then})
  #   assert now == then
  # end

  # test "mismatched tuple size - left" do
  #   assert match?({:a, :b, :c, :d}, {:a, :b, :c})
  # end

  # test "nested map, nested does not match" do
  #   assert match?(%{a: 1, b: %{a: 1}}, %{a: 1, b: %{a: 2}})
  # end

  # test "nested tuple" do
  #   assert match?({1, {1, 2}, {3, 4}}, {1, {3, 4}, {1, 2}})
  # end

  # test "nested list" do
  #   assert match?([1, [1, 2], [3, 4]], [1, [3, 4], [1, 2]])
  # end

  # test "nested list cons operator in a map" do
  #   assert match?(%{nested: [2 | [2 | _rest]]}, %{nested: [2, 3, 4]})
  # end

  # test "unmatched cons" do
  #   assert match?([1 | [2, 3]], %{a: 1})
  # end
end
