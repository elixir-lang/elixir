Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.DiffTest do
  use ExUnit.Case, async: true

  import ExUnit.Diff

  alias Inspect.Algebra
  alias ExUnit.Assertions

  defmodule User do
    defstruct [:age]
  end

  defmodule Person do
    defstruct [:age]
  end

  defmodule Opaque do
    defstruct [:data]

    defimpl Inspect do
      def inspect(_, _) do
        "#Opaque<???>"
      end
    end
  end

  defmacrop one, do: 1

  defmacrop tuple(a, b) do
    quote do
      {unquote(a), unquote(b)}
    end
  end

  defmacrop pin_x do
    x = Macro.var(:x, nil)
    quote(do: ^unquote(x))
  end

  defmacrop assert_diff({:=, _, [a, b]}, pins \\ [], binding \\ []) do
    a = Assertions.expand_pattern(a, __CALLER__) |> Macro.escape()

    quote do
      assert_diff(unquote(a), unquote(b), unquote(pins), unquote(binding), :match)
    end
  end

  defmacrop refute_diff({:=, _, [a, b]}, left_diff, right_diff, pins \\ []) do
    a = Assertions.expand_pattern(a, __CALLER__) |> Macro.escape()

    quote do
      refute_diff(
        unquote(a),
        unquote(b),
        unquote(left_diff),
        unquote(right_diff),
        unquote(pins),
        :match
      )
    end
  end

  test "atoms" do
    assert_diff(:a = :a)
    assert_diff(:"$a" = :"$a")

    refute_diff(:a = :b, "-:a-", "+:b+")
    refute_diff(:a = :aa, "-:a-", "+:aa+")

    refute_diff(:"$" = :"$a", ~s[-:"$"-], ~s[+:"$a"+])
    refute_diff(:"$a" = :"$b", ~s[-:"$a"-], ~s[+:"$b"+])

    refute_diff(:bar = 42, "-:bar-", "+42+")
    refute_diff(42 = :bar, "-42-", "+:bar+")

    pins = [a: :a, b: :b]

    assert_diff(x = :a, pins, x: :a)
    assert_diff(^a = :a, pins)
    assert_diff(^b = :b, pins)

    refute_diff(^a = :b, "-^a-", "+:b+", pins)
    refute_diff(^b = :a, "-^b-", "+:a+", pins)
  end

  test "integers" do
    assert_diff(123 = 123)
    assert_diff(-123 = -123)
    assert_diff(123 = +123)
    assert_diff(+123 = 123)

    refute_diff(12 = 13, "1-2-", "1+3+")

    refute_diff(12345 = 123, "123-45-", "123")
    refute_diff(123 = 12345, "123", "123+45+")

    refute_diff(12345 = 345, "-12-345", "345")
    refute_diff(345 = 12345, "345", "+12+345")

    refute_diff(123 = -123, "123", "+-+123")
    refute_diff(-123 = 123, "---123", "123")

    refute_diff(491_512_235 = 490_512_035, "49-1-512-2-35", "49+0+512+0+35")

    assert_diff(0xF = 15)

    refute_diff(0xF = 16, "1-5-", "1+6+")

    refute_diff(123 = :a, "-123-", "+:a+")
  end

  test "floats" do
    assert_diff(123.0 = 123.0)
    assert_diff(-123.0 = -123.0)
    assert_diff(123.0 = +123.0)
    assert_diff(+123.0 = 123.0)

    refute_diff(1.2 = 1.3, "1.-2-", "1.+3+")

    refute_diff(12.345 = 12.3, "12.3-45-", "12.3")
    refute_diff(12.3 = 12.345, "12.3", "12.3+45+")

    refute_diff(123.45 = 3.45, "-12-3.45", "3.45")
    refute_diff(3.45 = 123.45, "3.45", "+12+3.45")

    refute_diff(1.23 = -1.23, "1.23", "+-+1.23")
    refute_diff(-1.23 = 1.23, "---1.23", "1.23")

    refute_diff(123.0 = :a, "-123.0-", "+:a+")
    refute_diff(123.0 = 123_512_235, "-123.0-", "+123512235+")
  end

  test "lists" do
    assert_diff([] = [])

    refute_diff([] = [:a], "[]", "[+:a+]")
    refute_diff([:a] = [], "[-:a-]", "[]")

    assert_diff([:a] = [:a])
    assert_diff([:a, :b, :c] = [:a, :b, :c])

    refute_diff([:a] = [:b], "[-:a-]", "[+:b+]")

    refute_diff([:a, :d, :b, :c] = [:a, :b, :c, :d], "[:a, -:d-, :b, :c]", "[:a, :b, :c, +:d+]")

    refute_diff(
      [:a, [:d, :b, :c]] = [:a, [:b, :c, :d]],
      "[:a, [-:d-, :b, :c]]",
      "[:a, [:b, :c, +:d+]]"
    )

    refute_diff([:a, :b, :c] = [:a, :b, :x], "[:a, :b, -:c-]", "[:a, :b, +:x+]")
    refute_diff([:a, :x, :c] = [:a, :b, :c], "[:a, -:x-, :c]", "[:a, +:b+, :c]")

    refute_diff([:a, :b, :c] = [:a, :b, []], "[:a, :b, -:c-]", "[:a, :b, +[]+]")
    refute_diff([:a, :b, []] = [:a, :b, :c], "[:a, :b, -[]-]", "[:a, :b, +:c+]")

    refute_diff([:a, :b, :c] = [:a, :b], "[:a, :b, -:c-]", "[:a, :b]")
    refute_diff([:a, :b] = [:a, :b, :c], "[:a, :b]", "[:a, :b, +:c+]")

    refute_diff([:a, :b, :c, :d, :e] = [:a, :b], "[:a, :b, -:c-, -:d-, -:e-]", "[:a, :b]")
    refute_diff([:a, :b] = [:a, :b, :c, :d, :e], "[:a, :b]", "[:a, :b, +:c+, +:d+, +:e+]")

    refute_diff(
      [:e, :a, :b, :c, :d] = [:a, :b, :c, :d, :e],
      "[-:e-, :a, :b, :c, :d]",
      "[:a, :b, :c, :d, +:e+]"
    )

    refute_diff([:a, [:c, :b]] = [:a, [:b, :c]], "[:a, [-:c-, :b]]", "[:a, [:b, +:c+]]")

    refute_diff(:a = [:a, [:b, :c]], "-:a-", "+[:a, [:b, :c]]+")

    pins = [a: :a, b: :b, list_ab: [:a, :b]]

    assert_diff(x = [], pins, x: [])
    assert_diff(x = [:a, :b], pins, x: [:a, :b])
    assert_diff([x] = [:a], pins, x: :a)
    assert_diff([x, :b, :c] = [:a, :b, :c], pins, x: :a)
    assert_diff([x, y, z] = [:a, :b, :c], pins, x: :a, y: :b, z: :c)
    assert_diff([x, x, :c] = [:a, :a, :c], pins, x: :a)

    refute_diff([x] = [], "[-x-]", "[]")
    refute_diff([x, :b, :c] = [:a, :b, :x], "[x, :b, -:c-]", "[:a, :b, +:x+]")
    refute_diff([x, x, :c] = [:a, :b, :c], "[x, -x-, :c]", "[:a, +:b+, :c]")

    assert_diff(^list_ab = [:a, :b], pins)
    assert_diff([^a, :b, :c] = [:a, :b, :c], pins)
    assert_diff([^a, ^b, :c] = [:a, :b, :c], pins)
    assert_diff([^a, a, :c] = [:a, :b, :c], pins, a: :b)
    assert_diff([b, ^b, :c] = [:a, :b, :c], pins, b: :a)

    refute_diff(^list_ab = [:x, :b], "-^list_ab-", "[+:x+, :b]", pins)
    refute_diff([^a, :b, :c] = [:a, :b, :x], "[^a, :b, -:c-]", "[:a, :b, +:x+]", pins)
    refute_diff([:a, ^a, :c] = [:a, :b, :c], "[:a, -^a-, :c]", "[:a, +:b+, :c]", pins)

    refute_diff(
      [x, :a, :b, :c, :d] = [:a, :b, :c, :d, :e],
      "[x, -:a-, :b, :c, :d]",
      "[:a, :b, :c, :d, +:e+]"
    )

    refute_diff([:a, :b] = :a, "-[:a, :b]-", "+:a+")
  end

  test "improper lists" do
    assert_diff([:a | :b] = [:a | :b])
    assert_diff([:a, :b | :c] = [:a, :b | :c])

    refute_diff([:a | :b] = [:b | :a], "[-:a- | -:b-]", "[+:b+ | +:a+]")
    refute_diff([:a | :b] = [:a | :x], "[:a | -:b-]", "[:a | +:x+]")

    refute_diff([:a, :b | :c] = [:a, :b | :x], "[:a, :b | -:c-]", "[:a, :b | +:x+]")
    refute_diff([:a, :x | :c] = [:a, :b | :c], "[:a, -:x- | :c]", "[:a, +:b+ | :c]")
    refute_diff([:x, :b | :c] = [:a, :b | :c], "[-:x-, :b | :c]", "[+:a+, :b | :c]")
    refute_diff([:c, :a | :b] = [:a, :b | :c], "[-:c-, :a | -:b-]", "[:a, +:b+ | +:c+]")

    refute_diff(
      [:a, :c, :x | :b] = [:a, :b, :c | :d],
      "[:a, :c, -:x- | -:b-]",
      "[:a, +:b+, :c | +:d+]"
    )

    refute_diff([:a | :d] = [:a, :b, :c | :d], "[:a | :d]", "[:a, +:b+, +:c+ | :d]")

    refute_diff(
      [[:a | :x], :x | :d] = [[:a | :b], :c | :d],
      "[[:a | -:x-], -:x- | :d]",
      "[[:a | +:b+], +:c+ | :d]"
    )

    assert_diff([:a | x] = [:a | :b], [], x: :b)
  end

  test "proper lists" do
    assert_diff([:a | [:b]] = [:a, :b])
    assert_diff([:a | [:b, :c]] = [:a, :b, :c])

    refute_diff([:a | [:b]] = [:a, :x], "[:a | [-:b-]]", "[:a, +:x+]")

    refute_diff([:a, :b | [:c]] = [:a, :b, :x], "[:a, :b | [-:c-]]", "[:a, :b, +:x+]")
    refute_diff([:a, :x | [:c]] = [:a, :b, :c], "[:a, -:x- | [:c]]", "[:a, +:b+, :c]")
    refute_diff([:a | [:b, :c]] = [:a, :b, :x], "[:a | [:b, -:c-]]", "[:a, :b, +:x+]")
    refute_diff([:a | [:b, :c]] = [:x, :b, :c], "[-:a- | [:b, :c]]", "[+:x+, :b, :c]")

    refute_diff(
      [:a, :c, :x | [:b, :c]] = [:a, :b, :c, :d, :e],
      "[:a, -:c-, -:x- | [:b, :c]]",
      "[:a, :b, :c, +:d+, +:e+]"
    )

    refute_diff([:a, :b | [:c]] = [:a, :b], "[:a, :b | [-:c-]]", "[:a, :b]")
    refute_diff([:a, :b | []] = [:a, :b, :c], "[:a, :b | []]", "[:a, :b, +:c+]")

    refute_diff([:a, :b | [:c, :d]] = [:a, :b, :c], "[:a, :b | [:c, -:d-]]", "[:a, :b, :c]")
    refute_diff([:a, :b | [:c, :d]] = [:a], "[:a, -:b- | [-:c-, -:d-]]", "[:a]")

    refute_diff(
      [:a, [:b, :c] | [:d, :e]] = [:a, [:x, :y], :d, :e],
      "[:a, [-:b-, -:c-] | [:d, :e]]",
      "[:a, [+:x+, +:y+], :d, :e]"
    )

    refute_diff(
      [:a, [:b, :c] | [:d, :e]] = [:a, [:x, :c], :d, :e],
      "[:a, [-:b-, :c] | [:d, :e]]",
      "[:a, [+:x+, :c], :d, :e]"
    )

    pins = [list_bc: [:b, :c]]

    assert_diff([:a | x] = [:a, :b], pins, x: [:b])
    assert_diff([:a | x] = [:a, :b, :c], pins, x: [:b, :c])
    assert_diff([:a | ^list_bc] = [:a, :b, :c], pins)

    refute_diff([:a | ^list_bc] = [:x, :x, :c], "[-:a- | -^list_bc-]", "[+:x+, +:x+, :c]", pins)
    refute_diff([:a | ^list_bc] = [:a, :x, :c], "[:a | -^list_bc-]", "[:a, +:x+, :c]", pins)
  end

  test "concat lists" do
    assert_diff([:a] ++ [:b] = [:a, :b])
    assert_diff([:a, :b] ++ [] = [:a, :b])
    assert_diff([] ++ [:a, :b] = [:a, :b])

    refute_diff([:a, :b] ++ [:c] = [:a, :b], "[:a, :b] ++ [-:c-]", "[:a, :b]")
    refute_diff([:a, :c] ++ [:b] = [:a, :b], "[:a, -:c-] ++ [:b]", "[:a, :b]")

    refute_diff([:a] ++ [:b] ++ [:c] = [:a, :b], "[:a] ++ [:b] ++ [-:c-]", "[:a, :b]")

    assert_diff([:a] ++ :b = [:a | :b])
    assert_diff([:a] ++ x = [:a, :b], [], x: [:b])

    refute_diff([:a, :b] ++ :c = [:a, :b, :c], "[:a, :b] ++ -:c-", "[:a, :b, +:c+]")
    refute_diff([:a] ++ [:b] ++ :c = [:a, :b, :c], "[:a] ++ [:b] ++ -:c-", "[:a, :b, +:c+]")

    refute_diff([:a] ++ [:b] = :a, "-[:a] ++ [:b]-", "+:a+")
  end

  test "mixed lists" do
    refute_diff([:a | :b] = [:a, :b], "[:a | -:b-]", "[:a, +:b+]")
    refute_diff([:a, :b] = [:a | :b], "[:a, -:b-]", "[:a | +:b+]")
    refute_diff([:a | [:b]] = [:a | :b], "[:a | -[:b]-]", "[:a | +:b+]")

    refute_diff([:a | [:b | [:c]]] = [:a | :c], "[:a | [-:b- | -[:c]-]]", "[:a | +:c+]")

    refute_diff([:a | :b] = [:a, :b, :c], "[:a | -:b-]", "[:a, +:b+, +:c+]")
    refute_diff([:a, :b, :c] = [:a | :b], "[:a, -:b-, -:c-]", "[:a | +:b+]")

    refute_diff([:a | [:b] ++ [:c]] = [:a, :b], "[:a | [:b] ++ [-:c-]]", "[:a, :b]")

    refute_diff(
      [:a | [:b] ++ [:c]] ++ [:d | :e] = [:a, :b | :e],
      "[:a | [:b] ++ [-:c-]] ++ [-:d- | :e]",
      "[:a, :b | :e]"
    )
  end

  test "keyword lists" do
    assert_diff([file: "nofile", line: 1] = [file: "nofile", line: 1])

    refute_diff(
      [file: "nofile", line: 1] = [file: nil, lime: 1],
      ~s/[file: -"nofile"-, -line:- 1]/,
      "[file: +nil+, +lime:+ 1]"
    )

    refute_diff(
      [file: nil, line: 1] = [file: "nofile"],
      "[file: -nil-, -line: 1-]",
      ~s/[file: +"nofile"+]/
    )

    refute_diff(
      ["foo-bar": 1] = [],
      ~s/[-"foo-bar": 1-]/,
      "[]"
    )

    refute_diff(
      [file: nil] = [{:line, 1}, {1, :foo}],
      "[-file:- -nil-]",
      "[{+:line+, +1+}, +{1, :foo}+]"
    )
  end

  test "tuples" do
    assert_diff({:a, :b} = {:a, :b})

    refute_diff({:a, :b} = {:a, :x}, "{:a, -:b-}", "{:a, +:x+}")
    refute_diff({:a, :b} = {:x, :x}, "{-:a-, -:b-}", "{+:x+, +:x+}")

    refute_diff({:a, :b, :c} = {:a, :b, :x}, "{:a, :b, -:c-}", "{:a, :b, +:x+}")

    refute_diff({:a} = {:a, :b}, "{:a}", "{:a, +:b+}")
    refute_diff({:a, :b} = {:a}, "{:a, -:b-}", "{:a}")

    refute_diff({:a, :b} = :a, "-{:a, :b}-", "+:a+")
  end

  test "maps" do
    assert_diff(%{a: 1} = %{a: 1})
    assert_diff(%{a: 1} = %{a: 1, b: 2})
    assert_diff(%{a: 1, b: 2} = %{a: 1, b: 2})
    assert_diff(%{b: 2, a: 1} = %{a: 1, b: 2})
    assert_diff(%{a: 1, b: 2, c: 3} = %{a: 1, b: 2, c: 3})
    assert_diff(%{c: 3, b: 2, a: 1} = %{a: 1, b: 2, c: 3})

    refute_diff(%{a: 1, b: 2} = %{a: 1}, "%{a: 1, -b: 2-}", "%{a: 1}")
    refute_diff(%{a: 1, b: 2} = %{a: 1, b: 12}, "%{a: 1, b: 2}", "%{a: 1, b: +1+2}")
    refute_diff(%{a: 1, b: 2} = %{a: 1, c: 2}, "%{a: 1, -b: 2-}", "%{a: 1, c: 2}")
    refute_diff(%{a: 1, b: 2} = %{a: 1, c: 2}, "%{a: 1, -b: 2-}", "%{a: 1, c: 2}")

    refute_diff(%{a: 1, b: 2, c: 3} = %{a: 1, b: 12}, "%{a: 1, b: 2, -c: 3-}", "%{a: 1, b: +1+2}")
    refute_diff(%{a: 1, b: 2, c: 3} = %{a: 1, c: 2}, "%{a: 1, -b: 2-, c: -3-}", "%{a: 1, c: +2+}")

    refute_diff(%{a: 1} = %{a: 2, b: 2, c: 3}, "%{a: -1-}", "%{a: +2+, b: 2, c: 3}")

    refute_diff(
      %{1 => :a, 2 => :b} = %{1 => :a, 12 => :b},
      "%{1 => :a, -2 => :b-}",
      "%{1 => :a, 12 => :b}"
    )

    refute_diff(
      %{1 => :a, 2 => :b} = %{1 => :a, :b => 2},
      "%{1 => :a, -2 => :b-}",
      "%{1 => :a, :b => 2}"
    )

    refute_diff(%{a: 1} = :a, "-%{a: 1}-", "+:a+")

    pins = [a: :a, b: :b]

    assert_diff(%{^a => 1} = %{a: 1}, pins)
    assert_diff(%{^a => x} = %{a: 1}, pins, x: 1)

    refute_diff(%{^a => 1, :a => 2} = %{a: 1}, "%{^a => 1, -:a => 2-}", "%{a: 1}", pins)

    refute_diff(
      %{^a => x, ^b => x} = %{a: 1, b: 2},
      "%{^a => x, ^b => -x-}",
      "%{a: 1, b: +2+}",
      pins
    )
  end

  test "structs" do
    assert_diff(%User{age: 16} = %User{age: 16})
    assert_diff(%User{age: 16} = %{age: 16})
    assert_diff(%{age: 16, __struct__: User} = %User{age: 16})

    refute_diff(
      %User{age: 16} = %User{age: 21},
      "%ExUnit.DiffTest.User{age: 1-6-}",
      "%ExUnit.DiffTest.User{age: +2+1}"
    )

    refute_diff(
      %User{age: 16} = %Person{age: 21},
      "%-ExUnit.DiffTest.User-{age: 1-6-}",
      "%+ExUnit.DiffTest.Person+{age: +2+1}"
    )

    refute_diff(
      %User{age: 16} = %Person{age: 21},
      "%-ExUnit.DiffTest.User-{age: 1-6-}",
      "%+ExUnit.DiffTest.Person+{age: +2+1}"
    )

    refute_diff(
      %User{age: 16} = %{age: 21},
      "%-ExUnit.DiffTest.User-{age: 1-6-}",
      "%{age: +2+1}"
    )

    refute_diff(
      %{age: 16, __struct__: Person} = %User{age: 16},
      "%-ExUnit.DiffTest.Person-{age: 16}",
      "%+ExUnit.DiffTest.User+{age: 16}"
    )

    pins = [tweety_one: 21]

    assert_diff(%User{age: ^tweety_one} = %User{age: 21}, pins)
    assert_diff(%User{age: age} = %User{age: 21}, pins, age: 21)

    refute_diff(
      %User{^tweety_one => 21} = %User{age: 21},
      "%ExUnit.DiffTest.User{-^tweety_one => 21-}",
      "%ExUnit.DiffTest.User{age: 21}",
      pins
    )
  end

  # test "structs with inspect" do
  #   refute_diff(
  #     ~D[2017-10-01],
  #     ~D[2017-10-02],
  #     "~D[2017-10-0-1-]",
  #     "~D[2017-10-0+2+]"
  #   )
  # end

  test "strings" do
    assert_diff("" = "")
    assert_diff("fox hops over the dog" = "fox hops over the dog")

    refute_diff("fox" = "foo", "fo-x-", "fo+o+")

    refute_diff(
      "fox hops over \"the dog" = "fox  jumps over the  lazy cat",
      ~s/"fox -ho-ps over -\\\"-the -dog-"/,
      ~s/"fox + jum+ps over the + lazy cat+"/
    )

    refute_diff(
      "short" = "really long string that should not emit diff against short",
      ~s/"-short-"/,
      ~s/"+really long string that should not emit diff against short+"/
    )

    refute_diff("foo" = :a, ~s/-"foo"-/, "+:a+")
  end

  test "concat operator" do
    assert_diff("fox hops" <> " over the dog" = "fox hops over the dog")
    assert_diff("fox hops " <> "over " <> "the dog" = "fox hops over the dog")

    refute_diff(
      "fox hops" <> " under the dog" = "fox hops over the dog",
      ~s/"fox hops" <> " -und-er the dog"/,
      ~s/"fox hops +ov+er the dog"/
    )

    refute_diff(
      "fox hops over" <> " the dog" = "fox hops over",
      ~s/"fox hops over" <> "- the dog-"/,
      ~s/"fox hops over"/
    )

    refute_diff(
      "fox hops" <> " over the dog" = "fox",
      ~s/"-fox hops-" <> "- over the dog-"/,
      ~s/"+fox+"/
    )

    refute_diff(
      "fox" <> " hops" = "fox h",
      ~s/"fox" <> " h-ops-"/,
      ~s/"fox h"/
    )

    refute_diff(
      "fox hops " <> "hover " <> "the dog" = "fox hops over the dog",
      ~s/"fox hops " <> "-h-over " <> "the dog"/,
      ~s/"fox hops over the dog"/
    )

    assert_diff("fox hops" <> x = "fox hops over the dog", [], x: " over the dog")
    assert_diff("fox hops " <> "over " <> x = "fox hops over the dog", [], x: "the dog")

    refute_diff(
      "fox hops " <> "hover " <> x = "fox hops over the dog",
      ~s/"fox hops " <> "-h-over " <> x/,
      ~s/"fox hops over +t+he dog"/
    )

    refute_diff("fox" <> " hops" = :a, ~s/-"fox" <> " hops"-/, "+:a+")
  end

  test "underscore" do
    assert_diff(_ = :a)
    assert_diff({_, _} = {:a, :b})

    refute_diff({_, :a} = {:b, :b}, "{_, -:a-}", "{:b, +:b+}")
  end

  test "macros" do
    assert_diff(one() = 1)
    assert_diff(tuple(x, x) = {1, 1}, [], x: 1)

    refute_diff(one() = 2, "-one()-", "+2+")
    refute_diff(tuple(x, x) = {1, 2}, "-tuple(x, x)-", "{1, +2+}")

    pins = [x: 1]

    assert_diff(pin_x() = 1, pins)
    refute_diff(pin_x() = 2, "-pin_x()-", "+2+", pins)
  end

  test "charlists" do
    refute_diff(
      'fox hops over \'the dog' = 'fox jumps over the lazy cat',
      "'fox -ho-ps over -\\'-the -dog-'",
      "'fox +jum+ps over the +lazy cat+'"
    )
  end

  # test "structs without inspect difference" do
  #   opaque1 = %Opaque{data: 1}
  #   opaque2 = %Opaque{data: 2}
  #
  #   assert script(opaque1, opaque2) == [
  #            {:eq, "%ExUnit.DiffTest.Opaque{"},
  #            [[{:eq, "data: "}, [del: "1", ins: "2"]]],
  #            {:eq, "}"}
  #          ]
  # end
  #
  # test "not supported" do
  #   bin1 = <<147, 1, 2, 31>>
  #   bin2 = <<193, 1, 31>>
  #   assert script(bin1, bin2) == nil
  #   assert script(:foo, :bar) == nil
  #   assert script(:foo, "bar") == nil
  # end

  defp refute_diff(a, b, left_diff, right_diff, pins, context) do
    {result, _env} = compare_quoted(a, b, pins, context)

    left =
      result.left
      |> to_algebra(&diff_wrapper(&1, "-"))
      |> Algebra.format(:infinity)
      |> IO.iodata_to_binary()

    right =
      result.right
      |> to_algebra(&diff_wrapper(&1, "+"))
      |> Algebra.format(:infinity)
      |> IO.iodata_to_binary()

    assert left =~ left_diff
    assert right =~ right_diff
  end

  defp assert_diff(a, b, pins, bindings, context) do
    {result, env} = compare_quoted(a, b, pins, context)

    env_binding = for {{name, _}, value} <- env.current_vars, do: {name, value}

    assert result.equivalent?
    assert env_binding == bindings
  end

  defp diff_wrapper(doc, side) do
    Algebra.concat([side, doc, side])
  end
end
