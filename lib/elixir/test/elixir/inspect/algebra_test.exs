Code.require_file("../test_helper.exs", __DIR__)

defmodule Inspect.AlgebraTest do
  use ExUnit.Case, async: true

  doctest Inspect.Algebra

  import Inspect.Algebra

  defp render(doc, limit) do
    doc |> group() |> format(limit) |> IO.iodata_to_binary()
  end

  test "empty doc" do
    # Consistent with definitions
    assert empty() == :doc_nil

    # Consistent formatting
    assert render(empty(), 80) == ""
  end

  test "strict break doc" do
    # Consistent with definitions
    assert break("break") == {:doc_break, "break", :strict}
    assert break("") == {:doc_break, "", :strict}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> break(42) end

    # Consistent formatting
    assert render(break("_"), 80) == "_"
    assert render(glue("foo", " ", glue("bar", " ", "baz")), 10) == "foo\nbar\nbaz"
  end

  test "flex break doc" do
    # Consistent with definitions
    assert flex_break("break") == {:doc_break, "break", :flex}
    assert flex_break("") == {:doc_break, "", :flex}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> flex_break(42) end

    # Consistent formatting
    assert render(flex_break("_"), 80) == "_"
    assert render(flex_glue("foo", " ", flex_glue("bar", " ", "baz")), 10) == "foo bar\nbaz"
  end

  test "glue doc" do
    # Consistent with definitions
    assert glue("a", "->", "b") == {:doc_cons, "a", {:doc_cons, {:doc_break, "->", :strict}, "b"}}
    assert glue("a", "b") == glue("a", " ", "b")

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> glue("a", 42, "b") end
  end

  test "flex glue doc" do
    # Consistent with definitions
    assert flex_glue("a", "->", "b") ==
             {:doc_cons, "a", {:doc_cons, {:doc_break, "->", :flex}, "b"}}

    assert flex_glue("a", "b") == flex_glue("a", " ", "b")

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> flex_glue("a", 42, "b") end
  end

  test "binary doc" do
    assert render("_", 80) == "_"
  end

  test "string doc" do
    # Consistent with definitions
    assert string("ólá") == {:doc_string, "ólá", 3}

    # Counts graphemes
    doc = glue(string("olá"), " ", string("mundo"))
    assert render(doc, 9) == "olá mundo"
  end

  test "space doc" do
    # Consistent with definitions
    assert space("a", "b") == {:doc_cons, "a", {:doc_cons, " ", "b"}}
  end

  test "always nest doc" do
    # Consistent with definitions
    assert nest(empty(), 1) == {:doc_nest, empty(), 1, :always}
    assert nest(empty(), 0) == :doc_nil

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty()) end

    # Consistent formatting
    assert render(nest("a", 1), 80) == "a"
    assert render(nest(glue("a", "b"), 1), 2) == "a\n b"
    assert render(nest(line("a", "b"), 1), 20) == "a\n b"
  end

  test "break nest doc" do
    # Consistent with definitions
    assert nest(empty(), 1, :break) == {:doc_nest, empty(), 1, :break}
    assert nest(empty(), 0, :break) == :doc_nil

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty(), :break) end

    # Consistent formatting
    assert render(nest("a", 1, :break), 80) == "a"
    assert render(nest(glue("a", "b"), 1, :break), 2) == "a\n b"
    assert render(nest(line("a", "b"), 1, :break), 20) == "a\nb"
  end

  test "cursor nest doc" do
    # Consistent with definitions
    assert nest(empty(), :cursor) == {:doc_nest, empty(), :cursor, :always}

    # Consistent formatting
    assert render(nest("a", :cursor), 80) == "a"
    assert render(concat("prefix ", nest(glue("a", "b"), :cursor)), 2) == "prefix a\n       b"
    assert render(concat("prefix ", nest(line("a", "b"), :cursor)), 2) == "prefix a\n       b"
  end

  test "reset nest doc" do
    # Consistent with definitions
    assert nest(empty(), :cursor) == {:doc_nest, empty(), :cursor, :always}

    # Consistent formatting
    assert render(nest("a", :cursor), 80) == "a"
    assert render(nest(nest(glue("a", "b"), :reset), 10), 2) == "a\nb"
    assert render(nest(nest(line("a", "b"), :reset), 10), 2) == "a\nb"
  end

  test "color doc" do
    # Consistent with definitions
    opts = %Inspect.Opts{}
    assert color(empty(), :atom, opts) == empty()

    opts = %Inspect.Opts{syntax_colors: [regex: :red]}
    assert color(empty(), :atom, opts) == empty()

    opts = %Inspect.Opts{syntax_colors: [atom: :red]}
    doc1 = {:doc_color, "Hi", :red}
    doc2 = {:doc_color, empty(), :reset}
    assert color("Hi", :atom, opts) == concat(doc1, doc2)

    opts = %Inspect.Opts{syntax_colors: [reset: :red]}
    assert color(empty(), :atom, opts) == empty()

    opts = %Inspect.Opts{syntax_colors: [number: :cyan, reset: :red]}
    doc1 = {:doc_color, "123", :cyan}
    doc2 = {:doc_color, empty(), :red}
    assert color("123", :number, opts) == concat(doc1, doc2)

    # Consistent formatting
    opts = %Inspect.Opts{syntax_colors: [atom: :cyan]}
    assert render(glue(color("AA", :atom, opts), "BB"), 5) == "\e[36mAA\e[0m BB"
    assert render(glue(color("AA", :atom, opts), "BB"), 3) == "\e[36mAA\e[0m\nBB"
    assert render(glue("AA", color("BB", :atom, opts)), 6) == "AA \e[36mBB\e[0m"
  end

  test "line doc" do
    # Consistent with definitions
    assert line("a", "b") == {:doc_cons, "a", {:doc_cons, :doc_line, "b"}}

    # Consistent formatting
    assert render(line(glue("aaa", "bbb"), glue("ccc", "ddd")), 10) == "aaa bbb\nccc ddd"
  end

  test "group doc" do
    # Consistent with definitions
    assert group("ab") == {:doc_group, "ab", :self}
    assert group(empty()) == {:doc_group, empty(), :self}

    # Consistent formatting
    doc = concat(glue(glue(glue("hello", "a"), "b"), "c"), "d")
    assert render(group(doc), 5) == "hello\na\nb\ncd"
  end

  test "group doc with inherit" do
    # Consistent with definitions
    assert group("ab", :inherit) == {:doc_group, "ab", :inherit}
    assert group(empty(), :inherit) == {:doc_group, empty(), :inherit}

    # Consistent formatting
    doc = concat(glue(glue(group(glue("a", "b"), :self), "c"), "d"), "hello")
    assert render(group(doc), 5) == "a b\nc\ndhello"

    doc = concat(glue(glue(group(glue("a", "b"), :inherit), "c"), "d"), "hello")
    assert render(group(doc), 5) == "a\nb\nc\ndhello"
  end

  test "collapse lines" do
    # Consistent with definitions
    assert collapse_lines(3) == {:doc_collapse, 3}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> collapse_lines(0) end
    assert_raise FunctionClauseError, fn -> collapse_lines(empty()) end

    # Consistent formatting
    doc = concat([collapse_lines(2), line(), line(), line()])
    assert render(doc, 10) == "\n\n"
    assert render(nest(doc, 2), 10) == "\n\n  "

    doc = concat([collapse_lines(2), line(), line()])
    assert render(doc, 10) == "\n\n"
    assert render(nest(doc, 2), 10) == "\n\n  "

    doc = concat([collapse_lines(2), line()])
    assert render(doc, 10) == "\n"
    assert render(nest(doc, 2), 10) == "\n  "

    doc = concat([collapse_lines(2), line(), "", line(), "", line()])
    assert render(doc, 10) == "\n\n"
    assert render(nest(doc, 2), 10) == "\n\n  "

    doc = concat([collapse_lines(2), line(), "foo", line(), "bar", line()])
    assert render(doc, 10) == "\nfoo\nbar\n"
    assert render(nest(doc, 2), 10) == "\n  foo\n  bar\n  "
  end

  test "force doc and cancel doc" do
    # Consistent with definitions
    assert force_unfit("ab") == {:doc_force, "ab"}
    assert force_unfit(empty()) == {:doc_force, empty()}

    # Consistent with definitions
    assert next_break_fits("ab") == {:doc_fits, "ab", :enabled}
    assert next_break_fits(empty()) == {:doc_fits, empty(), :enabled}
    assert next_break_fits("ab", :disabled) == {:doc_fits, "ab", :disabled}
    assert next_break_fits(empty(), :disabled) == {:doc_fits, empty(), :disabled}

    # Consistent formatting
    doc = force_unfit(concat(glue(glue(glue("hello", "a"), "b"), "c"), "d"))
    assert render(doc, 20) == "hello\na\nb\ncd"
    assert render(next_break_fits(doc, :enabled), 20) == "hello a b cd"

    assert render(next_break_fits(next_break_fits(doc, :enabled), :disabled), 20) ==
             "hello\na\nb\ncd"
  end

  test "formatting groups with lines" do
    doc = line(glue("a", "b"), glue("hello", "world"))
    assert render(group(doc), 5) == "a\nb\nhello\nworld"
    assert render(group(doc), 100) == "a b\nhello world"
  end

  test "formatting with infinity" do
    str = String.duplicate("x", 50)
    colon = ";"

    doc =
      str
      |> glue(colon, str)
      |> glue(colon, str)
      |> glue(colon, str)
      |> glue(colon, str)
      |> group()

    assert render(doc, :infinity) ==
             str <> colon <> str <> colon <> str <> colon <> str <> colon <> str
  end

  test "formatting container_doc with empty" do
    sm = &container_doc("[", &1, "]", %Inspect.Opts{}, fn d, _ -> d end, separator: ",")

    assert sm.([]) |> render(80) == "[]"
    assert sm.([empty()]) |> render(80) == "[]"
    assert sm.([empty(), empty()]) |> render(80) == "[]"
    assert sm.(["a"]) |> render(80) == "[a]"
    assert sm.(["a", empty()]) |> render(80) == "[a]"
    assert sm.([empty(), "a"]) |> render(80) == "[a]"
    assert sm.(["a", empty(), "b"]) |> render(80) == "[a, b]"
    assert sm.([empty(), "a", "b"]) |> render(80) == "[a, b]"
    assert sm.(["a", "b", empty()]) |> render(80) == "[a, b]"
    assert sm.(["a", "b" | "c"]) |> render(80) == "[a, b | c]"
    assert sm.(["a" | "b"]) |> render(80) == "[a | b]"
    assert sm.(["a" | empty()]) |> render(80) == "[a]"
    assert sm.([empty() | "b"]) |> render(80) == "[b]"
  end

  defmodule Custom do
    defstruct []
  end

  test "put_inspect_fun/1" do
    fun = fn
      %Custom{}, _opts ->
        "#Custom<>"

      term, opts ->
        Inspect.Algebra.to_doc(term, opts)
    end

    Inspect.Algebra.put_inspect_fun(fun)
    assert Inspect.Algebra.get_inspect_fun() == fun
    assert inspect(%Custom{}) == "#Custom<>"
    assert inspect([%Custom{}]) == "[#Custom<>]"
  after
    Inspect.Algebra.put_inspect_fun(&Inspect.Algebra.to_doc/2)
  end
end
