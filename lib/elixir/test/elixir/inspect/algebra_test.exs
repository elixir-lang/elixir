Code.require_file "../test_helper.exs", __DIR__

defmodule Inspect.AlgebraTest do
  use ExUnit.Case, async: true

  doctest Inspect.Algebra

  import Inspect.Algebra

  defp render(doc, limit) do
    format(doc, limit) |> IO.iodata_to_binary
  end

  test "empty doc" do
    # Consistent with definitions
    assert empty() == :doc_nil

    # Consistent formatting
    assert render(empty(), 80) == ""
  end

  test "break doc" do
    # Consistent with definitions
    assert break("break") == {:doc_break, "break"}
    assert break("") == {:doc_break, ""}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> break(42) end

    # Consistent formatting
    assert render(break("_"), 80) == "_"
  end

  test "glue doc" do
    # Consistent with definitions
    assert glue("a", "->", "b") == {:doc_cons,
      "a", {:doc_cons, {:doc_break, "->"}, "b"}
   }
    assert glue("a", "b") == glue("a", " ", "b")

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> glue("a", 42, "b") end
  end

  test "text doc" do
    assert render("_", 80) == "_"
  end

  test "space doc" do
    # Consistent with definitions
    assert space("a", "b") == {:doc_cons,
      "a", {:doc_cons, " ", "b"}
   }
  end

  test "nest doc" do
    # Consistent with definitions
    assert nest(empty(), 1) == {:doc_nest, empty(), 1}
    assert nest(empty(), 0) == :doc_nil

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty()) end

    # Consistent formatting
    assert render(nest("a", 1), 80) == "a"
    assert render(nest(glue("a", "b"), 1), 2) == "a\n b"
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
    assert line("a", "b") ==
      {:doc_cons, "a", {:doc_cons, :doc_line, "b"}}

    # Consistent formatting
    assert render(line(glue("aaa", "bbb"), glue("ccc", "ddd")), 10) ==
           "aaa bbb\nccc ddd"
  end

  test "group doc" do
    # Consistent with definitions
    assert group(glue("a", "b")) ==
           {:doc_group, {:doc_cons, "a", concat(break(), "b")}, :flex}
    assert group(empty()) == {:doc_group, empty(), :flex}

    # Consistent formatting
    doc = concat(glue(glue(glue("hello", "a"), "b"), "c"), "d")
    assert render(group(doc, :flex), 5) == "hello\na b\ncd"
    assert render(group(doc, :strict), 5) == "hello\na\nb\ncd"

    # Breaks if one of the line in the group break
    doc = line(glue("a", "b"), glue("hello", "world"))
    assert render(group(doc, :flex), 5) == "a b\nhello\nworld"
    assert render(group(doc, :strict), 5) == "a\nb\nhello\nworld"
  end

  test "formatting with infinity" do
    s = String.duplicate "x", 50
    g = ";"
    doc = glue(s, g, s) |>  glue(g, s) |>  glue(g, s) |> glue(g, s) |> group

    assert render(doc, :infinity) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end

  test "formatting surround_many with empty" do
    sm = &surround_many("[", &1, "]", %Inspect.Opts{}, fn(d, _) -> d end, ",")

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

  test "surround_many with docs as the wrappers and as the separator" do
    opts = %Inspect.Opts{}
    fun = fn(d, _) -> d end

    doc = surround_many(break("["), ["a", "b", "c"], break("]"), opts, fun, break(","))
    assert render(doc, 80) == "[a, b, c]"
    assert render(doc, 5) == "[a, b\n  c]"
  end
end
