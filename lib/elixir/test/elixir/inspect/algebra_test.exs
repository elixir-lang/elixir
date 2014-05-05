Code.require_file "../test_helper.exs", __DIR__

defmodule Inspect.AlgebraTest do
  use ExUnit.Case, async: true

  import Inspect.Algebra

  def helloabcd do
    concat(
      glue(
        glue(
          glue("hello", "a"),
          "b"),
        "c"),
      "d")
  end

  def factor(doc, w), do: format(w, 0, [{0, :flat, group(doc)}])

  test "empty doc" do
    # Consistence with definitions
    assert empty == :doc_nil

    # Consistence of corresponding sdoc
    assert factor(empty, 80) == []

    # Consistent formatting
    assert pretty(empty, 80) == ""
  end

  test "break doc" do
    # Consistence with definitions
    assert break("break") == {:doc_break, "break"}
    assert break("") == {:doc_break, ""}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> break(42) end

    # Consistence of corresponding sdoc
    assert factor(break("_"), 80) == ["_"]

    # Consistent formatting
    assert pretty(break("_"), 80) == "_"
  end

  test "glue doc" do
    # Consistence with definitions
    assert glue("a", "->", "b") == {:doc_cons,
      "a", {:doc_cons, {:doc_break, "->"}, "b"}
   }
    assert glue("a", "b") == glue("a", " ", "b")

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> glue("a", 42, "b") end
  end

  test "text doc" do
    # Consistence of corresponding sdoc
    assert factor("_", 80) == ["_"]

    # Consistent formatting
    assert pretty("_", 80) == "_"
  end

  test "space doc" do
    # Consistency with definitions
    assert space("a", "b") == {:doc_cons,
      "a", {:doc_cons, " ", "b"}
   }
  end

  test "nest doc" do
    # Consistence with definitions
    assert nest(empty, 1) == {:doc_nest, 1, empty}
    assert nest(empty, 0) == :doc_nil

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty) end

    # Consistence of corresponding sdoc
    assert factor(nest("a", 1), 80)  == ["a"]
    assert format(2, 0, [{0, :break, nest(glue("a", "b"), 1)}]) == ["a", "\n ", "b"]

    # Consistent formatting
    assert pretty(nest("a", 1), 80) == "a"
    assert render(format 2, 0, [{0, :break, nest(glue("a", "b"), 1)}]) == "a\n b"
  end

  test "line doc" do
    # Consistency with definitions
    assert line("a", "b") ==
      {:doc_cons, "a", {:doc_cons, :doc_line, "b"}}

    # Consistence of corresponding sdoc
    assert factor(line("a", "b"), 1) == ["a", "\n", "b"]
    assert factor(line("a", "b"), 9) == ["a", "\n", "b"]

    # Consistent formatting
    assert pretty(line(glue("aaa", "bbb"), glue("ccc", "ddd")), 10) ==
           "aaa bbb\nccc ddd"
  end

  test "group doc" do
    # Consistency with definitions
    assert group(glue("a", "b")) ==
      {:doc_group, {:doc_cons, "a", concat(break, "b")}}
    assert group(empty) == {:doc_group, empty}

    # Consistence of corresponding sdoc
    assert factor(glue("a", "b"), 1) == ["a", " ", "b"]
    assert factor(glue("a", "b"), 9) == ["a", " ", "b"]

    # Consistent formatting
    assert pretty(helloabcd, 5) == "hello\na b\ncd"
    assert pretty(helloabcd, 80) == "hello a b cd"
  end

  test "formatting with infinity" do
    s = String.duplicate "x", 50
    g = ";"
    doc = group(glue(s, g, s) |>  glue(g, s) |>  glue(g, s) |> glue(g, s))

    assert pretty(doc, :infinity) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end
end
