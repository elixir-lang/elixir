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

  test :empty do
    # Consistence with definitions
    assert empty == :doc_nil
    # Consistence of corresponding sdoc
    assert factor(empty, 80) == []
    # Consistent formatting
    assert pretty(empty, 80) == ""
  end

  test :break do
    # Consistence with definitions
    ## Normal case
    assert break("break") == { :doc_break, "break" }
    ## Degeneracy
    assert break("") == { :doc_break, "" }
    ## ong argument type
    assert_raise FunctionClauseError, fn -> break(42) end
    # Consistence of corresponding sdoc
    assert factor(break("_"), 80) == ["_"]
    # Consistent formatting
    assert pretty(break("_"), 80) == "_"
  end

  test :glue do
    # Consistence with definitions
    assert glue("a", "->", "b") == { :doc_cons,
      "a", { :doc_cons, { :doc_break, "->" }, "b" }
    }
    assert glue("a", "b") == glue("a", " ", "b")

    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> glue("a", 42, "b") end
  end

  test :text do
    # Consistence of corresponding docfactor
    assert factor("_", 80) == ["_"]
    # Consistent formatting
    assert pretty("_", 80) == "_"
  end

  test :space do
    # Consistency with definitions
    assert space("a", "b") == { :doc_cons,
      "a", { :doc_cons, " ", "b" }
    }
  end

  test :nest do
    # Consistence with definitions
    ## Normal case
    assert nest(empty, 1) == { :doc_nest, 1, empty }
    ## Degeneracy
    assert nest(empty, 0) == :doc_nil
    ## ong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty) end

    a1   = fn -> nest("a", 1) end
    alb1 = fn -> nest(glue("a", "b"), 1) end
    # Consistence of corresponding sdoc
    ## Trivial case
    assert factor(a1.(), 80)  == ["a"]
    ## Correctly indenting line forcing linebreak
    assert format(2, 0, [{0, :break, alb1.()}]) == ["a", "\n", " ", "b"]

    # Consistent formatting
    ## Trivial case
    assert pretty(a1.(), 80) == "a"
    ## Correctly indenting line
    assert render(format 2, 0, [{0, :break, alb1.()}]) == "a\n b"
  end

  test :infinity do
    # w = :infinity should disable pretty printer
    s = String.duplicate "x", 50
    g = ";"
    big_document = group(glue(s, g, s) |>  glue(g, s) |>  glue(g, s) |> glue(g, s))

    assert pretty(big_document, :infinity) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end

  test :group do
    # Consistency with definitions
    ## Normal case
    assert group(glue("a", "b")) ==
      { :doc_group, { :doc_cons, "a", concat(break, "b") }}

    ## Degeneracy
    assert group(empty) == { :doc_group, empty }

    # Consistence of corresponding sdoc
    assert factor(glue("a", "b"), 1) == ["a", " ", "b"]
    assert factor(glue("a", "b"), 9) == ["a", " ", "b"]

    # Consistent formatting
    assert pretty(helloabcd, 5) == "hello\na b\ncd"
    assert pretty(helloabcd, 80) == "hello a b cd"
  end
end
