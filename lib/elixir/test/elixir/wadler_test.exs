Code.require_file "test_helper.exs", __DIR__

defmodule WadlerTest do
  use ExUnit.Case, async: true

  import Wadler

  def helloabcd do
    concat(
      glue(
        glue(
          glue(text("hello"), text("a")),
          text("b")),
        text("c")),
      text("d"))
  end

  def factor(doc, w), do: format(w, 0, [{0, :flat, group(doc)}])

  test :empty do
    # Consistence with definitions
    assert empty == :doc_nil
    # Consistence of corresponding sdoc
    assert factor(empty, 80) == :s_nil
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
    assert factor(break("_"), 80) == { :s_text, "_", :s_nil }
    # Consistent formatting
    assert pretty(break("_"), 80) == "_"
  end

  test :glue do
    # Consistence with definitions
    ## Normal case
    assert glue(text("a"), "->", text("b")) == { :doc_cons,
      { :doc_text, "a" },
      { :doc_cons, { :doc_break, "->" }, { :doc_text, "b" }}
    }
    assert glue(text("a"), text("b")) == glue(text("a"), " ", text("b"))

    ## ong argument type
    assert_raise FunctionClauseError, fn -> glue(text("a"), 42, text("b")) end
  end

  test :text do
    # Consistence with definitions
    ## Normal case
    assert text("text") == { :doc_text, "text" }
    ## Degeneracy
    assert text("") == { :doc_text, "" }
    ## ong argument type
    assert_raise FunctionClauseError, fn -> text(42) end
    # Consistence of corresponding docfactor
    assert factor(text("_"), 80) == { :s_text, "_", :s_nil }
    # Consistent formatting
    assert pretty(text("_"), 80) == "_"
  end

  test :space do
    # Consistency with definitions
    assert space(text("a"), text("b")) == { :doc_cons,
      text("a"), { :doc_cons, text(" "), text("b") }
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

    a1   = fn -> nest(text("a"), 1) end
    alb1 = fn -> nest(glue(text("a"), text("b")), 1) end
    # Consistence of corresponding sdoc
    ## Trivial case
    assert factor(a1.(), 80)  == { :s_text, "a", :s_nil }
    ## Correctly indenting line forcing linebreak
    assert format(2, 0, [{0, :break, alb1.()}]) ==
      { :s_text, "a", { :s_line, 1, { :s_text, "b", :s_nil }}}

    # Consistent formatting
    ## Trivial case
    assert pretty(a1.(), 80)   == "a"
    ## Correctly indenting line
    assert render(format 2, 0, [{0, :break, alb1.()}]) == "a\n b"
  end

  test :infinity do
    # w = :infinity should disable pretty printer
    s = String.duplicate "x", 50
    t = text(s)
    g = ";"
    big_document = group(glue(t, g, t) |>  glue(g, t) |>  glue(g, t) |> glue(g, t))

   assert pretty(big_document, :infinity) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end

  test :group do
    # Consistency with definitions
    ## Normal case
    assert group(glue(text("a"), text("b"))) ==
      { :doc_group, { :doc_cons, text("a"), concat(break, text("b")) }}

    ## Degeneracy
    assert group(empty) == { :doc_group, empty }

    # Consistence of corresponding sdoc
    assert factor(glue(text("a"), text("b")), 1) ==
      { :s_text, "a", { :s_line, 0, { :s_text, "b", :s_nil }}}

    assert factor(glue(text("a"), text("b")), 9) ==
      { :s_text, "a", { :s_text, " ", { :s_text, "b", :s_nil }}}

    # Consistent formatting
    assert pretty(helloabcd, 5) == "hello\na\nb\ncd"
    assert pretty(helloabcd, 80) == "hello a b cd"
  end
end
