Code.require_file "test_helper.exs", __DIR__

defmodule WadlerTest do
  use ExUnit.Case, async: true
  import Wadler
  alias Wadler, as: W

  def helloabcd do
    concat(
      glue(
        glue(
          glue(text("hello"), text("a")),
          text("b")),
        text("c")),
      text("d"))
  end

  def factor(w, doc), do: format(w, 0, [{0, :flat, group(doc)}])

  test :empty do
    # Consistence with definitions
    assert empty == :doc_nil
    # Consistence of corresponding sdoc
    assert factor(80, empty) == :s_nil
    # Consistent formatting
    assert pretty(80, empty) == ""
  end

  test :break do
    # Consistence with definitions
    ## Normal case
    assert break("break") == W.doc_break(str: "break")
    ## Degeneracy
    assert break("") == W.doc_break(str: "")
    ## ong argument type
    assert_raise FunctionClauseError, fn -> break(42) end
    # Consistence of corresponding sdoc
    assert factor(80, break("_")) == W.s_text(str: "_", sdoc: :s_nil)
    # Consistent formatting
    assert pretty(80, break("_")) == "_"
  end

  test :glue do
    # Consistence with definitions
    ## Normal case
    assert glue(text("a"), "->", text("b")) == W.doc_cons(
      left:  W.doc_break(str: "a"),
      right: W.doc_cons(
        left: break(str: "->"), 
        right: W.doc_text(str: "b")
      )
    )
    assert glue(text("a"), text("b")) == glue(text("a"), " ", text("b"))

    ## ong argument type
    assert_raise FunctionClauseError, fn -> glue(text("a"), 42, text("b")) end
  end

  test :text do
    # Consistence with definitions
    ## Normal case
    assert text("text") == W.doc_text(str: "text")
    ## Degeneracy
    assert text("") == W.doc_text(str: "")
    ## ong argument type
    assert_raise FunctionClauseError, fn -> text(42) end
    # Consistence of corresponding docfactor
    assert factor(80, text("_")) == W.s_text(str: "_", sdoc: :s_nil)
    # Consistent formatting
    assert pretty(80, text("_")) == "_"
  end

  test :space do
    # Consistency with definitions
    assert space(text("a"), text("b")) == W.doc_cons(
      left:  text("a"), 
      right: W.doc_cons(left: text(" "), right: text("b"))
    )
  end

  test :nest do
    # Consistence with definitions
    ## Normal case
    assert nest(1, empty) == W.doc_nest(indent: 1, doc: empty)
    ## Degeneracy
    assert nest(0, empty) == :doc_nil
    ## ong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty) end

    a1   = fn -> nest(1, text("a")) end
    alb1 = fn -> nest(1, glue(text("a"), text("b"))) end
    # Consistence of corresponding sdoc
    ## Trivial case
    assert factor(80, a1.())  == W.s_text(str: "a", sdoc: :s_nil)
    ## Correctly indenting line forcing linebreak
    assert format(2, 0, [{0, :break, alb1.()}]) == W.s_text(
      str: "a", 
      sdoc: W.s_line(
        indent: 1,
        sdoc: W.s_text(str: "b", sdoc: :s_nil)
      )
    )

    # Consistent formatting
    ## Trivial case
    assert pretty(80, a1.())   == "a"
    ## Correctly indenting line
    assert render(format 2, 0, [{0, :break, alb1.()}]) == "a\n b"
  end

  test :infinity do
    # w = :infinity should disable pretty printer
    s = String.duplicate "x", 50
    t = text(s)
    g = ";"
    big_document = group(glue(t, g, t) |>  glue(g, t) |>  glue(g, t) |> glue(g, t))

   assert pretty(:infinity, big_document) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end

  test :group do
    # Consistency with definitions 
    ## Normal case
    assert group(glue(text("a"), text("b"))) == W.doc_group(
      doc: W.doc_cons(
        left: text("a"), 
        right: concat(break, text("b"))
      )
    )

    ## Degeneracy
    assert group(empty) == W.doc_group(doc: empty)

    # Consistence of corresponding sdoc
    assert factor(1, glue(text("a"), text("b"))) == W.s_text(
      str: "a", 
      sdoc: W.s_line(
        indent: 0,
        sdoc: W.s_text(str: "b", sdoc: :s_nil)
      )
    )

    assert factor(9, glue(text("a"), text("b"))) == W.s_text(
      str: "a",
      sdoc: W.s_text(
        str: " ", 
        sdoc: W.s_text(str: "b", sdoc: :s_nil)
      )
    )

    # Consistent formatting
    assert pretty(5,  helloabcd) == "hello\na\nb\ncd"
    assert pretty(80, helloabcd) == "hello a b cd"
  end
end
