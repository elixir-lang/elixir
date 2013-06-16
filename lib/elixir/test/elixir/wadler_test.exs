Code.require_file "test_helper.exs", __DIR__

defmodule WadlerTest do
  use ExUnit.Case, async: true
  import Wadler
  alias Wadler, as: W

  def helloabcd do
    glue(
      glue(
        glue(
          glue(text("hello"), text("a")),
          text("b")),
        text("c")),
      text("d"))
  end

  def factor(w, doc), do: W.format(w, 0, [{0, Flat, group(doc)}])

  test :empty do
    # Consistence with definitions
    assert empty == DocNil
    # Consistence of corresponding sdoc
    assert factor(80, empty) == SNil
    # Consistent formatting
    assert pretty(80, empty) == ""
  end

  test :break do
    # Consistence with definitions
    ## Normal case
    assert break("break") == W.DocBreak[str: "break"]
    ## Degeneration
    assert break("") == W.DocBreak[str: ""]
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> break(42) end
    # Consistence of corresponding sdoc
    assert factor(80, break("_")) == W.SText[str: "_", sdoc: SNil]
    # Consistent formatting
    assert pretty(80, break("_")) == "_"
  end

  test :glue do
    # Consistence with definitions
    ## Normal case
    assert glue(text("a"), "->", text("b")) == W.DocCons[
      left:  W.DocText[str: "a"],
      right: W.DocCons[
        left: W.DocBreak[str: "->"], 
        right: W.DocText[str: "b"]
      ]
    ]
    assert glue(text("a"), text("b")) == glue(text("a"), " ", text("b"))

    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> glue(text("a"), 42, text("b")) end
  end

  test :text do
    # Consistence with definitions
    ## Normal case
    assert text("text") == W.DocText[str: "text"]
    ## Degeneration
    assert text("") == W.DocText[str: ""]
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> text(42) end
    # Consistence of corresponding docfactor
    assert factor(80, text("_")) == W.SText[str: "_", sdoc: SNil]
    # Consistent formatting
    assert pretty(80, text("_")) == "_"
  end

  test :space do
    # Consistency with definitions
    assert space(text("a"), text("b")) == W.DocCons[
      left:  text("a"), 
      right: W.DocCons[left: text(" "), right: text("b")]
    ]
  end

  test :nest do
    # Consistence with definitions
    ## Normal case
    assert nest(1, empty) == W.DocNest[indent: 1, doc: empty]
    ## Degeneration
    assert nest(0, empty) == DocNil
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty) end

    a1   = fn -> nest(1, text("a")) end
    alb1 = fn -> nest(1, line(text("a"), text("b"))) end
    # Consistence of corresponding sdoc
    ## Trivial case
    assert factor(80, a1.())  == W.SText[str: "a", sdoc: SNil]
    ## Correctly indenting line
    assert factor(2, alb1.()) == W.SText[
      str: "a", 
      sdoc: W.SLine[
        indent: 1,
        sdoc: W.SText[str: "b", sdoc: SNil]
      ]
    ]

    # Consistent formatting
    ## Trivial case
    assert pretty(80, a1.())   == "a"
    ## Correctly indenting line
    assert pretty(80, alb1.()) == "a\n b"
  end

  test :group do
    # Consistency with definitions 
    ## Normal case
    assert group(glue(text("a"), text("b"))) == W.DocGroup[
      doc: W.DocCons[
        left: text("a"), 
        right: concat(break, text("b"))
      ]
    ]

    ## Degeneracy
    assert group(empty) == W.DocGroup[doc: empty]

    # Consistence of corresponding sdoc
    assert factor(1, glue(text("a"), text("b"))) == W.SText[
      str: "a", 
      sdoc: W.SLine[
        indent: 0,
        sdoc: W.SText[str: "b", sdoc: SNil]
      ]
    ]

    assert factor(9, glue(text("a"), text("b"))) == W.SText[
      str: "a",
      sdoc: W.SText[
        str: " ", 
        sdoc: W.SText[str: "b", sdoc: SNil]
      ]
    ]

    # Consistent formatting
    assert pretty(6,  helloabcd) == "hello\na\nb\nc d"
    assert pretty(8,  helloabcd) == "hello a\nb\nc d"
    assert pretty(80, helloabcd) == "hello a b cd"
  end
end
