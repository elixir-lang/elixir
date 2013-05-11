Code.require_file "test_helper.exs", __DIR__

defmodule WadlerTest do
  use ExUnit.Case, async: true
  import Wadler
  alias Wadler, as: W

  def helloabcd do
    group(glue(
      group(line(
        group(line(
          group(line(text("hello"), text("a"))),
        text("b"))),
      text("c"))),
    text("d")))
  end

  def is_flat?(W.UNION[]),                          do: false
  def is_flat?(W.CONCAT[left: LINE,     right: _]), do: false
  def is_flat?(W.CONCAT[left: W.GLUE[], right: _]), do: false
  def is_flat?(LINE),                               do: false
  def is_flat?(W.GLUE[]),                           do: false
  def is_flat?(W.CONCAT[left: x, right: y]),        do: is_flat?(x) && is_flat?(y)

  test :null do
    # Consistence with definitions
    assert null == NIL
    # Consistence of corresponding docfactor
    assert factor(80, null) == Nil
    # Consistent formatting
    assert pretty(80, null) == ""
  end

  test :line do
    # Consistence with definitions
    assert line == LINE
    # Consistence of corresponding docfactor
    assert factor(80, line) == W.Line[indent: 0, rest: Nil]
    # Consistent formatting
    assert pretty(80, line) == "\n"
  end

  test :glue do
    # Consistence with definitions
    ## Normal case
    assert glue == W.GLUE[string: " "]
    assert glue("glue") == W.GLUE[string: "glue"]
    assert glue(text("a"), "->", text("b")) == W.CONCAT[
      left:  text("a"),
      right: W.CONCAT[left: glue("->"), right: text("b")]
    ]
    assert glue(text("a"), text("b")) == glue(text("a"), " ", text("b"))
    ## Degeneracy
    assert glue("") == W.GLUE[string: ""]
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> glue(42) end
    assert_raise FunctionClauseError, fn -> glue(text("a"), 42, text("b")) end

    # Consistence of corresponding docfactor
    assert factor(80, glue("_")) == W.Text[string: "_", rest: Nil]

    # Consistent formatting
    assert pretty(80, glue("_")) == "_"
  end

  test :text do
    # Consistence with definitions
    ## Normal case
    assert text("text") == W.TEXT[string: "text"]
    ## Degeneracy
    assert text("") == W.TEXT[string: ""]
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> text(42) end

    # Consistence of corresponding docfactor
    assert factor(80, text("_")) == W.Text[string: "_", rest: Nil]

    # Consistent formatting
    assert pretty(80, text("_")) == "_"
  end

  test :space do
    # Consistence with definitions
    assert space(text("a"), text("b")) == W.CONCAT[
      left:  text("a"), 
      right: W.CONCAT[left: text(" "), right: text("b")]
    ]
  end

  test :nest do
    # Consistence with definitions
    ## Normal case
    assert nest(1, null) == W.NEST[indent: 1, rest: null]
    ## Degeneracy
    assert nest(0, null) == W.NEST[indent: 0, rest: null]
    ## Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", null) end

    a1   = fn -> nest(1, text("a")) end
    alb1 = fn -> nest(1, line(text("a"), text("b"))) end
    # Consistence of corresponding docfactor
    ## Trivial case
    assert factor(80, a1.())   == W.Text[string: "a", rest: Nil]
    ## Correctly indenting line
    assert factor(80, alb1.()) == W.Text[string: "a", rest:
                                  W.Line[indent:  1 , rest:
                                  W.Text[string: "b", rest: Nil]]]

    # Consistent formatting
    ## Trivial case
    assert pretty(80, a1.())   == "a"
    ## Correctly indenting line
    assert pretty(80, alb1.()) == "a\n b"
  end

  test :group do
    # Consistence with definitions 
    ## Normal case
    assert group(line(text("a"), text("b"))) == W.UNION[ left:  concat(text("a"), concat(text(" "), text("b"))),
                                                         right: concat(text("a"), concat(line,      text("b"))) ]
    ## Degeneracy
    assert group(null) == W.UNION[left: null, right: null]

    # Consistence of corresponding docfactor
    assert factor(1, group(line(text("a"), text("b")))) == W.Text[string: "a", rest:
                                                           W.Line[indent:  0 , rest:
                                                           W.Text[string: "b", rest: Nil]]]

    assert factor(9, group(line(text("a"), text("b")))) == W.Text[string: "a", rest:
                                                           W.Text[string: " ", rest:
                                                           W.Text[string: "b", rest: Nil]]]

    # Consistent formatting
    assert pretty(6,  helloabcd) == "hello\na\nb\nc d"
    assert pretty(8,  helloabcd) == "hello a\nb\nc d"
    assert pretty(80, helloabcd) == "hello a b cd"
  end
end
