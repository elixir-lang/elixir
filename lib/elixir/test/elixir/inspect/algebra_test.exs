Code.require_file "../test_helper.exs", __DIR__

defmodule Inspect.AlgebraTest do
  use ExUnit.Case, async: true

  doctest Inspect.Algebra

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

  def sdoc(doc) do
    format(group(doc), :infinity)
  end

  defp render(doc, limit) do
    format(doc, limit) |> IO.iodata_to_binary
  end

  test "empty doc" do
    # Consistence with definitions
    assert empty == :doc_nil

    # Consistence of corresponding sdoc
    assert sdoc(empty) == []

    # Consistent formatting
    assert render(empty, 80) == ""
  end

  test "break doc" do
    # Consistence with definitions
    assert break("break") == {:doc_break, "break"}
    assert break("") == {:doc_break, ""}

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> break(42) end

    # Consistence of corresponding sdoc
    assert sdoc(break("_")) == ["_"]

    # Consistent formatting
    assert render(break("_"), 80) == "_"
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
    assert sdoc("_") == ["_"]

    # Consistent formatting
    assert render("_", 80) == "_"
  end

  test "space doc" do
    # Consistency with definitions
    assert space("a", "b") == {:doc_cons,
      "a", {:doc_cons, " ", "b"}
   }
  end

  test "nest doc" do
    # Consistence with definitions
    assert nest(empty, 1) == {:doc_nest, empty, 1}
    assert nest(empty, 0) == :doc_nil

    # Wrong argument type
    assert_raise FunctionClauseError, fn -> nest("foo", empty) end

    # Consistence of corresponding sdoc
    assert sdoc(nest("a", 1))  == ["a"]

    # Consistent formatting
    assert render(nest("a", 1), 80) == "a"
    assert render(nest(glue("a", "b"), 1), 2) == "a\n b"
  end

  test "line doc" do
    # Consistency with definitions
    assert line("a", "b") ==
      {:doc_cons, "a", {:doc_cons, :doc_line, "b"}}

    # Consistence of corresponding sdoc
    assert sdoc(line("a", "b")) == ["a", "\n", "b"]

    # Consistent formatting
    assert render(line(glue("aaa", "bbb"), glue("ccc", "ddd")), 10) ==
           "aaa bbb\nccc ddd"
  end

  test "group doc" do
    # Consistency with definitions
    assert group(glue("a", "b")) ==
      {:doc_group, {:doc_cons, "a", concat(break, "b")}}
    assert group(empty) == {:doc_group, empty}

    # Consistence of corresponding sdoc
    assert sdoc(glue("a", "b")) == ["a", " ", "b"]

    # Consistent formatting
    assert render(helloabcd, 5) == "hello\na b\ncd"
    assert render(helloabcd, 80) == "hello a b cd"
  end

  test "formatting with infinity" do
    s = String.duplicate "x", 50
    g = ";"
    doc = glue(s, g, s) |>  glue(g, s) |>  glue(g, s) |> glue(g, s) |> group

    assert render(doc, :infinity) == s <> g <> s <> g <> s <> g <> s <> g <> s
  end

  test "formatting surround_many with empty" do
    sm = &surround_many("[", &1, "]", %Inspect.Opts{}, fn(d, _) -> d end, ",")

    assert sm.([])                |> render(80) == "[]"
    assert sm.([empty])           |> render(80) == "[]"
    assert sm.([empty, empty])    |> render(80) == "[]"
    assert sm.(["a"])             |> render(80) == "[a]"
    assert sm.(["a", empty])      |> render(80) == "[a]"
    assert sm.([empty, "a"])      |> render(80) == "[a]"
    assert sm.(["a", empty, "b"]) |> render(80) == "[a, b]"
    assert sm.([empty, "a", "b"]) |> render(80) == "[a, b]"
    assert sm.(["a", "b", empty]) |> render(80) == "[a, b]"
    assert sm.(["a", "b" | "c"])  |> render(80) == "[a, b | c]"
    assert sm.(["a" | "b"])       |> render(80) == "[a | b]"
    assert sm.(["a" | empty])     |> render(80) == "[a]"
    assert sm.([empty | "b"])     |> render(80) == "[b]"
  end
end
