Code.require_file("../test_helper.exs", __DIR__)

defmodule EEx.TokenizerTest do
  use ExUnit.Case, async: true

  @opts [indentation: 0, trim: false]

  test "simple charlists" do
    assert EEx.tokenize(~c"foo", @opts) ==
             {:ok, [{:text, ~c"foo", %{column: 1, line: 1}}, {:eof, %{column: 4, line: 1}}]}
  end

  test "simple strings" do
    assert EEx.tokenize("foo", @opts) ==
             {:ok, [{:text, ~c"foo", %{column: 1, line: 1}}, {:eof, %{column: 4, line: 1}}]}
  end

  test "strings with embedded code" do
    assert EEx.tokenize(~c"foo <% bar %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo ", %{column: 1, line: 1}},
                {:expr, ~c"", ~c" bar ", %{column: 5, line: 1}},
                {:eof, %{column: 14, line: 1}}
              ]}
  end

  test "strings with embedded equals code" do
    assert EEx.tokenize(~c"foo <%= bar %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo ", %{column: 1, line: 1}},
                {:expr, ~c"=", ~c" bar ", %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with embedded slash code" do
    assert EEx.tokenize(~c"foo <%/ bar %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo ", %{column: 1, line: 1}},
                {:expr, ~c"/", ~c" bar ", %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with embedded pipe code" do
    assert EEx.tokenize(~c"foo <%| bar %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo ", %{column: 1, line: 1}},
                {:expr, ~c"|", ~c" bar ", %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with more than one line" do
    assert EEx.tokenize(~c"foo\n<%= bar %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo\n", %{column: 1, line: 1}},
                {:expr, ~c"=", ~c" bar ", %{column: 1, line: 2}},
                {:eof, %{column: 11, line: 2}}
              ]}
  end

  test "strings with more than one line and expression with more than one line" do
    string = ~c"""
    foo <%= bar

    baz %>
    <% foo %>
    """

    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:expr, ~c"=", ~c" bar\n\nbaz ", %{column: 5, line: 1}},
      {:text, ~c"\n", %{column: 7, line: 3}},
      {:expr, ~c"", ~c" foo ", %{column: 1, line: 4}},
      {:text, ~c"\n", %{column: 10, line: 4}},
      {:eof, %{column: 1, line: 5}}
    ]

    assert EEx.tokenize(string, @opts) == {:ok, exprs}
  end

  test "quotation" do
    assert EEx.tokenize(~c"foo <%% true %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo <% true %>", %{column: 1, line: 1}},
                {:eof, %{column: 16, line: 1}}
              ]}
  end

  test "quotation with do-end" do
    assert EEx.tokenize(~c"foo <%% true do %>bar<%% end %>", @opts) ==
             {:ok,
              [
                {:text, ~c"foo <% true do %>bar<% end %>", %{column: 1, line: 1}},
                {:eof, %{column: 32, line: 1}}
              ]}
  end

  test "quotation with interpolation" do
    exprs = [
      {:text, ~c"a <% b ", %{column: 1, line: 1}},
      {:expr, ~c"=", ~c" c ", %{column: 9, line: 1}},
      {:text, ~c" ", %{column: 17, line: 1}},
      {:expr, ~c"=", ~c" d ", %{column: 18, line: 1}},
      {:text, ~c" e %> f", %{column: 26, line: 1}},
      {:eof, %{column: 33, line: 1}}
    ]

    assert EEx.tokenize(~c"a <%% b <%= c %> <%= d %> e %> f", @opts) == {:ok, exprs}
  end

  test "improperly formatted quotation with interpolation" do
    exprs = [
      {:text, ~c"<%% a <%= b %> c %>", %{column: 1, line: 1}},
      {:eof, %{column: 22, line: 1}}
    ]

    assert EEx.tokenize(~c"<%%% a <%%= b %> c %>", @opts) == {:ok, exprs}
  end

  test "EEx comments" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      exprs = [
        {:text, ~c"foo ", %{column: 1, line: 1}},
        {:eof, %{column: 16, line: 1}}
      ]

      assert EEx.tokenize(~c"foo <%# true %>", @opts) == {:ok, exprs}

      exprs = [
        {:text, ~c"foo ", %{column: 1, line: 1}},
        {:eof, %{column: 8, line: 2}}
      ]

      assert EEx.tokenize(~c"foo <%#\ntrue %>", @opts) == {:ok, exprs}
    end)
  end

  test "EEx multi-line comments" do
    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:comment, ~c" true ", %{column: 5, line: 1}},
      {:text, ~c" bar", %{column: 20, line: 1}},
      {:eof, %{column: 24, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <%!-- true --%> bar", @opts) == {:ok, exprs}

    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:comment, ~c" \ntrue\n ", %{column: 5, line: 1}},
      {:text, ~c" bar", %{column: 6, line: 3}},
      {:eof, %{column: 10, line: 3}}
    ]

    assert EEx.tokenize(~c"foo <%!-- \ntrue\n --%> bar", @opts) == {:ok, exprs}

    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:comment, ~c" <%= true %> ", %{column: 5, line: 1}},
      {:text, ~c" bar", %{column: 27, line: 1}},
      {:eof, %{column: 31, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <%!-- <%= true %> --%> bar", @opts) == {:ok, exprs}
  end

  test "Elixir comments" do
    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:expr, [], ~c" true # this is a boolean ", %{column: 5, line: 1}},
      {:eof, %{column: 35, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <% true # this is a boolean %>", @opts) == {:ok, exprs}
  end

  test "Elixir comments with do-end" do
    exprs = [
      {:start_expr, [], ~c" if true do # startif ", %{column: 1, line: 1}},
      {:text, ~c"text", %{column: 27, line: 1}},
      {:end_expr, [], ~c" end # closeif ", %{column: 31, line: 1}},
      {:eof, %{column: 50, line: 1}}
    ]

    assert EEx.tokenize(~c"<% if true do # startif %>text<% end # closeif %>", @opts) ==
             {:ok, exprs}
  end

  test "strings with embedded do end" do
    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:start_expr, ~c"", ~c" if true do ", %{column: 5, line: 1}},
      {:text, ~c"bar", %{column: 21, line: 1}},
      {:end_expr, ~c"", ~c" end ", %{column: 24, line: 1}},
      {:eof, %{column: 33, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <% if true do %>bar<% end %>", @opts) == {:ok, exprs}
  end

  test "strings with embedded -> end" do
    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:start_expr, ~c"", ~c" cond do ", %{column: 5, line: 1}},
      {:middle_expr, ~c"", ~c" false -> ", %{column: 18, line: 1}},
      {:text, ~c"bar", %{column: 32, line: 1}},
      {:middle_expr, ~c"", ~c" true -> ", %{column: 35, line: 1}},
      {:text, ~c"baz", %{column: 48, line: 1}},
      {:end_expr, ~c"", ~c" end ", %{column: 51, line: 1}},
      {:eof, %{column: 60, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <% cond do %><% false -> %>bar<% true -> %>baz<% end %>", @opts) ==
             {:ok, exprs}
  end

  test "strings with fn-end with newline" do
    exprs = [
      {:start_expr, ~c"=", ~c" a fn ->\n", %{column: 1, line: 1}},
      {:text, ~c"foo", %{column: 3, line: 2}},
      {:end_expr, [], ~c" end ", %{column: 6, line: 2}},
      {:eof, %{column: 15, line: 2}}
    ]

    assert EEx.tokenize(~c"<%= a fn ->\n%>foo<% end %>", @opts) ==
             {:ok, exprs}
  end

  test "strings with multiple fn-end" do
    exprs = [
      {:start_expr, ~c"=", ~c" a fn -> ", %{column: 1, line: 1}},
      {:text, ~c"foo", %{column: 15, line: 1}},
      {:middle_expr, ~c"", ~c" end, fn -> ", %{column: 18, line: 1}},
      {:text, ~c"bar", %{column: 34, line: 1}},
      {:end_expr, ~c"", ~c" end ", %{column: 37, line: 1}},
      {:eof, %{column: 46, line: 1}}
    ]

    assert EEx.tokenize(~c"<%= a fn -> %>foo<% end, fn -> %>bar<% end %>", @opts) ==
             {:ok, exprs}
  end

  test "strings with fn-end followed by do block" do
    exprs = [
      {:start_expr, ~c"=", ~c" a fn -> ", %{column: 1, line: 1}},
      {:text, ~c"foo", %{column: 15, line: 1}},
      {:middle_expr, ~c"", ~c" end do ", %{column: 18, line: 1}},
      {:text, ~c"bar", %{column: 30, line: 1}},
      {:end_expr, ~c"", ~c" end ", %{column: 33, line: 1}},
      {:eof, %{column: 42, line: 1}}
    ]

    assert EEx.tokenize(~c"<%= a fn -> %>foo<% end do %>bar<% end %>", @opts) == {:ok, exprs}
  end

  test "strings with embedded keywords blocks" do
    exprs = [
      {:text, ~c"foo ", %{column: 1, line: 1}},
      {:start_expr, ~c"", ~c" if true do ", %{column: 5, line: 1}},
      {:text, ~c"bar", %{column: 21, line: 1}},
      {:middle_expr, ~c"", ~c" else ", %{column: 24, line: 1}},
      {:text, ~c"baz", %{column: 34, line: 1}},
      {:end_expr, ~c"", ~c" end ", %{column: 37, line: 1}},
      {:eof, %{column: 46, line: 1}}
    ]

    assert EEx.tokenize(~c"foo <% if true do %>bar<% else %>baz<% end %>", @opts) ==
             {:ok, exprs}
  end

  test "trim mode" do
    template = ~c"\t<%= if true do %> \n TRUE \n  <% else %>\n FALSE \n  <% end %>  \n\n  "

    exprs = [
      {:start_expr, ~c"=", ~c" if true do ", %{column: 2, line: 1}},
      {:text, ~c"\n TRUE \n", %{column: 20, line: 1}},
      {:middle_expr, ~c"", ~c" else ", %{column: 3, line: 3}},
      {:text, ~c"\n FALSE \n", %{column: 13, line: 3}},
      {:end_expr, ~c"", ~c" end ", %{column: 3, line: 5}},
      {:eof, %{column: 3, line: 7}}
    ]

    assert EEx.tokenize(template, [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode with multi-line comment" do
    exprs = [
      {:comment, ~c" comment ", %{column: 3, line: 1}},
      {:text, ~c"\n123", %{column: 23, line: 1}},
      {:eof, %{column: 4, line: 2}}
    ]

    assert EEx.tokenize(~c"  <%!-- comment --%>  \n123", [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode with CRLF" do
    exprs = [
      {:text, ~c"0\n", %{column: 1, line: 1}},
      {:expr, ~c"=", ~c" 12 ", %{column: 3, line: 2}},
      {:text, ~c"\n34", %{column: 15, line: 2}},
      {:eof, %{column: 3, line: 3}}
    ]

    assert EEx.tokenize(~c"0\r\n  <%= 12 %>  \r\n34", [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode set to false" do
    exprs = [
      {:text, ~c" ", %{column: 1, line: 1}},
      {:expr, ~c"=", ~c" 12 ", %{column: 2, line: 1}},
      {:text, ~c" \n", %{column: 11, line: 1}},
      {:eof, %{column: 1, line: 2}}
    ]

    assert EEx.tokenize(~c" <%= 12 %> \n", [trim: false] ++ @opts) == {:ok, exprs}
  end

  test "trim mode no false positives" do
    assert_not_trimmed = fn x ->
      assert EEx.tokenize(x, [trim: false] ++ @opts) == EEx.tokenize(x, @opts)
    end

    assert_not_trimmed.(~c"foo <%= \"bar\" %>  ")
    assert_not_trimmed.(~c"\n  <%= \"foo\" %>bar")
    assert_not_trimmed.(~c"  <%% hello %>  ")
    assert_not_trimmed.(~c"  <%= 01 %><%= 23 %>\n")
  end

  test "returns error when there is start mark and no end mark" do
    message = """
    expected closing '%>' for EEx expression
      |
    1 | foo <% :bar
      |     ^\
    """

    assert EEx.tokenize(~c"foo <% :bar", @opts) ==
             {:error, message, %{column: 5, line: 1}}

    message = """
    expected closing '--%>' for EEx expression
      |
    1 | <%!-- foo
      | ^\
    """

    assert EEx.tokenize(~c"<%!-- foo", @opts) == {:error, message, %{column: 1, line: 1}}
  end

  test "marks invalid expressions as regular expressions" do
    assert EEx.tokenize(~c"<% 1 $ 2 %>", @opts) ==
             {:ok,
              [
                {:expr, [], ~c" 1 $ 2 ", %{column: 1, line: 1}},
                {:eof, %{column: 12, line: 1}}
              ]}
  end
end
