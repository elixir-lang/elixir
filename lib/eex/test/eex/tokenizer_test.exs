Code.require_file("../test_helper.exs", __DIR__)

defmodule EEx.TokenizerTest do
  use ExUnit.Case, async: true

  @opts [indentation: 0, trim: false]

  test "simple chars lists" do
    assert EEx.tokenize('foo', @opts) ==
             {:ok, [{:text, 'foo', %{column: 1, line: 1}}, {:eof, %{column: 4, line: 1}}]}
  end

  test "simple strings" do
    assert EEx.tokenize("foo", @opts) ==
             {:ok, [{:text, 'foo', %{column: 1, line: 1}}, {:eof, %{column: 4, line: 1}}]}
  end

  test "strings with embedded code" do
    assert EEx.tokenize('foo <% bar %>', @opts) ==
             {:ok,
              [
                {:text, 'foo ', %{column: 1, line: 1}},
                {:expr, '', ' bar ', %{column: 5, line: 1}},
                {:eof, %{column: 14, line: 1}}
              ]}
  end

  test "strings with embedded equals code" do
    assert EEx.tokenize('foo <%= bar %>', @opts) ==
             {:ok,
              [
                {:text, 'foo ', %{column: 1, line: 1}},
                {:expr, '=', ' bar ', %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with embedded slash code" do
    assert EEx.tokenize('foo <%/ bar %>', @opts) ==
             {:ok,
              [
                {:text, 'foo ', %{column: 1, line: 1}},
                {:expr, '/', ' bar ', %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with embedded pipe code" do
    assert EEx.tokenize('foo <%| bar %>', @opts) ==
             {:ok,
              [
                {:text, 'foo ', %{column: 1, line: 1}},
                {:expr, '|', ' bar ', %{column: 5, line: 1}},
                {:eof, %{column: 15, line: 1}}
              ]}
  end

  test "strings with more than one line" do
    assert EEx.tokenize('foo\n<%= bar %>', @opts) ==
             {:ok,
              [
                {:text, 'foo\n', %{column: 1, line: 1}},
                {:expr, '=', ' bar ', %{column: 1, line: 2}},
                {:eof, %{column: 11, line: 2}}
              ]}
  end

  test "strings with more than one line and expression with more than one line" do
    string = '''
    foo <%= bar

    baz %>
    <% foo %>
    '''

    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:expr, '=', ' bar\n\nbaz ', %{column: 5, line: 1}},
      {:text, '\n', %{column: 7, line: 3}},
      {:expr, '', ' foo ', %{column: 1, line: 4}},
      {:text, '\n', %{column: 10, line: 4}},
      {:eof, %{column: 1, line: 5}}
    ]

    assert EEx.tokenize(string, @opts) == {:ok, exprs}
  end

  test "quotation" do
    assert EEx.tokenize('foo <%% true %>', @opts) ==
             {:ok,
              [
                {:text, 'foo <% true %>', %{column: 1, line: 1}},
                {:eof, %{column: 16, line: 1}}
              ]}
  end

  test "quotation with do-end" do
    assert EEx.tokenize('foo <%% true do %>bar<%% end %>', @opts) ==
             {:ok,
              [
                {:text, 'foo <% true do %>bar<% end %>', %{column: 1, line: 1}},
                {:eof, %{column: 32, line: 1}}
              ]}
  end

  test "quotation with interpolation" do
    exprs = [
      {:text, 'a <% b ', %{column: 1, line: 1}},
      {:expr, '=', ' c ', %{column: 9, line: 1}},
      {:text, ' ', %{column: 17, line: 1}},
      {:expr, '=', ' d ', %{column: 18, line: 1}},
      {:text, ' e %> f', %{column: 26, line: 1}},
      {:eof, %{column: 33, line: 1}}
    ]

    assert EEx.tokenize('a <%% b <%= c %> <%= d %> e %> f', @opts) == {:ok, exprs}
  end

  test "improperly formatted quotation with interpolation" do
    exprs = [
      {:text, '<%% a <%= b %> c %>', %{column: 1, line: 1}},
      {:eof, %{column: 22, line: 1}}
    ]

    assert EEx.tokenize('<%%% a <%%= b %> c %>', @opts) == {:ok, exprs}
  end

  test "EEx comments" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:eof, %{column: 16, line: 1}}
    ]

    assert EEx.tokenize('foo <%# true %>', @opts) == {:ok, exprs}

    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:eof, %{column: 8, line: 2}}
    ]

    assert EEx.tokenize('foo <%#\ntrue %>', @opts) == {:ok, exprs}
  end

  test "EEx comments with do-end" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:text, 'bar', %{column: 19, line: 1}},
      {:eof, %{column: 32, line: 1}}
    ]

    assert EEx.tokenize('foo <%# true do %>bar<%# end %>', @opts) == {:ok, exprs}
  end

  test "EEx comments inside do-end" do
    exprs = [
      {:start_expr, '', ' if true do ', %{column: 1, line: 1}},
      {:text, 'bar', %{column: 31, line: 1}},
      {:end_expr, [], ' end ', %{column: 34, line: 1}},
      {:eof, %{column: 43, line: 1}}
    ]

    assert EEx.tokenize('<% if true do %><%# comment %>bar<% end %>', @opts) == {:ok, exprs}

    exprs = [
      {:start_expr, [], ' case true do ', %{column: 1, line: 1}},
      {:middle_expr, '', ' true -> ', %{column: 33, line: 1}},
      {:text, 'bar', %{column: 46, line: 1}},
      {:end_expr, [], ' end ', %{column: 49, line: 1}},
      {:eof, %{column: 58, line: 1}}
    ]

    assert EEx.tokenize('<% case true do %><%# comment %><% true -> %>bar<% end %>', @opts) ==
             {:ok, exprs}
  end

  test "EEx multi-line comments" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:comment, ' true ', %{column: 5, line: 1}},
      {:text, ' bar', %{column: 20, line: 1}},
      {:eof, %{column: 24, line: 1}}
    ]

    assert EEx.tokenize('foo <%!-- true --%> bar', @opts) == {:ok, exprs}

    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:comment, ' \ntrue\n ', %{column: 5, line: 1}},
      {:text, ' bar', %{column: 6, line: 3}},
      {:eof, %{column: 10, line: 3}}
    ]

    assert EEx.tokenize('foo <%!-- \ntrue\n --%> bar', @opts) == {:ok, exprs}

    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:comment, ' <%= true %> ', %{column: 5, line: 1}},
      {:text, ' bar', %{column: 27, line: 1}},
      {:eof, %{column: 31, line: 1}}
    ]

    assert EEx.tokenize('foo <%!-- <%= true %> --%> bar', @opts) == {:ok, exprs}
  end

  test "Elixir comments" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:expr, [], ' true # this is a boolean ', %{column: 5, line: 1}},
      {:eof, %{column: 35, line: 1}}
    ]

    assert EEx.tokenize('foo <% true # this is a boolean %>', @opts) == {:ok, exprs}
  end

  test "Elixir comments with do-end" do
    exprs = [
      {:start_expr, [], ' if true do # startif ', %{column: 1, line: 1}},
      {:text, 'text', %{column: 27, line: 1}},
      {:end_expr, [], ' end # closeif ', %{column: 31, line: 1}},
      {:eof, %{column: 50, line: 1}}
    ]

    assert EEx.tokenize('<% if true do # startif %>text<% end # closeif %>', @opts) ==
             {:ok, exprs}
  end

  test "strings with embedded do end" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:start_expr, '', ' if true do ', %{column: 5, line: 1}},
      {:text, 'bar', %{column: 21, line: 1}},
      {:end_expr, '', ' end ', %{column: 24, line: 1}},
      {:eof, %{column: 33, line: 1}}
    ]

    assert EEx.tokenize('foo <% if true do %>bar<% end %>', @opts) == {:ok, exprs}
  end

  test "strings with embedded -> end" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:start_expr, '', ' cond do ', %{column: 5, line: 1}},
      {:middle_expr, '', ' false -> ', %{column: 18, line: 1}},
      {:text, 'bar', %{column: 32, line: 1}},
      {:middle_expr, '', ' true -> ', %{column: 35, line: 1}},
      {:text, 'baz', %{column: 48, line: 1}},
      {:end_expr, '', ' end ', %{column: 51, line: 1}},
      {:eof, %{column: 60, line: 1}}
    ]

    assert EEx.tokenize('foo <% cond do %><% false -> %>bar<% true -> %>baz<% end %>', @opts) ==
             {:ok, exprs}
  end

  test "strings with fn-end with newline" do
    exprs = [
      {:start_expr, '=', ' a fn ->\n', %{column: 1, line: 1}},
      {:text, 'foo', %{column: 3, line: 2}},
      {:end_expr, [], ' end ', %{column: 6, line: 2}},
      {:eof, %{column: 15, line: 2}}
    ]

    assert EEx.tokenize('<%= a fn ->\n%>foo<% end %>', @opts) ==
             {:ok, exprs}
  end

  test "strings with multiple fn-end" do
    exprs = [
      {:start_expr, '=', ' a fn -> ', %{column: 1, line: 1}},
      {:text, 'foo', %{column: 15, line: 1}},
      {:middle_expr, '', ' end, fn -> ', %{column: 18, line: 1}},
      {:text, 'bar', %{column: 34, line: 1}},
      {:end_expr, '', ' end ', %{column: 37, line: 1}},
      {:eof, %{column: 46, line: 1}}
    ]

    assert EEx.tokenize('<%= a fn -> %>foo<% end, fn -> %>bar<% end %>', @opts) ==
             {:ok, exprs}
  end

  test "strings with fn-end followed by do block" do
    exprs = [
      {:start_expr, '=', ' a fn -> ', %{column: 1, line: 1}},
      {:text, 'foo', %{column: 15, line: 1}},
      {:middle_expr, '', ' end do ', %{column: 18, line: 1}},
      {:text, 'bar', %{column: 30, line: 1}},
      {:end_expr, '', ' end ', %{column: 33, line: 1}},
      {:eof, %{column: 42, line: 1}}
    ]

    assert EEx.tokenize('<%= a fn -> %>foo<% end do %>bar<% end %>', @opts) == {:ok, exprs}
  end

  test "strings with embedded keywords blocks" do
    exprs = [
      {:text, 'foo ', %{column: 1, line: 1}},
      {:start_expr, '', ' if true do ', %{column: 5, line: 1}},
      {:text, 'bar', %{column: 21, line: 1}},
      {:middle_expr, '', ' else ', %{column: 24, line: 1}},
      {:text, 'baz', %{column: 34, line: 1}},
      {:end_expr, '', ' end ', %{column: 37, line: 1}},
      {:eof, %{column: 46, line: 1}}
    ]

    assert EEx.tokenize('foo <% if true do %>bar<% else %>baz<% end %>', @opts) ==
             {:ok, exprs}
  end

  test "trim mode" do
    template = '\t<%= if true do %> \n TRUE \n  <% else %>\n FALSE \n  <% end %>  \n\n  '

    exprs = [
      {:start_expr, '=', ' if true do ', %{column: 2, line: 1}},
      {:text, '\n TRUE \n', %{column: 20, line: 1}},
      {:middle_expr, '', ' else ', %{column: 3, line: 3}},
      {:text, '\n FALSE \n', %{column: 13, line: 3}},
      {:end_expr, '', ' end ', %{column: 3, line: 5}},
      {:eof, %{column: 3, line: 7}}
    ]

    assert EEx.tokenize(template, [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode with comment" do
    exprs = [
      {:text, '\n123', %{column: 19, line: 1}},
      {:eof, %{column: 4, line: 2}}
    ]

    assert EEx.tokenize('  <%# comment %>  \n123', [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode with multi-line comment" do
    exprs = [
      {:comment, ' comment ', %{column: 3, line: 1}},
      {:text, '\n123', %{column: 23, line: 1}},
      {:eof, %{column: 4, line: 2}}
    ]

    assert EEx.tokenize('  <%!-- comment --%>  \n123', [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode with CRLF" do
    exprs = [
      {:text, '0\n', %{column: 1, line: 1}},
      {:expr, '=', ' 12 ', %{column: 3, line: 2}},
      {:text, '\n34', %{column: 15, line: 2}},
      {:eof, %{column: 3, line: 3}}
    ]

    assert EEx.tokenize('0\r\n  <%= 12 %>  \r\n34', [trim: true] ++ @opts) == {:ok, exprs}
  end

  test "trim mode set to false" do
    exprs = [
      {:text, ' ', %{column: 1, line: 1}},
      {:expr, '=', ' 12 ', %{column: 2, line: 1}},
      {:text, ' \n', %{column: 11, line: 1}},
      {:eof, %{column: 1, line: 2}}
    ]

    assert EEx.tokenize(' <%= 12 %> \n', [trim: false] ++ @opts) == {:ok, exprs}
  end

  test "trim mode no false positives" do
    assert_not_trimmed = fn x ->
      assert EEx.tokenize(x, [trim: false] ++ @opts) == EEx.tokenize(x, @opts)
    end

    assert_not_trimmed.('foo <%= "bar" %>  ')
    assert_not_trimmed.('\n  <%= "foo" %>bar')
    assert_not_trimmed.('  <%% hello %>  ')
    assert_not_trimmed.('  <%= 01 %><%= 23 %>\n')
  end

  test "returns error when there is start mark and no end mark" do
    assert EEx.tokenize('foo <% :bar', @opts) ==
             {:error, "missing token '%>'", %{column: 12, line: 1}}

    assert EEx.tokenize('<%# true ', @opts) ==
             {:error, "missing token '%>'", %{column: 10, line: 1}}

    assert EEx.tokenize('<%!-- foo ', @opts) ==
             {:error, "missing token '--%>'", %{column: 11, line: 1}}
  end

  test "marks invalid expressions as regular expressions" do
    assert EEx.tokenize('<% 1 $ 2 %>', @opts) ==
             {:ok,
              [
                {:expr, [], ' 1 $ 2 ', %{column: 1, line: 1}},
                {:eof, %{column: 12, line: 1}}
              ]}
  end
end
