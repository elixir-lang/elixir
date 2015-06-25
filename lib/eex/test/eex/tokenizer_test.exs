Code.require_file "../test_helper.exs", __DIR__

defmodule EEx.TokenizerTest do
  use ExUnit.Case, async: true
  require EEx.Tokenizer, as: T

  test "simple chars lists" do
    assert T.tokenize('foo', 1) == {:ok, [{:text, 'foo'}]}
  end

  test "simple strings" do
    assert T.tokenize("foo", 1) == {:ok, [{:text, 'foo'}]}
  end

  test "strings with embedded code" do
    assert T.tokenize('foo <% bar %>', 1) ==
           {:ok, [{:text, 'foo '}, {:expr, 1, '', ' bar '}]}
  end

  test "strings with embedded equals code" do
    assert T.tokenize('foo <%= bar %>', 1) ==
           {:ok, [{:text, 'foo '}, {:expr, 1, '=', ' bar '}]}
  end

  test "strings with more than one line" do
    assert T.tokenize('foo\n<%= bar %>', 1) ==
           {:ok, [{:text, 'foo\n'}, {:expr, 2, '=', ' bar '}]}
  end

  test "strings with more than one line and expression with more than one line" do
    string = '''
foo <%= bar

baz %>
<% foo %>
'''

    assert T.tokenize(string, 1) == {:ok, [
      {:text, 'foo '},
      {:expr, 1, '=', ' bar\n\nbaz '},
      {:text, '\n'},
      {:expr, 4, '', ' foo '},
      {:text, '\n'}
    ]}
  end

  test "quotation" do
    assert T.tokenize('foo <%% true %>', 1) == {:ok, [
      {:text, 'foo <% true %>'}
    ]}
  end

  test "quotation with do/end" do
    assert T.tokenize('foo <%% true do %>bar<%% end %>', 1) == {:ok, [
      {:text, 'foo <% true do %>bar<% end %>'}
    ]}
  end

  test "quotation with interpolation" do
    assert T.tokenize('a <%% b <%= c %> <%= d %> e %> f', 1) == {:ok, [
      {:text, 'a <% b '},
      {:expr, 1, '=', ' c '},
      {:text, ' '},
      {:expr, 1, '=', ' d '},
      {:text, ' e %> f'}
    ]}

    assert T.tokenize('<%%% a <%%= b %> c %>', 1) == {:ok, [
      {:text, '<%% a <%= b %> c %>'}
    ]}
  end

  test "comments" do
    assert T.tokenize('foo <%# true %>', 1) == {:ok, [
      {:text, 'foo '}
    ]}
  end

  test "comments with do/end" do
    assert T.tokenize('foo <%# true do %>bar<%# end %>', 1) == {:ok, [
      {:text, 'foo bar'}
    ]}
  end

  test "strings with embedded do end" do
    assert T.tokenize('foo <% if true do %>bar<% end %>', 1) == {:ok, [
      {:text, 'foo '},
      {:start_expr, 1, '', ' if true do '},
      {:text, 'bar'},
      {:end_expr, 1, '', ' end '}
    ]}
  end

  test "strings with embedded -> end" do
    assert T.tokenize('foo <% cond do %><% false -> %>bar<% true -> %>baz<% end %>', 1) == {:ok, [
      {:text, 'foo '},
      {:start_expr, 1, '', ' cond do '},
      {:middle_expr, 1, '', ' false -> '},
      {:text, 'bar'},
      {:middle_expr, 1, '', ' true -> '},
      {:text, 'baz'},
      {:end_expr, 1, '', ' end '}
    ]}
  end

  test "strings with embedded keywords blocks" do
    assert T.tokenize('foo <% if true do %>bar<% else %>baz<% end %>', 1) == {:ok, [
      {:text, 'foo '},
      {:start_expr, 1, '', ' if true do '},
      {:text, 'bar'},
      {:middle_expr, 1, '', ' else '},
      {:text, 'baz'},
      {:end_expr, 1, '', ' end '}
    ]}
  end

  test "trim mode" do
    template = '\t<%= if true do %> \n TRUE \n  <% else %>\n FALSE \n  <% end %>  '
    assert T.tokenize(template, 1, trim: true) == {:ok, [
      {:start_expr, 1, '=', ' if true do '},
      {:text, ' TRUE \n'},
      {:middle_expr, 3, '', ' else '},
      {:text, ' FALSE \n'},
      {:end_expr, 5, '', ' end '}
    ]}
  end

  test "trim mode with comment" do
    assert T.tokenize('  <%# comment %>  \n123', 1, trim: true) == {:ok, [
      {:text, '123'}
    ]}
  end

  test "trim mode with CRLF" do
    assert T.tokenize('0\r\n  <%= 12 %>  \r\n34', 1, trim: true) == {:ok, [
      {:text, '0\r\n'},
      {:expr, 2, '=', ' 12 '},
      {:text, '34'}
    ]}
  end

  test "trim mode set to false" do
    assert T.tokenize(' <%= 12 %> \n', 1, trim: false) == {:ok, [
      {:text, ' '},
      {:expr, 1, '=', ' 12 '},
      {:text, ' \n'}
    ]}
  end

  test "trim mode no false positives" do
    assert_not_trimmed = fn x -> assert T.tokenize(x, 1, trim: true) == T.tokenize(x, 1) end

    assert_not_trimmed.('foo <%= "bar" %>  ')
    assert_not_trimmed.('\n  <%= "foo" %>bar')
    assert_not_trimmed.('  <%% hello %>  ')
    assert_not_trimmed.('  <%= 01 %><%= 23 %>\n')
  end

  test "raise syntax error when there is start mark and no end mark" do
    assert T.tokenize('foo <% :bar', 1) == {:error, 1, "missing token '%>'"}
  end
end
