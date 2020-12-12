Code.require_file("../test_helper.exs", __DIR__)

defmodule EEx.TokenizerTest do
  use ExUnit.Case, async: true
  require EEx.Tokenizer, as: T

  @opts %{indentation: 0, trim: false}

  test "simple chars lists" do
    assert T.tokenize('foo', 1, 1, @opts) == {:ok, [{:text, 1, 1, 'foo'}, {:eof, 1, 4}]}
  end

  test "simple strings" do
    assert T.tokenize("foo", 1, 1, @opts) == {:ok, [{:text, 1, 1, 'foo'}, {:eof, 1, 4}]}
  end

  test "strings with embedded code" do
    assert T.tokenize('foo <% bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo '}, {:expr, 1, 5, '', ' bar '}, {:eof, 1, 14}]}
  end

  test "strings with embedded equals code" do
    assert T.tokenize('foo <%= bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo '}, {:expr, 1, 5, '=', ' bar '}, {:eof, 1, 15}]}
  end

  test "strings with embedded slash code" do
    assert T.tokenize('foo <%/ bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo '}, {:expr, 1, 5, '/', ' bar '}, {:eof, 1, 15}]}
  end

  test "strings with embedded pipe code" do
    assert T.tokenize('foo <%| bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo '}, {:expr, 1, 5, '|', ' bar '}, {:eof, 1, 15}]}
  end

  test "strings with more than one line" do
    assert T.tokenize('foo\n<%= bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo\n'}, {:expr, 2, 1, '=', ' bar '}, {:eof, 2, 11}]}
  end

  test "strings with more than one line and expression with more than one line" do
    string = '''
    foo <%= bar

    baz %>
    <% foo %>
    '''

    exprs = [
      {:text, 1, 1, 'foo '},
      {:expr, 1, 5, '=', ' bar\n\nbaz '},
      {:text, 3, 7, '\n'},
      {:expr, 4, 1, '', ' foo '},
      {:text, 4, 10, '\n'},
      {:eof, 5, 1}
    ]

    assert T.tokenize(string, 1, 1, @opts) == {:ok, exprs}
  end

  test "quotation" do
    assert T.tokenize('foo <%% true %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo <% true %>'}, {:eof, 1, 16}]}
  end

  test "quotation with do/end" do
    assert T.tokenize('foo <%% true do %>bar<%% end %>', 1, 1, @opts) ==
             {:ok, [{:text, 1, 1, 'foo <% true do %>bar<% end %>'}, {:eof, 1, 32}]}
  end

  test "quotation with interpolation" do
    exprs = [
      {:text, 1, 1, 'a <% b '},
      {:expr, 1, 9, '=', ' c '},
      {:text, 1, 17, ' '},
      {:expr, 1, 18, '=', ' d '},
      {:text, 1, 26, ' e %> f'},
      {:eof, 1, 33}
    ]

    assert T.tokenize('a <%% b <%= c %> <%= d %> e %> f', 1, 1, @opts) == {:ok, exprs}
  end

  test "improperly formatted quotation with interpolation" do
    exprs = [
      {:text, 1, 1, '<%% a <%= b %> c %>'},
      {:eof, 1, 22}
    ]

    assert T.tokenize('<%%% a <%%= b %> c %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "eex comments" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:eof, 1, 16}
    ]

    assert T.tokenize('foo <%# true %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "eex comments with do/end" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:text, 1, 19, 'bar'},
      {:eof, 1, 32}
    ]

    assert T.tokenize('foo <%# true do %>bar<%# end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "elixir comments" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:expr, 1, 5, [], ' true # this is a boolean '},
      {:eof, 1, 35}
    ]

    assert T.tokenize('foo <% true # this is a boolean %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "elixir comments with do/end" do
    exprs = [
      {:start_expr, 1, 1, [], ' if true do # startif '},
      {:text, 1, 27, 'text'},
      {:end_expr, 1, 31, [], ' end # closeif '},
      {:eof, 1, 50}
    ]

    assert T.tokenize('<% if true do # startif %>text<% end # closeif %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "strings with embedded do end" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:start_expr, 1, 5, '', ' if true do '},
      {:text, 1, 21, 'bar'},
      {:end_expr, 1, 24, '', ' end '},
      {:eof, 1, 33}
    ]

    assert T.tokenize('foo <% if true do %>bar<% end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "strings with embedded -> end" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:start_expr, 1, 5, '', ' cond do '},
      {:middle_expr, 1, 18, '', ' false -> '},
      {:text, 1, 32, 'bar'},
      {:middle_expr, 1, 35, '', ' true -> '},
      {:text, 1, 48, 'baz'},
      {:end_expr, 1, 51, '', ' end '},
      {:eof, 1, 60}
    ]

    assert T.tokenize('foo <% cond do %><% false -> %>bar<% true -> %>baz<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "strings with multiple callbacks" do
    exprs = [
      {:start_expr, 1, 1, '=', ' a fn -> '},
      {:text, 1, 15, 'foo'},
      {:middle_expr, 1, 18, '', ' end, fn -> '},
      {:text, 1, 34, 'bar'},
      {:end_expr, 1, 37, '', ' end '},
      {:eof, 1, 46}
    ]

    assert T.tokenize('<%= a fn -> %>foo<% end, fn -> %>bar<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "strings with callback followed by do block" do
    exprs = [
      {:start_expr, 1, 1, '=', ' a fn -> '},
      {:text, 1, 15, 'foo'},
      {:middle_expr, 1, 18, '', ' end do '},
      {:text, 1, 30, 'bar'},
      {:end_expr, 1, 33, '', ' end '},
      {:eof, 1, 42}
    ]

    assert T.tokenize('<%= a fn -> %>foo<% end do %>bar<% end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "strings with embedded keywords blocks" do
    exprs = [
      {:text, 1, 1, 'foo '},
      {:start_expr, 1, 5, '', ' if true do '},
      {:text, 1, 21, 'bar'},
      {:middle_expr, 1, 24, '', ' else '},
      {:text, 1, 34, 'baz'},
      {:end_expr, 1, 37, '', ' end '},
      {:eof, 1, 46}
    ]

    assert T.tokenize('foo <% if true do %>bar<% else %>baz<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "trim mode" do
    template = '\t<%= if true do %> \n TRUE \n  <% else %>\n FALSE \n  <% end %>  \n\n  '

    exprs = [
      {:start_expr, 1, 2, '=', ' if true do '},
      {:text, 1, 20, '\n TRUE \n'},
      {:middle_expr, 3, 3, '', ' else '},
      {:text, 3, 13, '\n FALSE \n'},
      {:end_expr, 5, 3, '', ' end '},
      {:eof, 7, 3}
    ]

    assert T.tokenize(template, 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode with comment" do
    exprs = [
      {:text, 1, 19, '\n123'},
      {:eof, 2, 4}
    ]

    assert T.tokenize('  <%# comment %>  \n123', 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode with CRLF" do
    exprs = [
      {:text, 1, 1, '0\n'},
      {:expr, 2, 3, '=', ' 12 '},
      {:text, 2, 15, '\n34'},
      {:eof, 3, 3}
    ]

    assert T.tokenize('0\r\n  <%= 12 %>  \r\n34', 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode set to false" do
    exprs = [
      {:text, 1, 1, ' '},
      {:expr, 1, 2, '=', ' 12 '},
      {:text, 1, 11, ' \n'},
      {:eof, 2, 1}
    ]

    assert T.tokenize(' <%= 12 %> \n', 1, 1, %{@opts | trim: false}) == {:ok, exprs}
  end

  test "trim mode no false positives" do
    assert_not_trimmed = fn x ->
      assert T.tokenize(x, 1, 1, %{@opts | trim: false}) == T.tokenize(x, 1, 1, @opts)
    end

    assert_not_trimmed.('foo <%= "bar" %>  ')
    assert_not_trimmed.('\n  <%= "foo" %>bar')
    assert_not_trimmed.('  <%% hello %>  ')
    assert_not_trimmed.('  <%= 01 %><%= 23 %>\n')
  end

  test "returns error when there is start mark and no end mark" do
    assert T.tokenize('foo <% :bar', 1, 1, @opts) == {:error, 1, 12, "missing token '%>'"}
    assert T.tokenize('<%# true ', 1, 1, @opts) == {:error, 1, 10, "missing token '%>'"}
  end

  test "marks invalid expressions as regular expressions" do
    assert T.tokenize('<% 1 $ 2 %>', 1, 1, @opts) ==
             {:ok, [{:expr, 1, 1, [], ' 1 $ 2 '}, {:eof, 1, 12}]}
  end
end
