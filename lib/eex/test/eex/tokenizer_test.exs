Code.require_file("../test_helper.exs", __DIR__)

defmodule EEx.TokenizerTest do
  use ExUnit.Case, async: true
  require EEx.Tokenizer, as: T

  @opts %{indentation: 0, trim: false}

  test "simple chars lists" do
    assert T.tokenize('foo', 1, 1, @opts) == {:ok, [{:text, 'foo'}, {:eof, 1, 4}]}
  end

  test "simple strings" do
    assert T.tokenize("foo", 1, 1, @opts) == {:ok, [{:text, 'foo'}, {:eof, 1, 4}]}
  end

  test "strings with embedded code" do
    assert T.tokenize('foo <% bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo '}, {:expr, 1, 5, '', ' bar ', false}, {:eof, 1, 14}]}
  end

  test "strings with embedded equals code" do
    assert T.tokenize('foo <%= bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo '}, {:expr, 1, 5, '=', ' bar ', false}, {:eof, 1, 15}]}
  end

  test "strings with embedded slash code" do
    assert T.tokenize('foo <%/ bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo '}, {:expr, 1, 5, '/', ' bar ', false}, {:eof, 1, 15}]}
  end

  test "strings with embedded pipe code" do
    assert T.tokenize('foo <%| bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo '}, {:expr, 1, 5, '|', ' bar ', false}, {:eof, 1, 15}]}
  end

  test "strings with more than one line" do
    assert T.tokenize('foo\n<%= bar %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo\n'}, {:expr, 2, 1, '=', ' bar ', false}, {:eof, 2, 11}]}
  end

  test "strings with more than one line and expression with more than one line" do
    string = '''
    foo <%= bar

    baz %>
    <% foo %>
    '''

    exprs = [
      {:text, 'foo '},
      {:expr, 1, 5, '=', ' bar\n\nbaz ', false},
      {:text, '\n'},
      {:expr, 4, 1, '', ' foo ', false},
      {:text, '\n'},
      {:eof, 5, 1}
    ]

    assert T.tokenize(string, 1, 1, @opts) == {:ok, exprs}
  end

  test "quotation" do
    assert T.tokenize('foo <%% true %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo <% true %>'}, {:eof, 1, 16}]}
  end

  test "quotation with do/end" do
    assert T.tokenize('foo <%% true do %>bar<%% end %>', 1, 1, @opts) ==
             {:ok, [{:text, 'foo <% true do %>bar<% end %>'}, {:eof, 1, 32}]}
  end

  test "quotation with interpolation" do
    exprs = [
      {:text, 'a <% b '},
      {:expr, 1, 9, '=', ' c ', false},
      {:text, ' '},
      {:expr, 1, 18, '=', ' d ', false},
      {:text, ' e %> f'},
      {:eof, 1, 33}
    ]

    assert T.tokenize('a <%% b <%= c %> <%= d %> e %> f', 1, 1, @opts) == {:ok, exprs}
  end

  test "improperly formatted quotation with interpolation" do
    exprs = [
      {:text, '<%% a <%= b %> c %>'},
      {:eof, 1, 22}
    ]

    assert T.tokenize('<%%% a <%%= b %> c %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "comments" do
    exprs = [
      {:text, 'foo '},
      {:eof, 1, 16}
    ]

    assert T.tokenize('foo <%# true %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "comments with do/end" do
    exprs = [
      {:text, 'foo bar'},
      {:eof, 1, 32}
    ]

    assert T.tokenize('foo <%# true do %>bar<%# end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "strings with embedded do end" do
    exprs = [
      {:text, 'foo '},
      {:start_expr, 1, 5, '', ' if true do ', false},
      {:text, 'bar'},
      {:end_expr, 1, 24, '', ' end ', false},
      {:eof, 1, 33}
    ]

    assert T.tokenize('foo <% if true do %>bar<% end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "strings with embedded -> end" do
    exprs = [
      {:text, 'foo '},
      {:start_expr, 1, 5, '', ' cond do ', false},
      {:middle_expr, 1, 18, '', ' false -> ', false},
      {:text, 'bar'},
      {:middle_expr, 1, 35, '', ' true -> ', false},
      {:text, 'baz'},
      {:end_expr, 1, 51, '', ' end ', false},
      {:eof, 1, 60}
    ]

    assert T.tokenize('foo <% cond do %><% false -> %>bar<% true -> %>baz<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "strings with multiple callbacks" do
    exprs = [
      {:start_expr, 1, 1, '=', ' a fn -> ', false},
      {:text, 'foo'},
      {:middle_expr, 1, 18, '', ' end, fn -> ', false},
      {:text, 'bar'},
      {:end_expr, 1, 37, '', ' end ', false},
      {:eof, 1, 46}
    ]

    assert T.tokenize('<%= a fn -> %>foo<% end, fn -> %>bar<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "strings with callback followed by do block" do
    exprs = [
      {:start_expr, 1, 1, '=', ' a fn -> ', false},
      {:text, 'foo'},
      {:middle_expr, 1, 18, '', ' end do ', false},
      {:text, 'bar'},
      {:end_expr, 1, 33, '', ' end ', false},
      {:eof, 1, 42}
    ]

    assert T.tokenize('<%= a fn -> %>foo<% end do %>bar<% end %>', 1, 1, @opts) == {:ok, exprs}
  end

  test "strings with embedded keywords blocks" do
    exprs = [
      {:text, 'foo '},
      {:start_expr, 1, 5, '', ' if true do ', false},
      {:text, 'bar'},
      {:middle_expr, 1, 24, '', ' else ', false},
      {:text, 'baz'},
      {:end_expr, 1, 37, '', ' end ', false},
      {:eof, 1, 46}
    ]

    assert T.tokenize('foo <% if true do %>bar<% else %>baz<% end %>', 1, 1, @opts) ==
             {:ok, exprs}
  end

  test "trim mode" do
    template = '\t<%= if true do %> \n TRUE \n  <% else %>\n FALSE \n  <% end %>  '

    exprs = [
      {:start_expr, 1, 2, '=', ' if true do ', true},
      {:text, ' TRUE \n'},
      {:middle_expr, 3, 3, '', ' else ', true},
      {:text, ' FALSE \n'},
      {:end_expr, 5, 3, '', ' end ', true},
      {:eof, 5, 12}
    ]

    assert T.tokenize(template, 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode with comment" do
    exprs = [
      {:text, '123'},
      {:eof, 2, 4}
    ]

    assert T.tokenize('  <%# comment %>  \n123', 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode with CRLF" do
    exprs = [
      {:text, '0\r\n'},
      {:expr, 2, 3, '=', ' 12 ', true},
      {:text, '34'},
      {:eof, 3, 3}
    ]

    assert T.tokenize('0\r\n  <%= 12 %>  \r\n34', 1, 1, %{@opts | trim: true}) == {:ok, exprs}
  end

  test "trim mode set to false" do
    exprs = [
      {:text, ' '},
      {:expr, 1, 2, '=', ' 12 ', false},
      {:text, ' \n'},
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

  test "raise syntax error when there is start mark and no end mark" do
    assert T.tokenize('foo <% :bar', 1, 1, @opts) == {:error, 1, 12, "missing token '%>'"}
    assert T.tokenize('<%# true ', 1, 1, @opts) == {:error, 1, 10, "missing token '%>'"}
  end
end
