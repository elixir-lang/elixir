Code.require_file "../../test_helper", __FILE__

defmodule EEx.TokenizerTest do
  use ExUnit.Case
  require EEx.Tokenizer, as: T

  test "simple chars lists" do
    assert_equal [ { :text, 1, "foo" } ], T.tokenize('foo', 1)
  end

  test "simple strings" do
    assert_equal [ { :text, 1, "foo" } ], T.tokenize("foo", 1)
  end

  test "strings with embedded code" do
    assert_equal [ { :text, 1, "foo " }, { :expr, 1, [], ' bar ' } ], T.tokenize('foo <% bar %>', 1)
  end

  test "strings with embedded equals code" do
    assert_equal [ { :text, 1, "foo " }, { :expr, 1, '=', ' bar ' } ], T.tokenize('foo <%= bar %>', 1)
  end

  test "strings with more than one line" do
    assert_equal [ { :text, 1, "foo\n" },{ :expr, 2, '=', ' bar ' } ], T.tokenize('foo\n<%= bar %>', 1)
  end

  test "strings with more than one line and expression with more than one line" do
    string = '''
foo <%= bar

baz %>
<% foo %>
'''

    assert_equal [
      {:text, 1, "foo "},
      {:expr, 1, '=', ' bar\n\nbaz '},
      {:text, 3, "\n"},
      {:expr, 4, [], ' foo '},
      {:text, 4, "\n"}
      ], T.tokenize(string, 1)
  end

  test "strings with embedded do end" do
    assert_equal [
      { :text, 1, "foo " },
      { :start_expr, 1, '', ' if true do ' },
      { :text, 1, "bar" },
      { :end_expr, 1, '', ' end ' }
    ], T.tokenize('foo <% if true do %>bar<% end %>', 1)
  end

  test "strings with embedded -> end" do
    assert_equal [
      { :text, 1, "foo " },
      { :start_expr, 1, '', ' if(true)-> ' },
      { :text, 1, "bar" },
      { :end_expr, 1, '', ' end ' }
    ], T.tokenize('foo <% if(true)-> %>bar<% end %>', 1)
  end

  test "strings with embedded key-value blocks" do
    assert_equal [
      { :text, 1, "foo " },
      { :start_expr, 1, '', ' if true do ' },
      { :text, 1, "bar" },
      { :middle_expr, 1, '', ' elsif: false ' },
      { :text, 1, "baz" },
      { :end_expr, 1, '', ' end ' }
    ], T.tokenize('foo <% if true do %>bar<% elsif: false %>baz<% end %>', 1)
  end

  test "raise syntax error when there is start mark and no end mark" do
    assert_raises EEx.SyntaxError, "invalid token: ' :bar'", fn ->
      T.tokenize('foo <% :bar', 1)
    end
  end
end
