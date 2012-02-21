Code.require_file "../../test_helper", __FILE__

defmodule EEx::TokenizerTest do
  use ExUnit::Case
  require EEx::Tokenizer, as: T

  test "simple chars lists" do
    assert_equal [ { :text, "foo" } ], T.tokenize('foo')
  end

  test "simple strings" do
    assert_equal [ { :text, "foo" } ], T.tokenize("foo")
  end

  test "strings with embedded code" do
    assert_equal [ { :text, "foo " }, { :expr, [], ' bar ' }], T.tokenize('foo <% bar %>')
  end

  test "strings with embedded equals code" do
    assert_equal [ { :text, "foo " }, { :expr, '=', ' bar ' }], T.tokenize('foo <%= bar %>')
  end

  test "strings with embedded do end" do
    assert_equal [
      { :text, "foo " },
      { :start_expr, '', ' if true do ' },
      { :text, "bar" },
      { :end_expr, '', ' end ' }
    ], T.tokenize('foo <% if true do %>bar<% end %>')
  end

  test "strings with embedded -> end" do
    assert_equal [
      { :text, "foo " },
      { :start_expr, '', ' if(true)-> ' },
      { :text, "bar" },
      { :end_expr, '', ' end ' }
    ], T.tokenize('foo <% if(true)-> %>bar<% end %>')
  end

  test "strings with embedded key-value blocks" do
    assert_equal [
      { :text, "foo " },
      { :start_expr, '', ' if true do ' },
      { :text, "bar" },
      { :middle_expr, '', ' elsif: false ' },
      { :text, "baz" },
      { :end_expr, '', ' end ' }
    ], T.tokenize('foo <% if true do %>bar<% elsif: false %>baz<% end %>')
  end
end
