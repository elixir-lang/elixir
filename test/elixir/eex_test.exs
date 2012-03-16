Code.require_file "../test_helper", __FILE__

defmodule EExTest do
  use ExUnit::Case

  test "evaluates simple string" do
    assert_eval "foo bar", "foo bar"
  end

  test "evaluates with embedded" do
    assert_eval "foo bar", "foo <%= :bar %>"
  end

  test "evaluates with embedded and the binding" do
    assert_equal "foo 1", EEx.eval_string("foo <%= bar %>", [bar: 1])
  end

  test "evaluates with embedded do end" do
    assert_eval "foo bar", "foo <% if true do %>bar<% end %>"
  end

  test "evaluates with embedded do end and eval the expression" do
    assert_eval "foo ", "foo <% if false do %>bar<% end %>"
  end

  test "evaluates with embedded do end and nested print expression" do
    assert_eval "foo bar", "foo <% if true do %><%= :bar %><% end %>"
  end

  test "evaluates with embedded do end and nested expressions" do
    assert_eval "foo bar baz", "foo <% if true do %>bar <% Process.put(:eex_text, 1) %><%= :baz %><% end %>"
    assert_equal 1, Process.get(:eex_text)
  end

  test "evaluates with embedded middle expression" do
    assert_eval "foo bar", "foo <% if true do %>bar<% else: %>baz<% end %>"
  end

  test "evaluates with embedded middle expression and eval the expression" do
    assert_eval "foo baz", "foo <% if false do %>bar<% else: %>baz<% end %>"
  end

  test "evaluates with nested start expression" do
    assert_eval "foo bar", "foo <% if true do %><% if true do %>bar<% end %><% end %>"
  end

  test "evaluates with nested middle expression" do
    assert_eval "foo baz", "foo <% if true do %><% if false do %>bar<% else: %>baz<% end %><% end %>"
  end

  test "evaluates with defined variable" do
    assert_eval "foo 1", "foo <% bar = 1 %><%= bar %>"
  end

  test "evaluates with require code" do
    assert_eval "foo 1,2,3", "foo <% require Enum, as: E %><%= E.join [1,2,3], \",\" %>"
  end

  test "evaluates with end of token" do
    assert_eval "foo bar %>", "foo bar %>"
  end

  test "raises a syntax error when the token is invalid" do
    assert_raises EEx::SyntaxError, "invalid token: ' bar'", fn ->
      EEx.compile_string "foo <%= bar"
    end
  end

  test "raises a syntax error when end expression is found without a start expression" do
    assert_raises EEx::SyntaxError, "unexpected token: ' end ' at line 1",  fn ->
      EEx.compile_string "foo <% end %>"
    end
  end

  test "raises a syntax error when start expression is found without an end expression" do
    assert_raises EEx::SyntaxError, "unexpected end of string. expecting a closing <% end %>.", fn ->
      EEx.compile_string "foo <% if true do %>"
    end
  end

  test "raises a syntax error when nested end expression is found without an start expression" do
    assert_raises EEx::SyntaxError, "unexpected token: ' end ' at line 1", fn ->
      EEx.compile_string "foo <%if true do %><% end %><% end %>"
    end
  end

  test "respects line numbers" do
    expected = """
foo
2
"""

    string = """
foo
<%= __LINE__ %>
"""

    assert_eval expected, string
  end

  test "respects line numbers inside nested expressions" do
    expected = """
foo

3

5
"""

    string = """
foo
<% if true do %>
<%= __LINE__ %>
<% end %>
<%= __LINE__ %>
"""

    assert_eval expected, string
  end

  test "respects line numbers inside start expression" do
    expected = """
foo

true

5
"""

    string = """
foo
<% if __LINE__ == 2 do %>
<%= true %>
<% end %>
<%= __LINE__ %>
"""

    assert_eval expected, string
  end

  test "respects line numbers inside middle expression" do
    expected = """
foo

true

7
"""

    string = """
foo
<% if false do %>
<%= false %>
<% elsif: __LINE__ == 4 %>
<%= true %>
<% end %>
<%= __LINE__ %>
"""

    assert_eval expected, string
  end

  test "respects line number inside nested expressions with many clauses" do
    expected = """
foo

5

7
"""

    string = """
foo
<% if false do %>
<%= __LINE__ %>
<% else: %>
<%= __LINE__ %>
<% end %>
<%= __LINE__ %>
"""

    assert_eval expected, string
  end

  test "evaluates the source from a given file" do
    filename = File.expand_path("../fixtures/eex_template.eex", __FILE__)
    result = EEx.eval_file(filename)
    assert_equal "foo bar.\n", result
  end

  test "evaluates the source from a given file with bindings" do
    filename = File.expand_path("../fixtures/eex_template_with_bindings.eex", __FILE__)
    result = EEx.eval_file(filename, [bar: 1])
    assert_equal "foo 1\n", result
  end

  test "raises an Exception when there's an error with the given file" do
    assert_raises File::Exception, "could not read file non-existent.eex: no such file or directory", fn ->
      filename = "non-existent.eex"
      EEx.compile_file(filename)
    end
  end

  defp assert_eval(expected, atual) do
    result = EEx.eval_string(atual, [], EEx::Engine, __FILE__)
    assert_equal expected, result
  end
end
