Code.require_file "test_helper.exs", __DIR__

require EEx

defmodule EExText.Compiled do
  def before_compile do
    fill_in_stacktrace
    { __ENV__.line, hd(tl(System.stacktrace)) }
  end
  { :erlang, 1, 2 }.tuple_to_list
  EEx.function_from_string :def, :string_sample, "<%= a + b %>", [:a, :b]

  filename = Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")
  EEx.function_from_file :defp, :private_file_sample, filename, [:bar]
  def file_sample(arg), do: private_file_sample(arg)

  def after_compile do
    fill_in_stacktrace
    { __ENV__.line, hd(tl(System.stacktrace)) }
  end

  @file "unknown"
  def unknown do
    fill_in_stacktrace
    { __ENV__.line, hd(tl(System.stacktrace)) }
  end

  defp fill_in_stacktrace do
    try do
      :erlang.error "failed"
    catch
      :error, _ -> System.stacktrace
    end
  end
end

defmodule Clause do
  defmacro defclause(expr, block) do
    quote do
      def unquote(expr), unquote(block)
    end
  end
end

defmodule EExTest do
  use ExUnit.Case, async: true

  doctest EEx
  doctest EEx.AssignsEngine

  test "evaluates simple string" do
    assert_eval "foo bar", "foo bar"
  end

  test "evaluates with embedded" do
    assert_eval "foo bar", "foo <%= :bar %>"
  end

  test "evaluates with embedded and the binding" do
    assert EEx.eval_string("foo <%= bar %>", [bar: 1]) == "foo 1"
  end

  test "evaluates with embedded do end" do
    assert_eval "foo bar", "foo <%= if true do %>bar<% end %>"
  end

  test "evaluates with embedded do end and eval the expression" do
    assert_eval "foo ", "foo <%= if false do %>bar<% end %>"
  end

  test "evaluates with embedded do end and nested print expression" do
    assert_eval "foo bar", "foo <%= if true do %><%= :bar %><% end %>"
  end

  test "evaluates with embedded do end and nested expressions" do
    assert_eval "foo bar baz", "foo <%= if true do %>bar <% Process.put(:eex_text, 1) %><%= :baz %><% end %>"
    assert Process.get(:eex_text) == 1
  end

  test "evaluates with embedded middle expression" do
    assert_eval "foo bar", "foo <%= if true do %>bar<% else %>baz<% end %>"
  end

  test "evaluates with embedded middle expression and eval the expression" do
    assert_eval "foo baz", "foo <%= if false do %>bar<% else %>baz<% end %>"
  end

  test "evaluates with nested start expression" do
    assert_eval "foo bar", "foo <%= if true do %><%= if true do %>bar<% end %><% end %>"
  end

  test "evaluates with nested middle expression" do
    assert_eval "foo baz", "foo <%= if true do %><%= if false do %>bar<% else %>baz<% end %><% end %>"
  end

  test "evaluates with defined variable" do
    assert_eval "foo 1", "foo <% bar = 1 %><%= bar %>"
  end

  test "evaluates with require code" do
    assert_eval "foo 1,2,3", "foo <% require Enum, as: E %><%= E.join [1, 2, 3], \",\" %>"
  end

  test "evaluates with end of token" do
    assert_eval "foo bar %>", "foo bar %>"
  end

  test "raises a syntax error when the token is invalid" do
    assert_raise EEx.SyntaxError, "missing token: %>", fn ->
      EEx.compile_string "foo <%= bar"
    end
  end

  test "raises a syntax error when end expression is found without a start expression" do
    assert_raise EEx.SyntaxError, "unexpected token: ' end ' at line 1",  fn ->
      EEx.compile_string "foo <% end %>"
    end
  end

  test "raises a syntax error when start expression is found without an end expression" do
    assert_raise EEx.SyntaxError, "unexpected end of string. expecting a closing <% end %>.", fn ->
      EEx.compile_string "foo <% if true do %>"
    end
  end

  test "raises a syntax error when nested end expression is found without an start expression" do
    assert_raise EEx.SyntaxError, "unexpected token: ' end ' at line 1", fn ->
      EEx.compile_string "foo <% if true do %><% end %><% end %>"
    end
  end

  test "respects line numbers" do
    expected = """
foo
2
"""

    string = """
foo
<%= __ENV__.line %>
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
<%= if true do %>
<%= __ENV__.line %>
<% end %>
<%= __ENV__.line %>
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
<%= if __ENV__.line == 2 do %>
<%= true %>
<% end %>
<%= __ENV__.line %>
"""

    assert_eval expected, string
  end

  test "respects line numbers inside middle expression with ->" do
    expected = """
foo

true

7
"""

    string = """
foo
<%= cond do %>
<% false -> %> false
<% __ENV__.line == 4 -> %>
<%= true %>
<% end %>
<%= __ENV__.line %>
"""

    assert_eval expected, string
  end

  test "respects line number inside middle expressions with keywords" do
    expected = """
foo

5

7
"""

    string = """
foo
<%= if false do %>
<%= __ENV__.line %>
<% else %>
<%= __ENV__.line %>
<% end %>
<%= __ENV__.line %>
"""

    assert_eval expected, string
  end

  test "properly handle functions" do
    expected = """

Number 1

Number 2

Number 3

"""

    string = """
<%= Enum.map [1, 2, 3], fn x -> %>
Number <%= x %>
<% end %>
"""

    assert_eval expected, string
  end

  test "do not consider already finished functions" do
    expected = """
foo

true

"""

    string = """
foo
<%= cond do %>
<% false -> %> false
<% fn -> 1 end -> %>
<%= true %>
<% end %>
"""

    assert_eval expected, string
  end

  test "evaluates nested do expressions" do
    string = """
    <% y = ["a", "b", "c"] %>
    <%= cond do %>
     <% "a" in y -> %>
      Good
     <% true -> %>
      <% if true do %>true<% else %>false<% end %>
      Bad
    <% end %>
    """

    assert_eval "\n\n  Good\n \n", string
  end

  test "unicode" do
    template = """
      • <%= "•" %> •
      <%= "Jößé Vâlìm" %> Jößé Vâlìm
    """
    result = EEx.eval_string(template)
    assert result == "  • • •\n  Jößé Vâlìm Jößé Vâlìm\n"
  end

  test "evaluates the source from a given file" do
    filename = Path.join(__DIR__, "fixtures/eex_template.eex")
    result = EEx.eval_file(filename)
    assert result == "foo bar.\n"
  end

  test "evaluates the source from a given file with bindings" do
    filename = Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")
    result = EEx.eval_file(filename, [bar: 1])
    assert result == "foo 1\n"
  end

  test "raises an Exception when there's an error with the given file" do
    assert_raise File.Error, "could not read file non-existent.eex: no such file or directory", fn ->
      filename = "non-existent.eex"
      EEx.compile_file(filename)
    end
  end

  test "defined from string" do
    assert EExText.Compiled.string_sample(1, 2) == "3"
  end

  test "defined from file" do
    assert EExText.Compiled.file_sample(1) == "foo 1\n"
  end

  test "defined from file do not affect backtrace" do
    assert EExText.Compiled.before_compile ==
      { 8,
        { EExText.Compiled,
          :before_compile,
          0,
          [file: to_char_list(Path.relative_to_cwd(__ENV__.file)), line: 7]
        }
      }

    assert EExText.Compiled.after_compile ==
      { 19,
        { EExText.Compiled,
          :after_compile,
          0,
          [file: to_char_list(Path.relative_to_cwd(__ENV__.file)), line: 18]
        }
      }

    assert EExText.Compiled.unknown ==
      { 25,
        { EExText.Compiled,
          :unknown,
          0,
          [file: 'unknown', line: 24]
        }
      }
  end

  defmodule TestEngine do
    @behaviour EEx.Engine

    def handle_body(body) do
      { :wrapped, body }
    end

    def handle_text(buffer, text) do
      EEx.Engine.handle_text(buffer, text)
    end

    def handle_expr(buffer, mark, expr) do
      EEx.Engine.handle_expr(buffer, mark, expr)
    end
  end

  test "calls handle_body" do
    assert { :wrapped, "foo" } = EEx.eval_string("foo", [], engine: TestEngine)
  end

  defp assert_eval(expected, actual) do
    result = EEx.eval_string(actual, [], file: __ENV__.file, engine: EEx.Engine)
    assert result == expected
  end
end
