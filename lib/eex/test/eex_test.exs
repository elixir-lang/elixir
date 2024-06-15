Code.require_file("test_helper.exs", __DIR__)

require EEx

defmodule EExTest.Compiled do
  def before_compile do
    {__ENV__.line, hd(tl(get_stacktrace()))}
  end

  EEx.function_from_string(:def, :string_sample, "<%= a + b %>", [:a, :b])

  filename = Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")
  EEx.function_from_file(:defp, :private_file_sample, filename, [:bar])

  filename = Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")
  EEx.function_from_file(:def, :public_file_sample, filename, [:bar])

  def file_sample(arg), do: private_file_sample(arg)

  def after_compile do
    {__ENV__.line, hd(tl(get_stacktrace()))}
  end

  @file "unknown"
  def unknown do
    {__ENV__.line, hd(tl(get_stacktrace()))}
  end

  defp get_stacktrace do
    try do
      :erlang.error("failed")
    rescue
      _ -> __STACKTRACE__
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
  doctest EEx.Engine
  doctest EEx.SmartEngine

  describe "evaluates" do
    test "simple string" do
      assert_eval("foo bar", "foo bar")
    end

    test "Unicode" do
      template = """
        • <%= "•" %> •
        <%= "Jößé Vâlìm" %> Jößé Vâlìm
      """

      assert_eval("  • • •\n  Jößé Vâlìm Jößé Vâlìm\n", template)
    end

    test "no spaces" do
      string = """
      <%=cond do%>
      <%false ->%>
        this
      <%true ->%>
        that
      <%end%>
      """

      expected = "\n  that\n\n"
      assert_eval(expected, string, [])
    end

    test "trim mode" do
      string = "<%= 123 %> \n  \n  <%= 789 %>"
      expected = "123\n789"
      assert_eval(expected, string, [], trim: true)

      string = "<%= 123 %> \n456\n  <%= 789 %>"
      expected = "123\n456\n789"
      assert_eval(expected, string, [], trim: true)

      string = "<%= 123 %> \n\n456\n\n  <%= 789 %>"
      expected = "123\n456\n789"
      assert_eval(expected, string, [], trim: true)

      string = "<%= 123 %> \n  \n456\n  \n  <%= 789 %>"
      expected = "123\n456\n789"
      assert_eval(expected, string, [], trim: true)

      string = "\n  <%= 123 %> \n  <%= 456 %>  \n  <%= 789 %>  \n"
      expected = "123\n456\n789"
      assert_eval(expected, string, [], trim: true)

      string = "\r\n  <%= 123 %> \r\n  <%= 456 %>  \r\n  <%= 789 %>  \r\n"
      expected = "123\n456\n789"
      assert_eval(expected, string, [], trim: true)
    end

    test "trim mode with middle expression" do
      string = """
      <%= cond do %>
      <% false -> %>
        this
      <% true -> %>
        that
      <% end %>
      """

      expected = "\n  that\n"
      assert_eval(expected, string, [], trim: true)
    end

    test "trim mode with multiple lines" do
      string = """
      <%= "First line" %>
      <%= "Second line" %>
      <%= "Third line" %>
      <%= "Fourth line" %>
      """

      expected = "First line\nSecond line\nThird line\nFourth line"
      assert_eval(expected, string, [], trim: true)
    end

    test "trim mode with no spaces" do
      string = """
      <%=if true do%>
        this
      <%else%>
        that
      <%end%>
      """

      expected = "\n  this\n"
      assert_eval(expected, string, [], trim: true)

      string = """
      <%=cond do%>
      <%false ->%>
        this
      <%true ->%>
        that
      <%end%>
      """

      expected = "\n  that\n"
      assert_eval(expected, string, [], trim: true)
    end

    test "embedded code" do
      assert_eval("foo bar", "foo <%= :bar %>")
    end

    test "embedded code with binding" do
      assert EEx.eval_string("foo <%= bar %>", bar: 1) == "foo 1"
    end

    test "embedded code with do end when true" do
      assert_eval("foo bar", "foo <%= if true do %>bar<% end %>")
    end

    test "embedded code with do end when false" do
      assert_eval("foo ", "foo <%= if false do %>bar<% end %>")
    end

    test "embedded code with do preceded by bracket" do
      assert_eval("foo bar", "foo <%= if {true}do %>bar<% end %>")
      assert_eval("foo bar", "foo <%= if (true)do %>bar<% end %>")
      assert_eval("foo bar", "foo <%= if [true]do %>bar<% end %>")
    end

    test "embedded code with do end and expression" do
      assert_eval("foo bar", "foo <%= if true do %><%= :bar %><% end %>")
    end

    test "embedded code with do end and multiple expressions" do
      assert_eval(
        "foo bar baz",
        "foo <%= if true do %>bar <% Process.put(:eex_text, 1) %><%= :baz %><% end %>"
      )

      assert Process.get(:eex_text) == 1
    end

    test "embedded code with middle expression" do
      assert_eval("foo bar", "foo <%= if true do %>bar<% else %>baz<% end %>")
    end

    test "embedded code with evaluated middle expression" do
      assert_eval("foo baz", "foo <%= if false do %>bar<% else %>baz<% end %>")
    end

    test "embedded code with multi-line comments in do end" do
      assert_eval("foo bar", "foo <%= case true do %><%!-- comment --%><% true -> %>bar<% end %>")

      assert_eval(
        "foo\n\nbar\n",
        "foo\n<%= case true do %>\n<%!-- comment --%>\n<% true -> %>\nbar\n<% end %>"
      )
    end

    test "embedded code with nested do end" do
      assert_eval("foo bar", "foo <%= if true do %><%= if true do %>bar<% end %><% end %>")
    end

    test "embedded code with nested do end with middle expression" do
      assert_eval(
        "foo baz",
        "foo <%= if true do %><%= if false do %>bar<% else %>baz<% end %><% end %>"
      )
    end

    test "embedded code with end followed by bracket" do
      assert_eval(
        " 101  102  103 ",
        "<%= Enum.map([1, 2, 3], fn x -> %> <%= 100 + x %> <% end) %>"
      )

      assert_eval(
        " 101  102  103 ",
        "<%= Enum.map([1, 2, 3], fn x ->\n%> <%= 100 + x %> <% end) %>"
      )

      assert_eval(
        " 101  102  103 ",
        "<%= apply Enum, :map, [[1, 2, 3], fn x -> %> <%= 100 + x %> <% end] %>"
      )

      assert_eval(
        " 101  102  103 ",
        "<%= #{__MODULE__}.tuple_map {[1, 2, 3], fn x -> %> <%= 100 + x %> <% end} %>"
      )

      assert_eval(
        " 101  102  103 ",
        "<%= apply(Enum, :map, [[1, 2, 3], fn x -> %> <%= 100 + x %> <% end]) %>"
      )

      assert_eval(
        " 101  102  103 ",
        "<%= Enum.map([1, 2, 3], (fn x -> %> <%= 100 + x %> <% end) ) %>"
      )
    end

    test "embedded code with variable definition" do
      assert_eval("foo 1", "foo <% bar = 1 %><%= bar %>")
    end

    test "embedded code with require" do
      assert_eval("foo 1,2,3", "foo <% require Enum, as: E %><%= E.join [1, 2, 3], \",\" %>")
    end

    test "with end of token" do
      assert_eval("foo bar %>", "foo bar %>")
    end
  end

  describe "raises syntax errors" do
    test "with relative file information" do
      message = """
      foobar.eex:1:5: expected closing '%>' for EEx expression
        |
      1 | foo <%= bar
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <%= bar", file: Path.join(File.cwd!(), "foobar.eex"))
      end
    end

    test "when <%!-- is not closed" do
      message = """
      my_file.eex:1:5: expected closing '--%>' for EEx expression
        |
      1 | foo <%!-- bar
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <%!-- bar", file: "my_file.eex")
      end
    end

    test "when the token is invalid" do
      message = """
      nofile:1:5: expected closing '%>' for EEx expression
        |
      1 | foo <%= bar
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <%= bar")
      end
    end

    test "when middle expression is found without a start expression" do
      message = """
      nofile:5:1: unexpected middle of expression <% else %>
        |
      2 | <%= "content" %>
      3 | <%= if true %>
      4 |   <%= "foo" %>
      5 | <% else %>
        | ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string(
          ~s(<h1>Hi!</h1>\n<%= "content" %>\n<%= if true %>\n  <%= "foo" %>\n<% else %>\n  bar<% end %>)
        )
      end
    end

    test "proper format line number of code snippet" do
      message = """
      nofile:11:1: unexpected middle of expression <% else %>
         |
       8 | <%= "content" %>
       9 | <%= if true %>
      10 |   <%= "foo" %>
      11 | <% else %>
         | ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string(
          ~s(\n\n\n\n\n\n<h1>Hi!</h1>\n<%= "content" %>\n<%= if true %>\n  <%= "foo" %>\n<% else %>\n  bar<% end %>)
        )
      end
    end

    test "when there is only middle expression" do
      message = """
      nofile:1:1: unexpected middle of expression <% else %>
        |
      1 | <% else %>
        | ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string(~s(<% else %>))
      end
    end

    test "when it is missing a `do` in case expr" do
      message = """
      nofile:3:3: unexpected middle of expression <% :something -> %>
        |
      1 | content
      2 | <%= case @var %>
      3 |   <% :something -> %>
        |   ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("content\n<%= case @var %>\n  <% :something -> %>\n    bar<% end %>")
      end
    end

    test "when it is a `do` in cond expr" do
      message = """
      nofile:3:3: unexpected middle of expression <% true -> %>
        |
      1 | content
      2 | <%= cond %>
      3 |   <% true -> %>
        |   ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("content\n<%= cond %>\n  <% true -> %>\n    bar<% end %>")
      end
    end

    test "when end expression is found without a start expression" do
      message = """
      nofile:1:5: unexpected end of expression <% end %>
        |
      1 | foo <% end %>
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <% end %>")
      end
    end

    test "when start expression is found without an end expression" do
      message = """
      nofile:2:5: expected a closing '<% end %>' for block expression in EEx
        |
      1 | foo
      2 | <%= if true do %>
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo\n<%= if true do %>\nfoo\n")
      end

      message = """
      nofile:3:3: expected a closing '<% end %>' for block expression in EEx
        |
      1 | foo
      2 | <%=
      3 |   if true do %>
        |   ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo\n<%=\n  if true do %>\nfoo\n", indentation: 0)
      end

      message = """
      nofile:3:6: expected a closing '<% end %>' for block expression in EEx
        |
      1 |    foo
      2 |    <%=
      3 |      if true do %>
        |      ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo\n<%=\n  if true do %>\nfoo\n", indentation: 3)
      end
    end

    test "when start expression with middle expression is found without an end expression" do
      message = """
      nofile:2:5: expected a closing '<% end %>' for block expression in EEx
        |
      1 | foo
      2 | <%= if true do %>
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo\n<%= if true do %>\nfoo\n<% else %>\n")
      end
    end

    test "when multiple start expressions is found without an end expression" do
      message = """
      nofile:5:5: expected a closing '<% end %>' for block expression in EEx
        |
      2 | <%= if true do %>
      3 |   <%= @something %>
      4 |\s
      5 | <%= if @var do %>
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string(
          "foo\n<%= if true do %>\n  <%= @something %>\n\n<%= if @var do %>\nfoo\n"
        )
      end
    end

    test "when nested end expression is found without a start expression" do
      message = """
      nofile:1:31: unexpected end of expression <% end %>
        |
      1 | foo <%= if true do %><% end %><% end %>
        |                               ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <%= if true do %><% end %><% end %>")
      end
    end

    test "when trying to use marker '|' without implementation" do
      msg =
        ~r/unsupported EEx syntax <%| %> \(the syntax is valid but not supported by the current EEx engine\)/

      assert_raise EEx.SyntaxError, msg, fn ->
        EEx.compile_string("<%| true %>")
      end
    end

    test "when trying to use marker '/' without implementation" do
      msg =
        ~r/unsupported EEx syntax <%\/ %> \(the syntax is valid but not supported by the current EEx engine\)/

      assert_raise EEx.SyntaxError, msg, fn ->
        EEx.compile_string("<%/ true %>")
      end
    end

    test "honor line numbers" do
      assert_raise EEx.SyntaxError,
                   "nofile:100:6: expected closing '%>' for EEx expression",
                   fn ->
                     EEx.compile_string("foo\n bar <%= baz", line: 99)
                   end
    end

    test "honor file names" do
      message = """
      my_file.eex:1:5: expected closing '%>' for EEx expression
        |
      1 | foo <%= bar
        |     ^\
      """

      assert_raise EEx.SyntaxError, message, fn ->
        EEx.compile_string("foo <%= bar", file: "my_file.eex")
      end
    end
  end

  describe "warnings" do
    test "when middle expression has a modifier" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               EEx.compile_string("foo <%= if true do %>true<%= else %>false<% end %>")
             end) =~ ~s[unexpected beginning of EEx tag \"<%=\" on \"<%= else %>\"]
    end

    test "when end expression has a modifier" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               EEx.compile_string("foo <%= if true do %>true<% else %>false<%= end %>")
             end) =~
               ~s[unexpected beginning of EEx tag \"<%=\" on \"<%= end %>\"]
    end

    test "unused \"do\" block without \"<%=\" modifier" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               EEx.compile_string("<% if true do %>I'm invisible!<% end %>")
             end) =~ "the contents of this expression won't be output"

      # These are fine though
      EEx.compile_string("<% foo = fn -> %>Hello<% end %>")
      EEx.compile_string("<% foo = if true do %>Hello<% end %>")
    end

    test "from tokenizer" do
      warning =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          EEx.compile_string(~s'<%= :"foo" %>', file: "tokenizer.ex")
        end)

      assert warning =~ "found quoted atom \"foo\" but the quotes are not required"
      assert warning =~ "tokenizer.ex:1:5"
    end
  end

  describe "environment" do
    test "respects line numbers" do
      expected = """
      foo
      2
      """

      string = """
      foo
      <%= __ENV__.line %>
      """

      assert_eval(expected, string)
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

      assert_eval(expected, string)
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

      assert_eval(expected, string)
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

      assert_eval(expected, string)
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

      assert_eval(expected, string)
    end

    test "respects files" do
      assert_eval("sample.ex", "<%= __ENV__.file %>", [], file: "sample.ex")
    end
  end

  describe "clauses" do
    test "inside functions" do
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

      assert_eval(expected, string)
    end

    test "inside multiple functions" do
      expected = """

      A 1

      B 2

      A 3

      """

      string = """
      <%= #{__MODULE__}.switching_map [1, 2, 3], fn x -> %>
      A <%= x %>
      <% end, fn x -> %>
      B <%= x %>
      <% end %>
      """

      assert_eval(expected, string)
    end

    test "inside callback and do block" do
      expected = """


      A 1

      B 2

      A 3

      """

      string = """
      <% require #{__MODULE__} %>
      <%= #{__MODULE__}.switching_macro [1, 2, 3], fn x -> %>
      A <%= x %>
      <% end do %>
      B <%= x %>
      <% end %>
      """

      assert_eval(expected, string)
    end

    test "inside cond" do
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

      assert_eval(expected, string)
    end

    test "inside cond with do end" do
      string = """
      <% y = ["a", "b", "c"] %>
      <%= cond do %>
       <% "a" in y -> %>
        Good
       <% true -> %>
        <%= if true do %>true<% else %>false<% end %>
        Bad
      <% end %>
      """

      assert_eval("\n\n  Good\n \n", string)
    end

    test "line and column meta" do
      indentation = 12

      ast =
        EEx.compile_string(
          """
          <%= f() %> <% f() %>
            <%= f fn -> %>
              <%= f() %>
            <% end %>
          """,
          indentation: indentation
        )

      {_, calls} =
        Macro.prewalk(ast, [], fn
          {:f, meta, _args} = expr, acc -> {expr, [meta | acc]}
          other, acc -> {other, acc}
        end)

      assert Enum.reverse(calls) == [
               [line: 1, column: indentation + 5],
               [line: 1, column: indentation + 15],
               [line: 2, column: indentation + 7],
               [line: 3, column: indentation + 9]
             ]
    end
  end

  describe "buffers" do
    test "inside comprehensions" do
      string = """
      <%= for _name <- packages || [] do %>
      <% end %>
      <%= all || :done %>
      """

      assert_eval("\ndone\n", string, packages: nil, all: nil)
    end
  end

  describe "from file" do
    test "evaluates the source" do
      filename = Path.join(__DIR__, "fixtures/eex_template.eex")
      result = EEx.eval_file(filename)
      assert_normalized_newline_equal("foo bar.\n", result)
    end

    test "evaluates the source with bindings" do
      filename = Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")
      result = EEx.eval_file(filename, bar: 1)
      assert_normalized_newline_equal("foo 1\n", result)
    end

    test "raises an Exception when file is missing" do
      msg = "could not read file \"non-existent.eex\": no such file or directory"

      assert_raise File.Error, msg, fn ->
        filename = "non-existent.eex"
        EEx.compile_file(filename)
      end
    end

    test "sets external resource attribute" do
      assert EExTest.Compiled.__info__(:attributes)[:external_resource] ==
               [Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex")]
    end

    test "supports t:Path.t() paths" do
      filename = to_charlist(Path.join(__DIR__, "fixtures/eex_template_with_bindings.eex"))
      result = EEx.eval_file(filename, bar: 1)
      assert_normalized_newline_equal("foo 1\n", result)
    end

    test "supports overriding file and line through options" do
      filename = Path.join(__DIR__, "fixtures/eex_template_with_syntax_error.eex")

      assert_raise EEx.SyntaxError,
                   "my_file.eex:10:5: expected closing '%>' for EEx expression",
                   fn ->
                     EEx.eval_file(filename, _bindings = [], file: "my_file.eex", line: 10)
                   end
    end
  end

  describe "precompiled" do
    test "from string" do
      assert EExTest.Compiled.string_sample(1, 2) == "3"
    end

    test "from file" do
      assert_normalized_newline_equal("foo 1\n", EExTest.Compiled.file_sample(1))
      assert_normalized_newline_equal("foo 1\n", EExTest.Compiled.public_file_sample(1))
    end

    test "from file does not affect backtrace" do
      file = to_charlist(Path.relative_to_cwd(__ENV__.file))

      assert EExTest.Compiled.before_compile() ==
               {7, {EExTest.Compiled, :before_compile, 0, [file: file, line: 7]}}

      assert EExTest.Compiled.after_compile() ==
               {21, {EExTest.Compiled, :after_compile, 0, [file: file, line: 21]}}

      assert EExTest.Compiled.unknown() ==
               {26, {EExTest.Compiled, :unknown, 0, [file: ~c"unknown", line: 26]}}
    end
  end

  defmodule TestEngine do
    @behaviour EEx.Engine

    def init(_opts) do
      "INIT"
    end

    def handle_body(body) do
      "BODY(#{body})"
    end

    def handle_begin(_) do
      "BEGIN"
    end

    def handle_end(buffer) do
      buffer <> ":END"
    end

    def handle_text(buffer, meta, text) do
      buffer <> ":TEXT-#{meta[:line]}-#{meta[:column]}(#{String.trim(text)})"
    end

    def handle_expr(buffer, "/", expr) do
      buffer <> ":DIV(#{Macro.to_string(expr)})"
    end

    def handle_expr(buffer, "=", expr) do
      buffer <> ":EQUAL(#{Macro.to_string(expr)})"
    end

    def handle_expr(buffer, mark, expr) do
      EEx.Engine.handle_expr(buffer, mark, expr)
    end
  end

  describe "custom engines" do
    test "text" do
      assert_eval("BODY(INIT:TEXT-1-1(foo))", "foo", [], engine: TestEngine)
    end

    test "custom marker" do
      assert_eval("BODY(INIT:TEXT-1-1(foo):DIV(:bar))", "foo <%/ :bar %>", [], engine: TestEngine)
    end

    test "begin/end" do
      assert_eval(
        ~s[BODY(INIT:TEXT-1-1(foo):EQUAL(if do\n  "BEGIN:TEXT-1-17(this):END"\nelse\n  "BEGIN:TEXT-1-31(that):END"\nend))],
        "foo <%= if do %>this<% else %>that<% end %>",
        [],
        engine: TestEngine
      )
    end

    test "not implemented custom marker" do
      msg =
        ~r/unsupported EEx syntax <%| %> \(the syntax is valid but not supported by the current EEx engine\)/

      assert_raise EEx.SyntaxError, msg, fn ->
        assert_eval({:wrapped, "foo baz"}, "foo <%| :bar %>", [], engine: TestEngine)
      end
    end
  end

  describe "parser options" do
    test "customizes parsed code" do
      atoms_encoder = fn "not_jose", _ -> {:ok, :jose} end

      assert_eval("valid", "<%= not_jose %>", [jose: "valid"],
        parser_options: [static_atoms_encoder: atoms_encoder]
      )
    end
  end

  defp assert_eval(expected, actual, binding \\ [], opts \\ []) do
    opts = Keyword.merge([file: __ENV__.file, engine: opts[:engine] || EEx.Engine], opts)
    result = EEx.eval_string(actual, binding, opts)
    assert result == expected
  end

  defp assert_normalized_newline_equal(expected, actual) do
    assert String.replace(expected, "\r\n", "\n") == String.replace(actual, "\r\n", "\n")
  end

  def tuple_map({list, callback}) do
    Enum.map(list, callback)
  end

  def switching_map(list, a, b) do
    list
    |> Enum.with_index()
    |> Enum.map(fn
      {element, index} when rem(index, 2) == 0 -> a.(element)
      {element, index} when rem(index, 2) == 1 -> b.(element)
    end)
  end

  defmacro switching_macro(list, a, do: block) do
    quote do
      b = fn var!(x) ->
        unquote(block)
      end

      unquote(__MODULE__).switching_map(unquote(list), unquote(a), b)
    end
  end
end
