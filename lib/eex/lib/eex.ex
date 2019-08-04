defmodule EEx.SyntaxError do
  defexception [:message, :file, :line]

  @impl true
  def message(exception) do
    "#{exception.file}:#{exception.line}: #{exception.message}"
  end
end

defmodule EEx do
  @moduledoc ~S"""
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way.

      iex> EEx.eval_string("foo <%= bar %>", bar: "baz")
      "foo baz"

  ## API

  This module provides 3 main APIs for you to use:

    1. Evaluate a string (`eval_string`) or a file (`eval_file`)
       directly. This is the simplest API to use but also the
       slowest, since the code is evaluated and not compiled before.

    2. Define a function from a string (`function_from_string`)
       or a file (`function_from_file`). This allows you to embed
       the template as a function inside a module which will then
       be compiled. This is the preferred API if you have access
       to the template at compilation time.

    3. Compile a string (`compile_string`) or a file (`compile_file`)
       into Elixir syntax tree. This is the API used by both functions
       above and is available to you if you want to provide your own
       ways of handling the compiled template.

  ## Options

  All functions in this module accept EEx-related options.
  They are:

    * `:line` - the line to be used as the template start. Defaults to 1.
    * `:file` - the file to be used in the template. Defaults to the given
      file the template is read from or to "nofile" when compiling from a string.
    * `:engine` - the EEx engine to be used for compilation.
    * `:trim` - trims whitespace left/right of quotation tags. If a quotation
      tag appears on its own in a given line, line endings are also removed.

  ## Engine

  EEx has the concept of engines which allows you to modify or
  transform the code extracted from the given string or file.

  By default, `EEx` uses the `EEx.SmartEngine` that provides some
  conveniences on top of the simple `EEx.Engine`.

  ### Tags

  `EEx.SmartEngine` supports the following tags:

      <% Elixir expression - inline with output %>
      <%= Elixir expression - replace with result %>
      <%% EEx quotation - returns the contents inside %>
      <%# Comments - they are discarded from source %>

  All expressions that output something to the template
  **must** use the equals sign (`=`). Since everything in
  Elixir is an expression, there are no exceptions for this rule.
  For example, while some template languages would special-case
  `if/2` clauses, they are treated the same in EEx and
  also require `=` in order to have their result printed:

      <%= if true do %>
        It is obviously true
      <% else %>
        This will never appear
      <% end %>

  To escape eex in eex use `<%% content %>`. For example:

      <%%= x + 3 %>

  Will be rendered as `<%= x + 3 %>`

  Notice that different engines may have different rules
  for each tag. Other tags may be added in future versions.

  ### Macros

  `EEx.SmartEngine` also adds some macros to your template.
  An example is the `@` macro which allows easy data access
  in a template:

      iex> EEx.eval_string("<%= @foo %>", assigns: [foo: 1])
      "1"

  In other words, `<%= @foo %>` translates to:

      <%= {:ok, v} = Access.fetch(assigns, :foo); v %>

  The `assigns` extension is useful when the number of variables
  required by the template is not specified at compilation time.
  """

  @doc """
  Generates a function definition from the string.

  The kind (`:def` or `:defp`) must be given, the
  function name, its arguments and the compilation options.

  ## Examples

      iex> defmodule Sample do
      ...>   require EEx
      ...>   EEx.function_from_string(:def, :sample, "<%= a + b %>", [:a, :b])
      ...> end
      iex> Sample.sample(1, 2)
      "3"

  """
  defmacro function_from_string(kind, name, source, args \\ [], options \\ []) do
    quote bind_quoted: binding() do
      info = Keyword.merge([file: __ENV__.file, line: __ENV__.line], options)
      args = Enum.map(args, fn arg -> {arg, [line: info[:line]], nil} end)
      compiled = EEx.compile_string(source, info)

      case kind do
        :def -> def unquote(name)(unquote_splicing(args)), do: unquote(compiled)
        :defp -> defp unquote(name)(unquote_splicing(args)), do: unquote(compiled)
      end
    end
  end

  @doc """
  Generates a function definition from the file contents.

  The kind (`:def` or `:defp`) must be given, the
  function name, its arguments and the compilation options.

  This function is useful in case you have templates but
  you want to precompile inside a module for speed.

  ## Examples

      # sample.eex
      <%= a + b %>

      # sample.ex
      defmodule Sample do
        require EEx
        EEx.function_from_file(:def, :sample, "sample.eex", [:a, :b])
      end

      # iex
      Sample.sample(1, 2)
      #=> "3"

  """
  defmacro function_from_file(kind, name, file, args \\ [], options \\ []) do
    quote bind_quoted: binding() do
      info = Keyword.merge(options, file: file, line: 1)
      args = Enum.map(args, fn arg -> {arg, [line: 1], nil} end)
      compiled = EEx.compile_file(file, info)

      @external_resource file
      @file file
      case kind do
        :def -> def unquote(name)(unquote_splicing(args)), do: unquote(compiled)
        :defp -> defp unquote(name)(unquote_splicing(args)), do: unquote(compiled)
      end
    end
  end

  @doc """
  Gets a string `source` and generate a quoted expression
  that can be evaluated by Elixir or compiled to a function.
  """
  @spec compile_string(String.t(), keyword) :: Macro.t()
  def compile_string(source, options \\ []) when is_binary(source) and is_list(options) do
    EEx.Compiler.compile(source, options)
  end

  @doc """
  Gets a `filename` and generate a quoted expression
  that can be evaluated by Elixir or compiled to a function.
  """
  @spec compile_file(String.t(), keyword) :: Macro.t()
  def compile_file(filename, options \\ []) when is_binary(filename) and is_list(options) do
    options = Keyword.merge(options, file: filename, line: 1)
    compile_string(File.read!(filename), options)
  end

  @doc """
  Gets a string `source` and evaluate the values using the `bindings`.

  ## Examples

      iex> EEx.eval_string("foo <%= bar %>", bar: "baz")
      "foo baz"

  """
  @spec eval_string(String.t(), keyword, keyword) :: any
  def eval_string(source, bindings \\ [], options \\ [])
      when is_binary(source) and is_list(bindings) and is_list(options) do
    compiled = compile_string(source, options)
    do_eval(compiled, bindings, options)
  end

  @doc """
  Gets a `filename` and evaluate the values using the `bindings`.

  ## Examples

      # sample.eex
      foo <%= bar %>

      # iex
      EEx.eval_file("sample.eex", bar: "baz")
      #=> "foo baz"

  """
  @spec eval_file(String.t(), keyword, keyword) :: any
  def eval_file(filename, bindings \\ [], options \\ [])
      when is_binary(filename) and is_list(bindings) and is_list(options) do
    options = Keyword.put(options, :file, filename)
    compiled = compile_file(filename, options)
    do_eval(compiled, bindings, options)
  end

  ### Helpers

  defp do_eval(compiled, bindings, options) do
    {result, _} = Code.eval_quoted(compiled, bindings, options)
    result
  end
end
