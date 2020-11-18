defmodule EEx.SyntaxError do
  defexception [:message, :file, :line, :column]

  @impl true
  def message(exception) do
    "#{exception.file}:#{exception.line}:#{exception.column}: #{exception.message}"
  end
end

defmodule EEx do
  @moduledoc ~S"""
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way.

      iex> EEx.eval_string("foo <%= bar %>", bar: "baz")
      "foo baz"

  ## API

  This module provides three main APIs for you to use:

    1. Evaluate a string (`eval_string/3`) or a file (`eval_file/3`)
       directly. This is the simplest API to use but also the
       slowest, since the code is evaluated at runtime and not precompiled.

    2. Define a function from a string (`function_from_string/5`)
       or a file (`function_from_file/5`). This allows you to embed
       the template as a function inside a module which will then
       be compiled. This is the preferred API if you have access
       to the template at compilation time.

    3. Compile a string (`compile_string/2`) or a file (`compile_file/2`)
       into Elixir syntax tree. This is the API used by both functions
       above and is available to you if you want to provide your own
       ways of handling the compiled template.

  ## Options

  All functions in this module accept EEx-related options.
  They are:

    * `:file` - the file to be used in the template. Defaults to the given
      file the template is read from or to `"nofile"` when compiling from a string.
    * `:line` - the line to be used as the template start. Defaults to `1`.
    * `:indentation` - (since v1.11.0) an integer added to the column after every
      new line. Defaults to `0`.
    * `:engine` - the EEx engine to be used for compilation.
    * `:trim` - if `true`, trims whitespace left and right of quotation tags up until
      newlines. At least one newline is retained. Defaults to `false`.

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
  `if` clauses, they are treated the same in EEx and
  also require `=` in order to have their result printed:

      <%= if true do %>
        It is obviously true
      <% else %>
        This will never appear
      <% end %>

  To escape an EEx expression in EEx use `<%% content %>`. For example:

      <%%= x + 3 %>

  will be rendered as `<%= x + 3 %>`.

  Note that different engines may have different rules
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
  Generates a function definition from the given string.

  The first argument is the kind of the generated function (`:def` or `:defp`).
  The `name` argument is the name that the generated function will have.
  `template` is the string containing the EEx template. `args` is a list of arguments
  that the generated function will accept. They will be available inside the EEx
  template. `options` is a list of EEx compilation options (see the module documentation).

  ## Examples

      iex> defmodule Sample do
      ...>   require EEx
      ...>   EEx.function_from_string(:def, :sample, "<%= a + b %>", [:a, :b])
      ...> end
      iex> Sample.sample(1, 2)
      "3"

  """
  defmacro function_from_string(kind, name, template, args \\ [], options \\ []) do
    quote bind_quoted: binding() do
      info = Keyword.merge([file: __ENV__.file, line: __ENV__.line], options)
      args = Enum.map(args, fn arg -> {arg, [line: info[:line]], nil} end)
      compiled = EEx.compile_string(template, info)

      case kind do
        :def -> def unquote(name)(unquote_splicing(args)), do: unquote(compiled)
        :defp -> defp unquote(name)(unquote_splicing(args)), do: unquote(compiled)
      end
    end
  end

  @doc """
  Generates a function definition from the file contents.

  The first argument is the kind of the generated function (`:def` or `:defp`).
  The `name` argument is the name that the generated function will have.
  `file` is the path to the EEx template file. `args` is a list of arguments
  that the generated function will accept. They will be available inside the EEx
  template. `options` is a list of EEx compilation options (see the module documentation).

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
      info = Keyword.merge([file: IO.chardata_to_string(file), line: 1], options)
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
  Gets a string `source` and generates a quoted expression
  that can be evaluated by Elixir or compiled to a function.

  This is useful if you want to compile a EEx template into code and inject
  that code somewhere or evaluate it at runtime.

  The generated quoted code will use variables defined in the template that
  will be taken from the context where the code is evaluated. If you
  have a template such as `<%= a + b %>`, then the returned quoted code
  will use the `a` and `b` variables in the context where it's evaluated. See
  examples below.

  ## Examples

      iex> quoted = EEx.compile_string("<%= a + b %>")
      iex> {result, _bindings} = Code.eval_quoted(quoted, a: 1, b: 2)
      iex> result
      "3"

  """
  @spec compile_string(String.t(), keyword) :: Macro.t()
  def compile_string(source, options \\ []) when is_binary(source) and is_list(options) do
    EEx.Compiler.compile(source, options)
  end

  @doc """
  Gets a `filename` and generates a quoted expression
  that can be evaluated by Elixir or compiled to a function.

  This is useful if you want to compile a EEx template into code and inject
  that code somewhere or evaluate it at runtime.

  The generated quoted code will use variables defined in the template that
  will be taken from the context where the code is evaluated. If you
  have a template such as `<%= a + b %>`, then the returned quoted code
  will use the `a` and `b` variables in the context where it's evaluated. See
  examples below.

  ## Examples

      # sample.eex
      <%= a + b %>

      # In code:
      quoted = EEx.compile_file("sample.eex")
      {result, _bindings} = Code.eval_quoted(quoted, a: 1, b: 2)
      result
      #=> "3"

  """
  @spec compile_file(Path.t(), keyword) :: Macro.t()
  def compile_file(filename, options \\ []) when is_list(options) do
    filename = IO.chardata_to_string(filename)
    options = Keyword.merge([file: filename, line: 1], options)
    compile_string(File.read!(filename), options)
  end

  @doc """
  Gets a string `source` and evaluate the values using the `bindings`.

  ## Examples

      iex> EEx.eval_string("foo <%= bar %>", bar: "baz")
      "foo baz"

  """
  @spec eval_string(String.t(), keyword, keyword) :: String.t()
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

      # IEx
      EEx.eval_file("sample.eex", bar: "baz")
      #=> "foo baz"

  """
  @spec eval_file(Path.t(), keyword, keyword) :: String.t()
  def eval_file(filename, bindings \\ [], options \\ [])
      when is_list(bindings) and is_list(options) do
    filename = IO.chardata_to_string(filename)
    options = Keyword.put_new(options, :file, filename)
    compiled = compile_file(filename, options)
    do_eval(compiled, bindings, options)
  end

  ### Helpers

  defp do_eval(compiled, bindings, options) do
    {result, _} = Code.eval_quoted(compiled, bindings, options)
    result
  end
end
