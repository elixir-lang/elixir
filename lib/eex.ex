defexception EEx.SyntaxError, message: nil

defmodule EEx do
  @moduledoc %B"""
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way:

      EEx.eval_string "foo <%= bar %>", [bar: "baz"]
      #=> "foo baz"

  ## API

  This module provides 3 main APIs for you to use:

  1) Evaluate a string (`eval_string`) or a file (`eval_file`)
     directly. This is the simplest API to use but also the
     slowest, since the code is evaluated and not compiled before;

  2) Define a function from a string (`function_from_string`)
     or a file (`function_from_file`). This allows you to embed
     the template as a function inside a module which will then
     be compiled. This is the preferred API;

  3) Compile a string (`compile_string`) or a file (`compile_file`)
     into Elixir syntax tree. This is the API used by both functions
     above and is available to you if you want to provide your own
     ways of handling the compiled template.

  ## Engine

  EEx has the concept of engines which allows you to modify or
  transform the code extracted from the given string or file.

  By default, `EEx` uses the `EEx.SmartEngine` that provides some
  conveniences on top of the simple `EEx.Engine`.

  ### Tags

  `EEx.SmartEngine` supports the following tags:

      <% Elixir expression - inline with output %>
      <& Elxir matching expression - not printed &>
      <%= Elixir expression - replace with result %>

  All expressions that output something to the template
  *must* use the equals sign (`=`). Since everything in
  Elixir is a macro, there are no exceptions for this rule.
  For example, while some template languages would special-
  case `if` clauses, they are treated the same in EEx and
  also requires `=` in order to have their result printed:

      <%= if true do %>
        It is obviously true
      <% else %>
        This will never appear
      <% end %>

  The `<& ... &>` expression is only used in matching clauses.
  For example, the `cond` macro would be written as:

      <%= cond do %>
        <& false -> &> Never printed
        <& true  -> &> always printed
      <% end %>

  Notice that different engines may have different rules
  for each tag. Other tags may be added in future versions.

  ### Macros

  `EEx.SmartEngine` also adds two macros to your template.
  The first one is the `for` macro, which allows you to easily
  loop a variable:

      EEx.eval_string "<%= for x in [1,2,3] do %><%= x %>\n<% end %>", []
      #=> "1\n2\n3\n"

  It also adds defines a macro named `@` that allows easy access:

      EEx.eval_string "<%= @foo %>", assigns: [foo: 1]
      #=> 1

  In other words, <%= @foo %> is simply translated to:

      <%= Keyword.get assigns, :foo %>

  The assigns extension is useful when the number of variables
  required by the template is not specified at compilation time.
  """

  @doc """
  Generates a function definition from the string.
  The kind (`:def` or `:defp`) must be given, the
  function name, its arguments and the compilation options.

  ## Examples

      defmodule Sample do
        require EEx
        EEx.function_from_string :def, :sample, "<%= a + b %>", [:a, :b]
      end

      Sample.sample(1, 2) #=> "3"

  """
  defmacro function_from_string(kind, name, source, args // [], options // []) do
    quote do
      EEx.function_from_quoted(__MODULE__, unquote(kind), unquote(name),
        unquote(args), EEx.compile_string(unquote(source), unquote(options)),
        line: __LINE__, file: __FILE__)
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
        EEx.function_from_file :def, :sample, "sample.eex", [:a, :b]
      end

      # iex
      Sample.sample(1, 2) #=> "3"

  """
  defmacro function_from_file(kind, name, filename, args // [], options // []) do
    quote do
      EEx.function_from_quoted(__MODULE__, unquote(kind), unquote(name),
        unquote(args), EEx.compile_file(unquote(filename), unquote(options)),
        line: __LINE__, file: __FILE__)
    end
  end

  @doc """
  Get a string `source` and generate a quoted expression
  that can be evaluated by Elixir or compiled to a function.
  """
  def compile_string(source, options // []) do
    EEx.Compiler.compile(source, options)
  end

  @doc """
  Get a `filename` and generate a quoted expression
  that can be evaluated by Elixir or compiled to a function.
  """
  def compile_file(filename, options // []) do
    options = Keyword.put options, :file, filename
    compile_string(File.read!(filename), options)
  end

  @doc """
  Get a string `source` and evaluate the values using the `bindings`.

  ## Examples

      EEx.eval_string "foo <%= bar %>", [bar: "baz"]
      #=> "foo baz"

  """
  def eval_string(source, bindings // [], options // []) do
    compiled = compile_string(source, options)
    do_eval(compiled, bindings, options)
  end

  @doc """
  Get a `filename` and evaluate the values using the `bindings`.

  ## Examples

      # sample.ex
      foo <%= bar %>

      # iex
      EEx.eval_file "sample.ex", [bar: "baz"]
      #=> "foo baz"

  """
  def eval_file(filename, bindings // [], options // []) do
    options  = Keyword.put options, :file, filename
    compiled = compile_file(filename, options)
    do_eval(compiled, bindings, options)
  end

  ### Helpers

  @doc false
  def function_from_quoted(module, kind, name, args, source, info) do
    args  = Enum.map args, fn arg -> { arg, 0, nil } end
    quote = quote do
      unquote(kind).(unquote(name).(unquote_splicing(args)), do: unquote(source))
    end
    Module.eval_quoted module, quote, [], info
  end

  defp do_eval(compiled, bindings, options) do
    { result, _ } = Code.eval_quoted(compiled, bindings, options)
    result
  end
end
