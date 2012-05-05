defexception EEx.SyntaxError, message: nil

defmodule EEx do
  @moduledoc """
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way.
  """

  @doc """
  Generates a function definition from the string.
  The kind (`:def` or `:defp`) must be given, the
  function name, its arguments and the common
  compilation options.

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
  function name, its arguments and the common
  compilation options.

  This function is useful in case you have templates but
  you want to precompile inside a module for speed.

  ## Examples

      defmodule Sample do
        require EEx
        EEx.function_from_file :def, :sample, "sample.eex", [:a, :b]
      end

      Sample.sample(1, 2) #=> "3"

  """
  defmacro function_from_file(kind, name, filename, args // [], options // []) do
    quote do
      EEx.function_from_quoted(__MODULE__, unquote(kind), unquote(name),
        unquote(args), EEx.compile_file(unquote(filename), unquote(options)),
        line: __LINE__, file: __FILE__)
    end
  end

  @doc false
  # Function used internally by function_from_file and function_from_string
  def function_from_quoted(module, kind, name, args, source, info) do
    args  = Enum.map args, fn(arg) -> { arg, 0, nil } end
    quote = quote do
      unquote(kind).(unquote(name).(unquote_splicing(args)), do: unquote(source))
    end
    Module.eval_quoted module, quote, [], info
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
  Get a string `source` and evaluate the values using the `bindings`
  """
  def eval_string(source, bindings // [], options // []) do
    compiled = compile_string(source, options)
    do_eval(compiled, bindings, options)
  end

  @doc """
  Get a `filename` and evaluate the values using the `bindings`
  """
  def eval_file(filename, bindings // [], options // []) do
    options  = Keyword.put options, :file, filename
    compiled = compile_file(filename, options)
    do_eval(compiled, bindings, options)
  end

  ### Helpers

  defp do_eval(compiled, bindings, options) do
    { result, _ } = Code.eval_quoted(compiled, bindings, options)
    result
  end
end
