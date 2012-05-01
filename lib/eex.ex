defexception EEx.SyntaxError, message: nil

defmodule EEx do
  @moduledoc """
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way.
  """

  @doc """
  Get a string `source` and generate the correspondents
  quotes to be evaluated by Elixir.
  """
  def compile_string(source, options // []) do
    EEx.Compiler.compile(source, options)
  end

  @doc """
  Get a `filename` and generate the correspondents quotes to
  be evaluated by Elixir.
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