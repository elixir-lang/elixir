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
  def compile_string(source, engine // EEx.Engine, filename // 'nofile', line // 1) do
    EEx.Compiler.compile(source, engine, filename, line)
  end

  @doc """
  Get a `filename` and generate the correspondents quotes to
  be evaluated by Elixir.
  """
  def compile_file(filename, engine // EEx.Engine) do
    compile_string(File.read!(filename), engine, filename)
  end

  @doc """
  Get a string `source` and evaluate the values using the `bindings`
  """
  def eval_string(source, bindings // [], engine // EEx.Engine, filename // 'nofile', line // 1) do
    compiled = compile_string(source, engine, filename, line)
    do_eval(compiled, bindings, filename, line)
  end

  @doc """
  Get a `filename` and evaluate the values using the `bindings`
  """
  def eval_file(filename, bindings // [], engine // EEx.Engine) do
    compiled = compile_file(filename, engine)
    do_eval(compiled, bindings, filename)
  end

  ### Helpers

  defp do_eval(compiled, bindings, filename, line // 1) do
    { result, _ } = Code.eval_quoted(compiled, bindings, filename, line)
    result
  end
end

