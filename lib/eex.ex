defexception EEx::SyntaxError, message: nil

defmodule EEx do
  @moduledoc """
  EEx stands for Embedded Elixir. It allows you to embed
  Elixir code inside a string in a robust way.
  """

  @doc """
  Get a string source and generate the correspondents
  quotes to be evaluated by Elixir.
  """
  def compile_string(source, engine // EEx::Engine) do
    EEx::Compiler.compile(source, engine)
  end

  @doc """
  Get a file and generate the correspondents quotes to
  be evaluated by Elixir.
  """
  def compile_file(filename, engine // EEx::Engine) do
    EEx.compile_string(File.read!(filename), engine)
  end
end

