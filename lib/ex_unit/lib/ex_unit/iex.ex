defmodule ExUnit.Iex do
  @moduledoc """
    Iterate on tests from a shell

      $ MIX_ENV=test iex -S mix
      iex> TestIex.start()
      iex> TestIex.test("test/test.exs", 100)
      # Change code
      iex> recompile()
      iex> TestIex.test("test/test.exs", 100)
  """

  def start() do
    ExUnit.start()
    Code.eval_file("test/test_helper.exs", File.cwd!())
    :ok
  end

  def test(path, line \\ nil) do
    if line, do: ExUnit.configure(exclude: [:test], include: [line: line]), else: ExUnit.configure([])

    Code.load_file(path)
    ExUnit.Server.modules_loaded()

    ExUnit.run()
  end
end
